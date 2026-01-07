use crate::{
    ErrType, Program, ProgramError,
    nativelib::{ThrumModule, get_native_lib},
    parsing::{ast_structure::{Expr, ExprInfo, Span, TypeKind}, desugar},
    typing::{check_expressions::ExprContext, type_environment::{TypecheckScope, TypecheckVar}}
};
use std::{cell::RefCell, collections::HashMap};

mod check_expressions;
mod check_pattern_types;
pub mod type_environment;
mod inference;



// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x -> Inference(0) - (0 -> arr<inference<1>>)
// y -> Inference(2) - (2 -> arr<inference<1>>)
// z -> Inference(3) - (3 -> str) and figures the type of inference<1> out. now all types are fully known.



pub fn typecheck_program(program: &mut Program) {
    let mut type_checker = Typechecker::new(&mut program.errors, get_native_lib());
    
    // PASS 1: run type unification/constraint solving logic.
    type_checker.check_expression(program.ast.as_mut().unwrap(), &ExprContext::default());

    // PASS 2: clean up (remove all TypeKind::Infered(id))
    type_checker.finalize_expression(program.ast.as_mut().unwrap());
    program.type_lookup = type_checker.type_lookup;
}




#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct TypeID (pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarID (pub usize);


pub struct Typechecker<'a> {
    errors: &'a mut Vec<ProgramError>,
    library: ThrumModule,
    scopes: Vec<TypecheckScope>,

    type_lookup: HashMap<TypeID, TypeKind>,
    next_inference_id: usize,

    var_lookup: HashMap<VarID, TypecheckVar>,
    next_var_id: usize,

    current_function_return_type: Option<TypeKind>,
    current_break_types: Vec<(String, TypeKind)>,
}

impl<'a> Typechecker<'a> {
    pub fn new(errors: &'a mut Vec<ProgramError>, lib: ThrumModule) -> Self {
        let mut tc = Typechecker {
            errors,
            library: ThrumModule::default(),
            scopes: vec![TypecheckScope::default()],

            type_lookup: HashMap::default(),
            // starts at 1, meaning it can never be 0
            // maybe the compiler can optimize stuff with that, idk
            next_inference_id: 1,

            var_lookup: HashMap::default(),
            next_var_id: 1,

            current_function_return_type: None,
            current_break_types: Vec::new(),
        };
        tc.load_prelude_from_lib(&lib);
        tc.library = lib;
        tc
    }

    fn error(&mut self, err_type: ErrType, span: Span) -> TypeKind {
        self.errors.push(ProgramError {
            line: span.line,
            byte_offset: span.byte_offset,
            length: span.length,
            typ: err_type
        });
        TypeKind::TypeError
    }

    fn type_mismatch(&mut self, expected: TypeKind, found: TypeKind, span: Span) -> TypeKind {
        self.error(ErrType::TyperMismatch(expected, found), span)
    }






    fn finalize_expression(&mut self, expr: &mut ExprInfo) {
        // wrap self in a RefCell to allow multiple "borrows" that are checked at runtime.
        let self_cell = RefCell::new(self);

        desugar::loop_over_every_ast_node(
            expr,
            |expr| {
                match expr.expression {
                    Expr::Ensure { condition, alt, then } => {
                        ExprInfo {
                            expression: Expr::If { condition, then, alt },
                            typ: expr.typ,
                            span: expr.span
                        }
                    }
                    _ => expr
                }
            },
            |pattern| pattern,

            |typ| {
                let mut self_borrow = self_cell.borrow_mut();
                let pruned = self_borrow.prune(&typ);
                let final_typ = match pruned {
                    TypeKind::ParserUnknown => {
                        unreachable!("somehow the typechecker missed a ParserUnknown type...")
                    }
                    TypeKind::Inference(id) => {
                        self_borrow.type_lookup.insert(id, TypeKind::TypeError);
                        self_borrow.error(ErrType::TyperCantInferType(pruned), Span::invalid())
                    }
                    _ => pruned,
                };
                final_typ
            }
        );
    }
}