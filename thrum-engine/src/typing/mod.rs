use crate::{
    ErrType, Program, ProgramError, nativelib::{ThrumModule, ThrumType, get_native_lib}, parsing::{ast_structure::{Expr, MatchPattern, TupleType, TypeKind, ExprInfo}, desugar}, typing::{check_expressions::ExprContext, type_environment::TypecheckEnvironment}
};
use std::{cell::RefCell, collections::HashMap, fmt};


mod check_expressions;
mod check_pattern_types;
mod type_environment;
mod inference;


// example:
// let x = []
// let y = x
// let z: str = x[1]
//
// x -> Inference(0) - (0 -> arr<inference<1>>)
// y -> Inference(2) - (2 -> arr<inference<1>>)
// z -> Inference(3) - (3 -> str) and figures the type of inference<1> out. now all types are fully known.



pub struct TypeCheckError {
    pub message: String
}
impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}


pub fn typecheck_program(program: &mut Program) {
    let mut type_checker = TypeChecker::new(&mut program.errors);
    
    // PASS 1: run type unification/constraint solving logic.
    type_checker.check_block(&mut program.ast, &ExprContext::default());

    // PASS 2: clean up (remove all TypeKind::Infered(id))
    type_checker.finalize_expressions(&mut program.ast);
    program.type_lookup = type_checker.type_lookup;
}




struct AssignablePatternType {
    typ: TypeKind,
    has_place: bool,
    can_fail: bool,
    vars: Vec<(String, TypeKind)>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct TypeID (usize);


pub struct TypeChecker<'a> {
    errors: &'a mut Vec<ProgramError>,
    env: TypecheckEnvironment,
    library: ThrumModule,
    type_lookup: HashMap<TypeID, TypeKind>,
    next_inference_id: usize,

    current_function_return_type: Option<TypeKind>,
    current_break_types: Vec<(String, TypeKind)>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(errors: &'a mut Vec<ProgramError>) -> Self {
        let library = get_native_lib();
        let mut env = TypecheckEnvironment::new();
        env.load_prelude_from_lib(&library);

        TypeChecker {
            errors,
            env,
            library,

            type_lookup: HashMap::default(),
            next_inference_id: 0,
            current_function_return_type: None,
            current_break_types: Vec::new(),
        }
    }

    fn error(&mut self, err_type: ErrType) -> TypeKind {
        self.errors.push(ProgramError {
            line: 0,
            byte_offset: 0,
            typ: err_type
        });
        TypeKind::TypeError
    }

    fn type_mismatch(&mut self, expected: TypeKind, found: TypeKind) -> TypeKind {
        self.error(ErrType::TyperMismatch(expected, found))
    }


    fn checked_define_variable(&mut self, name: String, typ: TypeKind) {
        if self.env.define_variable(name.clone(), typ) {
            self.error(ErrType::TyperNameAlreadyDefined(name));
        }
    }
    fn checked_define_type(&mut self, name: String, typ: ThrumType) {
        if self.env.define_type(name.clone(), typ) {
            self.error(ErrType::TyperNameAlreadyDefined(name));
        }
    }






    fn finalize_expressions(&mut self, expressions: &mut [ExprInfo]) {
        // wrap self in a RefCell to allow multiple "borrows" that are checked at runtime.
        let self_cell = RefCell::new(self);

        desugar::loop_over_every_ast_node(
            expressions,

            |mut expr| {
                let mut self_borrow = self_cell.borrow_mut();
                self_borrow.finalize_type(&mut expr.typ);
                match &mut expr.expression {
                    Expr::FnDefinition { return_type, .. }
                    | Expr::Closure { return_type, .. } => {
                        self_borrow.finalize_type(return_type);
                    }

                    // Do nothing to other nodes
                    _ => { }
                }
                expr
            },

            |mut pattern| {
                match &mut pattern {
                    // finalize binding pattern types
                    MatchPattern::Binding { typ, .. } => {
                        self_cell.borrow_mut().finalize_type(typ);
                    }
                    // Do nothing to other patterns
                    _ => {}
                }
                pattern
            }
        );
    }

    fn finalize_type(&mut self, typ: &mut TypeKind) -> TypeKind {
        let pruned = self.prune(typ);
        let final_typ = match pruned {
            TypeKind::Inference(id) => {
                self.type_lookup.insert(id, TypeKind::TypeError);
                self.error(ErrType::TyperCantInferType(pruned))
            }
            TypeKind::Arr(mut inner) => TypeKind::Arr(Box::new(self.finalize_type(&mut inner))),
            TypeKind::Tup(elements) => TypeKind::Tup(
                elements.into_iter().map(|TupleType { label, mut typ }| TupleType { label, typ: self.finalize_type(&mut typ) }).collect()
            ),
            TypeKind::Fn { param_types, mut return_type } => TypeKind::Fn {
                param_types: param_types.into_iter().map(|mut t| self.finalize_type(&mut t)).collect(),
                return_type: Box::new(self.finalize_type(&mut return_type)),
            },
            _ => pruned,
        };
        *typ = final_typ;
        typ.clone()
    }
}