use std::{collections::{HashMap, HashSet}, fmt::{self, Write}};
use derive_more::Display;

use crate::{
    ErrType, ProgramError, ProgramErrorData, lexing::tokens::Span, nativelib::get_native_lib,
    parsing::ast::{AstArena, AstIds, ExprId, PatternId},
    typing::{check_expressions::CheckExprCtx, type_vars::{SnapshotVarsState, TypeVar, TypeVarScope}}, vm_compiling::{FunctionRegistry, RuntimeValue}
};

pub mod type_vars;
mod check_expressions;
mod check_patterns;
mod exhaustiveness;


#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Type {
    Num,
    Str,
    Bool,

    /// 0-bit type
    Void,
    /// something that never happens at runtime
    /// e.g. the type of (return, break, continue)-exprs
    Never,

    /// `const X = bool`, X has type `MetaType`
    MetaType,

    Enum(EnumId, Option<EnumSpecializationId>),
    CustomType(CustomTypeId, TypeId),

    Tup(Vec<TypeTuple>),
    TupArr(TypeId, usize),

    Fn { param_types: Vec<TypeId>, return_type: TypeId },

    Borrow { inner: TypeId, mutable: bool, borrows_var: Option<TypeVarId> },

    Infer(TypeInferId),
    Error,
}

#[derive(Debug, Display, Clone, Eq, Hash, PartialEq)]
#[display("{label}: {typ:?}")]
pub struct TypeTuple {
    pub label: String,
    pub typ: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub enum EnumSpecialization {
    /// "currently" its this variant. could change though
    Specialized(usize),
    /// always this exact variant. `e.g. fn handle_some(x: Option.Some)`
    HardSpecialized(usize),
    /// can be multiple
    Multiple,
}


#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd)]
pub struct TypeId(pub AstIds);
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct EnumId(pub AstIds);
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd)]
pub struct EnumSpecializationId(pub AstIds);
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, PartialOrd)]
pub struct CustomTypeId(pub AstIds);
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct TypeInferId(pub AstIds);
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct TypeVarId(pub AstIds);

impl TypeId {
    pub const ERROR: Self = Self(0);
    pub const NEVER: Self = Self(1);
    pub const VOID:  Self = Self(2);
    pub const TYPE:  Self = Self(3);
    pub const NUM:   Self = Self(4);
    pub const BOOL:  Self = Self(5);
    pub const STR:   Self = Self(6);

    pub const MAP_CONSTS: [(Self, Type); 7] = [
        (Self::ERROR, Type::Error), (Self::NEVER, Type::Never), (Self::VOID, Type::Void), (Self::TYPE, Type::MetaType),
        (Self::NUM, Type::Num), (Self::BOOL, Type::Bool), (Self::STR, Type::Str)
    ];
}



pub struct TypeChecker<'a> {
    error_data: &'a mut ProgramErrorData,
    ast: &'a AstArena,  // needs to be mut for auto-deref
    typed_ast: TypedAst,
    
    // maps variable names to their Id
    var_scopes: Vec<TypeVarScope<'a>>,

    type_arena: TypeArena,

    // if inference_types[0] is Some(TypeId(3))
    // => InferType 0 was resolved to TypeId 3
    inference_types: Vec<Option<TypeId>>,  // indexed with TypeInferId

    enum_specialization: Vec<EnumSpecialization>,

    // implemented stuff on types
    // e.g. `impl Number { ... }`
    custom_type_impls: Vec<TypeVarScope<'a>>,  // indexed with CustomTypeId
    
    // for return
    curr_function_return_type: Option<TypeId>,
    // for break/continue
    curr_label_infos: Vec<LabelInfo<'a>>,
    // for impl so they can use Self and self
    curr_impl_self: Option<TypeId>,
    
    // meta compiling stuff, if a function gets compiled during the typechecking phase,
    // it gets kept and doesn't need to be compiled again in the VmCompiler stage
    compiled_functions: FunctionRegistry,
}

#[derive(Debug, Default)]
/// this struct gets build in the typechecker phase, and is read-only afterwards.
/// 
/// it has a bunch of notes telling the `VmCompiler` everything it needs to compile.
/// it needs to know to compile the ast-nodes (which are read-only after parsing already)
pub struct TypedAst {
    pub expr_types: Vec<TypeId>,  // Indexed with ExprId

    pub enum_defs: Vec<EnumDefinition>,  // indexed with EnumId

    pub vars: Vec<TypeVar>,  // indexed with TypeVarId
    pub resolved_expr_var: HashMap<ExprId, TypeVarId>,
    pub resolved_pattern_var: HashMap<PatternId, TypeVarId>,

    pub resolved_impl_self_type: HashMap<ExprId, TypeId>,
    pub resolved_type_instantian: HashMap<ExprId, ResolvedTypeInstantiation>,
    pub resolved_type_destruction_not_a_tuple: HashSet<PatternId>,
    
    pub resolved_tuple_arr_length: HashMap<ExprId, usize>,
    pub resolved_enum_variant: HashMap<ExprId, (EnumId, usize)>,
    pub resolved_enum_variant_pattern: HashMap<PatternId, (EnumId, usize)>,
    pub resolved_closure_fn_id: HashMap<ExprId, usize>,
    pub resolved_member_access: HashMap<ExprId, ResolvedMemberAccess>,
    pub resolved_labels: HashMap<ExprId, ExprId>,  // maps a labeled-expr to where it needs to jump to
    pub auto_derefs: HashMap<ExprId, usize>,  // amount of autoderefs
    pub move_expr: HashSet<ExprId>,
}
impl TypedAst {
    #[must_use]
    pub fn new(ast_exprs: usize) -> Self {
        Self { expr_types: vec![TypeId::ERROR; ast_exprs], ..Default::default() }
    }
    #[must_use] pub fn get_expr_type(&self, id: ExprId) -> TypeId { self.expr_types[id.0 as usize] }

    #[must_use] pub fn get_var(&self, id: TypeVarId) -> &TypeVar { &self.vars[id.0 as usize] }
    #[must_use] pub fn get_var_mut(&mut self, id: TypeVarId) -> &mut TypeVar { &mut self.vars[id.0 as usize] }
}

#[derive(Debug)]
pub struct TypeArena {
    // indexed with TypeId
    pub types: Vec<Type>,

    // duplicate types should get the same id
    // because of inference types there can still be dupes
    // but those get filtered out in the zonking phase.
    type_dedup: HashMap<Type, TypeId>,
}
impl TypeArena {
    #[must_use]
    fn new() -> Self {
        let mut ta = Self { types: Vec::new(), type_dedup: HashMap::new() };
        // add the hardcoded types
        for (id, typ) in TypeId::MAP_CONSTS {
            assert_eq!(id, ta.add_type(typ));
        }
        ta
    }

    #[must_use]
    pub fn get_type(&self, id: TypeId) -> Type { self.types[id.0 as usize].clone() }

    #[must_use]
    pub fn add_type(&mut self, typ: Type) -> TypeId {
        if let Some(&id) = self.type_dedup.get(&typ) {
            id
        } else {
            let id = TypeId(AstIds::try_from(self.types.len()).unwrap());
            self.types.push(typ.clone());
            self.type_dedup.insert(typ, id);
            id
        }
    }
}

#[derive(Debug)]
pub struct EnumDefinition {
    /// maps variant names to their `attached_tuple`
    /// e.g. "Some" -> `Type::Tup(...)`
    /// e.g. "None" -> `Type::Void`
    variants: Vec<(Box<str>, TypeId)>
}

pub struct LabelInfo<'ast> {
    label: &'ast str,
    expr: ExprId,
    typ: TypeId,
    break_snapshots: Vec<Option<SnapshotVarsState>>,
}

#[derive(Debug)]
pub enum ResolvedMemberAccess {
    TupleRefIndex { index: usize },  // tup.0
    TupleIndex { index: usize },  // tup^.0
    Member { constant: RuntimeValue },  // Point.distance(p)
    MemberWithSelfSugar { constant: RuntimeValue, self_sugar_expr: ExprId },  // p.distance()
    EnumWithNoData { i: usize }  // Option.None
}

#[derive(Debug)]
pub enum ResolvedTypeInstantiation {
    NewType,
    Tuple,
    EnumVariant(usize),
}

impl TypeChecker<'_> {
    pub fn start(error_data: &mut ProgramErrorData, ast: &AstArena) -> (TypedAst, FunctionRegistry) {
        let mut tc = TypeChecker {
            error_data, ast,
            typed_ast: TypedAst::new(ast.exprs.len()),
            var_scopes: vec![TypeVarScope::default()],
            type_arena: TypeArena::new(),
            inference_types: Vec::new(),
            enum_specialization: Vec::new(),
            custom_type_impls: Vec::new(),
            curr_function_return_type: None,
            curr_label_infos: Vec::new(),
            curr_impl_self: None,
            compiled_functions: FunctionRegistry::new(),
        };
        let native_lib = get_native_lib(&mut tc.type_arena);
        tc.load_prelude_from_lib(&native_lib);
        
        // check the main expression
        tc.check_expression(ExprId(0), &mut false, &CheckExprCtx::default());

        tc.finalize_types();

        (tc.typed_ast, tc.compiled_functions)
    }

    #[track_caller]
    fn error(&mut self, err_type: ErrType, span: Span) -> TypeId {
        self.error_data.errors.push(ProgramError {
            span,
            err_type,
            compiler_location: std::panic::Location::caller()
        });
        TypeId::ERROR
    }

    #[track_caller]
    fn type_mismatch(&mut self, expected: TypeId, found: TypeId, span: Span) -> TypeId {
        self.error(ErrType::TyperMismatch {
            expected: self.fmt_type(expected),
            found: self.fmt_type(found)
        }, span)
    }

    pub fn new_infer_type(&mut self) -> TypeId {
        let id = TypeInferId(AstIds::try_from(self.inference_types.len()).unwrap());
        self.inference_types.push(None);  // unresolved initially
        self.type_arena.add_type(Type::Infer(id))
    }
    
    #[must_use]
    pub fn prune_id_once(&self, id: TypeId) -> TypeId {
        let mut current_id = id;
        while let Type::Infer(infer_id) = self.type_arena.types[current_id.0 as usize] {
            if let Some(resolved_id) = self.inference_types[infer_id.0 as usize] {
                current_id = resolved_id;
            } else { break }
        }
        current_id
    }
    #[must_use]
    pub fn prune_type_once(&self, id: TypeId) -> Type {
        let id = self.prune_id_once(id);
        self.type_arena.get_type(id)
    }
    #[must_use]
    pub fn prune_type_once_infer_err(&mut self, id: TypeId, err_span: Span) -> Type {
        let typ = self.prune_type_once(id);
        if let Type::Infer(_) = typ {
            self.error(ErrType::TyperTypeMustBeKnownHere { typ: self.fmt_type(id) }, err_span);
            Type::Error
        } else {
            typ
        }
    }

    #[track_caller]
    pub fn unify_types(&mut self, expected: TypeId, other: TypeId, span: Span) {
        let id_a = self.prune_id_once(expected);
        let id_b = self.prune_id_once(other);

        // already equal -> do nothing
        if id_a == id_b { return }

        let a = self.type_arena.types[id_a.0 as usize].clone();
        let b = self.type_arena.types[id_b.0 as usize].clone();
        let mut mismatch = false;

        assert!(a != b, "only equal after?");

        match (a, b) {            
            // if one is an inference variable, bind it to the other type.
            (Type::Infer(id), _) => self.inference_types[id.0 as usize] = Some(id_b),
            (_, Type::Infer(id)) => self.inference_types[id.0 as usize] = Some(id_a),

            // if one is Never or Error, do nothing
            (Type::Never | Type::Error, _)
            | (_, Type::Never | Type::Error) => { /* Do nothing */ }

            (Type::TupArr(inner_a, length_a), Type::TupArr(inner_b, length_b)) => {
                self.unify_types(inner_a, inner_b, span);
                if length_a != length_b {
                    mismatch = true;
                }
            }

            (Type::Tup(elems_a), Type::Tup(elems_b)) => {
                if elems_a.len() == elems_b.len() {
                    for (ia, ib) in elems_a.into_iter().zip(elems_b) {
                        // types have to match
                        self.unify_types(ia.typ, ib.typ, span);
                        // labels can't mismatch (if both labels are non number labels)
                        if ia.label != ib.label && ia.label.as_bytes()[0].is_ascii_digit() && ib.label.as_bytes()[0].is_ascii_digit() {
                            mismatch = true;
                            break;
                        } 
                    }
                } else {
                    mismatch = true;
                }
            }

            (Type::CustomType(id_a, inner_a), Type::CustomType(id_b, inner_b)) => {
                if id_a == id_b {
                    self.unify_types(inner_a, inner_b, span);
                } else {
                    // Different custom_id => they can't unify
                    // e.g. `type N1 = num;  type N2 = num;  N1{ 2 } + N2{ 2 }`
                    mismatch = true;
                }
            }

            (Type::Enum(a_id, a_spec), Type::Enum(b_id, b_spec)) => {
                #[allow(clippy::if_not_else, reason = "clippy wants to obscure the error path :(")]
                if a_id != b_id {
                    mismatch = true;
                } else {
                    // if both are actually an instance of an enum, and not just the enum type
                    // (otherwise just do nothing, its fine)
                    if let (Some(spec_id_a), Some(spec_id_b)) = (a_spec, b_spec) {
                        let enum_spec_a = self.enum_specialization[spec_id_a.0 as usize];
                        let enum_spec_b = self.enum_specialization[spec_id_b.0 as usize];

                        let merged_result = match (enum_spec_a, enum_spec_b) {
                            // --- SOFT SPECIALIZATIONS ---
                            // two soft specializations of the same variant stay specialized!
                            // otherwise it turns into `Multiple`
                            // e.g. `if cond { Option.Some } else { Option.None }` -> Becomes Multiple
                            (EnumSpecialization::Specialized(x), EnumSpecialization::Specialized(y)) => {
                                if x == y { Some(EnumSpecialization::Specialized(x)) } 
                                else { Some(EnumSpecialization::Multiple) }
                            }
                            
                            // --- HARD SPECIALIZATIONS ---
                            // Hard specializations have to meet the same variant, otherwise WoOOPs error.
                            (EnumSpecialization::HardSpecialized(x), EnumSpecialization::HardSpecialized(y)) => {
                                (x == y).then_some(EnumSpecialization::HardSpecialized(x))
                            }
    
                            (EnumSpecialization::HardSpecialized(hard), EnumSpecialization::Specialized(soft))
                            | (EnumSpecialization::Specialized(soft), EnumSpecialization::HardSpecialized(hard)) => {
                                // upgrading soft => hard is fine, IF they are the same variant.
                                (soft == hard).then_some(EnumSpecialization::HardSpecialized(hard))
                            }
    
                            // Hard <=> Multiple is an error
                            (EnumSpecialization::Multiple, EnumSpecialization::HardSpecialized(_)) |
                            (EnumSpecialization::HardSpecialized(_), EnumSpecialization::Multiple) => None,

                            // Multiple with anything else just stays Multiple
                            (EnumSpecialization::Multiple, _) | (_, EnumSpecialization::Multiple) => {
                                Some(EnumSpecialization::Multiple)
                            }
                        };

                        if let Some(new_spec) = merged_result {
                            // update the spec on both types!
                            self.enum_specialization[spec_id_a.0 as usize] = new_spec;
                            self.enum_specialization[spec_id_b.0 as usize] = new_spec;
                        } else {
                            mismatch = true;
                        }
                    }
                }
            }

            // a tuple with only types IS a type itself
            (Type::MetaType, Type::Tup(elems)) => {
                for elem in elems {
                    self.unify_types(TypeId::TYPE, elem.typ, span);
                }
            }

            (Type::Borrow { mutable: mut_a, inner: inner_a, borrows_var: _ },
            Type::Borrow { mutable: mut_b, inner: inner_b, borrows_var: _ }) => {
                self.unify_types(inner_a, inner_b, span);
                if mut_a != mut_b {
                    mismatch = true;
                }
            }

            (Type::Fn { param_types: params_a, return_type: return_a },
            Type::Fn { param_types: params_b, return_type: return_b }) => {
                if params_a.len() == params_b.len() {
                    for (ia, ib) in params_a.into_iter().zip(params_b) {
                        self.unify_types(ia, ib, span);
                    }
                } else {
                    mismatch = true;
                }
                self.unify_types(return_a, return_b, span);
            }

            // any other case is a mismatch
            _ => mismatch = true
        }
        
        if mismatch {
            self.type_mismatch(expected, other, span);
        }
    }

    fn unify_type_vec(&mut self, types: &[TypeId], span: Span) -> TypeId {
        if let Some((&first, others)) = types.split_first() {
            for &other in others {
                self.unify_types(first, other, span);
            }
            first
        } else {
            self.new_infer_type()
        }
    }

    pub(super) fn are_types_equivalent_ignore_spec(&self, left: TypeId, right: TypeId) -> bool {
        if left == right { return true }

        match (self.prune_type_once(left), self.prune_type_once(right)) {
            (Type::Enum(id1, _), Type::Enum(id2, _)) => id1 == id2,
            (Type::CustomType(left_id, left_inner), Type::CustomType(right_id, right_inner)) => {
                left_id == right_id && self.are_types_equivalent_ignore_spec(left_inner, right_inner)
            }
            _ => false
        }
    }


    pub(super) fn fmt_type(&self, typ: TypeId) -> String {
        let mut s = String::new();
        self.write_type(typ, &mut s).unwrap();
        s
    }

    fn write_type(&self, id: TypeId, s: &mut String) -> fmt::Result {
        match self.type_arena.get_type(self.prune_id_once(id)) {
            Type::Num => write!(s, "num"),
            Type::Str => write!(s, "str"),
            Type::Bool => write!(s, "bool"),
            Type::Void => write!(s, "void"),
            Type::Never => write!(s, "never"),
            Type::MetaType => write!(s, "type"),
            Type::Error => write!(s, "error"),
            Type::Infer(infer) => write!(s, "?{}", infer.0),

            Type::Enum(enum_id, spec_note) => {
                write!(s, "enum<{enum_id:?}")?;
                
                if let Some(note) = spec_note {
                    match self.enum_specialization[note.0 as usize] {
                        EnumSpecialization::Multiple => write!(s, ", Multiple")?,
                        EnumSpecialization::Specialized(i) => write!(s, ", Soft({i})")?,
                        EnumSpecialization::HardSpecialized(i) => write!(s, ", Hard({i})")?,
                    }
                }
                write!(s, ">")
            }

            Type::CustomType(id, inner) => {
                write!(s, "customtype<{id:?}, ")?;
                self.write_type(inner, s)?;
                write!(s, ">")
            }

            Type::Tup(elems) => {
                write!(s, "tup<")?;
                for (i, elem) in elems.into_iter().enumerate() {
                    if i > 0 { write!(s, ", ")?; }
                    
                    // Only print the label if it isn't an auto-generated number label
                    if !elem.label.as_bytes()[0].is_ascii_digit() {
                        write!(s, "{}: ", elem.label)?;
                    }
                    self.write_type(elem.typ, s)?;
                }
                write!(s, ">")
            }

            Type::TupArr(inner, length) => {
                write!(s, "tupArr<")?;
                self.write_type(inner, s)?;
                write!(s, "; {length}>")
            }

            Type::Fn { param_types, return_type } => {
                write!(s, "|")?;
                for (i, param) in param_types.into_iter().enumerate() {
                    if i > 0 { write!(s, ", ")?; }
                    self.write_type(param, s)?;
                }
                write!(s, " -> ")?;
                self.write_type(return_type, s)
            }

            Type::Borrow { inner, mutable, borrows_var } => {
                if mutable {
                    write!(s, "mut ")?;
                }
                write!(s, "ref ")?;
                self.write_type(inner, s)?;
                if let Some(var) = borrows_var {
                    write!(s, " ({})", self.typed_ast.get_var(var).name)?;
                }
                Ok(())
            }
        }
    }


    /// The final smoshing phase, also called zonking
    /// here its getting rid of all `Infer()` types. If it can't, it will throw a `TyperCantInferType` error
    /// (num. Infer(0)) -> (num, num)
    pub(super) fn finalize_types(&mut self) {
        // cache to prevent recursively zonking the same type thousands of times.
        let mut cache = vec![None; self.type_arena.types.len()];

        // zonk Expression types
        for i in 0..self.typed_ast.expr_types.len() {
            let span = self.ast.expr_spans[i];
            self.typed_ast.expr_types[i] = self.zonk_type(self.typed_ast.expr_types[i], span, &mut cache);
        }
        // zonk Variable types
        for i in 0..self.typed_ast.vars.len() {
            let span = self.typed_ast.vars[i].declared_at;
            self.typed_ast.vars[i].typ = self.zonk_type(self.typed_ast.vars[i].typ, span, &mut cache);
        }
        // zonk enum variant types
        for i in 0..self.typed_ast.enum_defs.len() {
            for j in 0..self.typed_ast.enum_defs[i].variants.len() {
                self.typed_ast.enum_defs[i].variants[j].1 = self.zonk_type(self.typed_ast.enum_defs[i].variants[j].1, Span::invalid(), &mut cache);
            }
        }
    }

    fn zonk_type(&mut self, id: TypeId, span: Span, cache: &mut Vec<Option<TypeId>>) -> TypeId {
        if let Some(zonked) = cache[id.0 as usize] {
            return zonked;
        }

        let typ = self.type_arena.types[id.0 as usize].clone();

        let zonked_id = match typ {
            // try to resolve this Type::Infer!
            Type::Infer(infer_id) => {
                if let Some(resolved_id) = self.inference_types[infer_id.0 as usize] {
                    // resolved! keep recursively zonking.
                    self.zonk_type(resolved_id, span, cache)
                } else {
                    self.error(ErrType::TyperCantInferType { typ: self.fmt_type(id) }, span)
                }
            }

            // OTHER CASES: recursively zonk the inner types and then re-deduplicate them!
            Type::TupArr(inner, length) => {
                let new_inner = self.zonk_type(inner, span, cache);
                self.type_arena.add_type(Type::TupArr(new_inner, length))
            }
            Type::Tup(elems) => {
                let new_elems = elems.into_iter().map(|e| TypeTuple {
                    label: e.label,
                    typ: self.zonk_type(e.typ, span, cache),
                }).collect();
                self.type_arena.add_type(Type::Tup(new_elems))
            }
            Type::Fn { param_types, return_type } => {
                let new_params = param_types.into_iter().map(|p| self.zonk_type(p, span, cache)).collect();
                let new_ret = self.zonk_type(return_type, span, cache);
                self.type_arena.add_type(Type::Fn { param_types: new_params, return_type: new_ret })
            }
            Type::Borrow { inner, mutable, borrows_var } => {
                let new_inner = self.zonk_type(inner, span, cache);
                self.type_arena.add_type(Type::Borrow { inner: new_inner, mutable, borrows_var })
            }
            Type::CustomType(custom_id, inner) => {
                let new_inner = self.zonk_type(inner, span, cache);
                self.type_arena.add_type(Type::CustomType(custom_id, new_inner))
            }

            // simple types don't need zonking
            Type::Num | Type::Str | Type::Bool | Type::Void | Type::Never
            | Type::MetaType | Type::Error | Type::Enum(_, _) => id
        };

        // resize the cache dynamically, because `self.type_arena.add_type()` might have added stuff
        if cache.len() < self.type_arena.types.len() {
            cache.resize(self.type_arena.types.len(), None);
        }
        cache[id.0 as usize] = Some(zonked_id);
        zonked_id
    }
}