use std::collections::HashMap;

use crate::{nativelib::ThrumType, parsing::ast_structure::TypeKind};










#[derive(Default)]
pub struct TypecheckScope {
    pub vars: HashMap<String, TypecheckValue>,
    types: HashMap<String, ThrumType>,
}

pub struct TypecheckValue {
    typ: TypeKind,
    pub mut_borrowed_by: Option<String>,
}
#[derive(Default)]
pub struct TypecheckEnvironment {
    pub scopes: Vec<TypecheckScope>,
}
impl TypecheckEnvironment {
    pub fn new() -> Self { TypecheckEnvironment { scopes: vec![TypecheckScope::default()] } }

    // e.g. for a block or function
    pub fn enter_scope(&mut self) { self.scopes.push(TypecheckScope::default()); }
    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn define_variable(&mut self, name: String, typ: TypeKind) -> bool {
        let already_exists = self.name_exists_already(&name);
        self.scopes.last_mut().unwrap().vars.insert(name, TypecheckValue { typ, mut_borrowed_by: None });
        already_exists
    }
    pub fn lookup_variable(&self, name: &str) -> Option<TypeKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.vars.get(name) {
                return Some(t.typ.clone());
            }
        }
        None
    }

    pub fn define_type(&mut self, name: String, typ: ThrumType) -> bool {
        let already_exists = self.name_exists_already(&name);
        self.scopes.last_mut().unwrap().types.insert(name, typ);
        already_exists
    }
    pub fn lookup_type(&mut self, name: &str) -> Option<ThrumType> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    pub fn name_exists_already(&mut self, name: &str) -> bool {
        self.lookup_variable(name).is_some() || self.lookup_type(name).is_some()
    }
}