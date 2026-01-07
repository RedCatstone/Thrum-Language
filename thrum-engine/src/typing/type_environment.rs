use std::collections::HashMap;

use crate::{ErrType, nativelib::{ThrumModule, ThrumType}, parsing::ast_structure::{Span, TypeKind}, typing::{Typechecker, VarID}};













#[derive(Default)]
pub struct TypecheckScope {
    pub vars: HashMap<String, VarID>,
    pub types: HashMap<String, ThrumType>,
}
#[derive(Clone)]
pub struct TypecheckVar {
    pub var_id: VarID,
    pub name: String,
    pub typ: TypeKind,
    
    // Source code location - for error messages
    pub declared_at: Span,
    pub is_declared_mut: bool,
    pub is_used: bool,
    pub is_used_mut: bool,
}

impl<'a> Typechecker<'a> {
    // e.g. for a block or function
    pub fn enter_scope(&mut self) {
        self.scopes.push(TypecheckScope::default());
    }

    pub fn exit_scope(&mut self) -> Vec<VarID> {
        let dropped_scope = self.scopes.pop().unwrap();
        dropped_scope.vars.into_values().collect()
        
    }

    pub fn define_variable(&mut self, name: String, mutable: bool, typ: TypeKind, span: Span) -> TypecheckVar {
        if self.name_exists_already(&name) {
            self.error(ErrType::TyperNameAlreadyDefined(name.clone()), span);
        }

        // make the new var
        let var_id = VarID(self.next_var_id);
        self.next_var_id += 1;
        let new_var = TypecheckVar {
            var_id,
            typ,
            name: name.clone(),
            declared_at: span,
            is_declared_mut: mutable,
            is_used: false,
            is_used_mut: false,
        };

        // and insert into both maps.
        self.var_lookup.insert(var_id, new_var.clone());
        self.scopes
            .last_mut().unwrap()
            .vars.insert(name, var_id);
        
        new_var
    }
    pub fn lookup_variable(&mut self, name: &str) -> Option<&mut TypecheckVar> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(id) = scope.vars.get_mut(name) {
                let var = self.var_lookup.get_mut(id).unwrap();
                return Some(var);
            }
        }
        None
    }
    pub fn use_variable(&mut self, name: &str, mutable: bool, span: Span, var_id_to_fill: &mut Option<VarID>) -> TypeKind {
        match self.lookup_variable(name) {
            Some(var) => {
                let var_id = var.var_id;
                let var_typ = var.typ.clone();
                *var_id_to_fill = Some(var_id);
                var.is_used = true;
                if mutable {
                    var.is_used_mut = true;
                    if !var.is_declared_mut {
                        self.error(ErrType::TyperVarIsntDeclaredMut(var_id), span);
                    }
                }
                var_typ
            }
            None => {
                self.error(ErrType::TyperUndefinedIdentifier(name.to_string()), span);
                TypeKind::TypeError
            }
        }
    }

    pub fn define_type(&mut self, name: String, typ: ThrumType, span: Span) {
        if self.name_exists_already(&name) {
            self.error(ErrType::TyperNameAlreadyDefined(name.clone()), span);
        }
        self.scopes.last_mut().unwrap().types.insert(name, typ);
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


    pub fn load_prelude_from_lib(&mut self, module: &ThrumModule) {
        for (name, value) in &module.values {
            if value.is_prelude {
                self.define_variable(name.clone(), false, value.typ.clone(), Span::invalid());
            }
        }
        // Recursion
        for sub_module in module.sub_modules.values() {
            self.load_prelude_from_lib(sub_module);
        }
    }
}