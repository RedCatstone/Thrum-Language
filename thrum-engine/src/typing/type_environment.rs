use std::collections::HashMap;

use crate::{ErrType, nativelib::{ThrumModule, ThrumType}, parsing::ast_structure::{Span, TypeKind}, typing::{BreakTypeInfo, Typechecker, VarID}};




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
    pub is_initialized: InitializedState,
    pub is_declared_mut: bool,
    pub is_used: bool,
    pub is_used_mut: bool,
}

#[derive(Clone, Copy, PartialEq)]
pub enum InitializedState {
    No, Yes, Maybe
}


pub type SnapshotInitVars = HashMap<VarID, InitializedState>;

impl<'a> Typechecker<'a> {
    // e.g. for a block or function
    pub(super) fn enter_scope(&mut self) {
        self.scopes.push(TypecheckScope::default());
    }

    pub(super) fn exit_scope(&mut self) -> Vec<VarID> {
        let dropped_scope = self.scopes.pop().unwrap();
        dropped_scope.vars.into_values().collect()
        
    }

    pub(super) fn define_variable(&mut self, name: String, mutable: bool, is_init: bool, typ: TypeKind, span: Span) -> TypecheckVar {
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
            is_initialized: if is_init { InitializedState::Yes } else { InitializedState::No },
            is_declared_mut: mutable,
            is_used: false,
            is_used_mut: false,
        };

        // and insert into both maps.
        self.var_lookup.insert(var_id, new_var.clone());
        self.scopes
            .last_mut().expect("there should always be a scope here")
            .vars.insert(name, var_id);
        
        new_var
    }
    pub(super) fn lookup_variable(&mut self, name: &str, var_id_to_fill: &mut Option<VarID>) -> Option<&mut TypecheckVar> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(id) = scope.vars.get_mut(name) {
                *var_id_to_fill = Some(*id);
                let var = self.var_lookup.get_mut(id).unwrap();
                return Some(var);
            }
        }
        None
    }
    pub(super) fn update_variable(&mut self, name: &str, span: Span, var_id_to_fill: &mut Option<VarID>) -> TypeKind {
        match self.lookup_variable(name, var_id_to_fill) {
            Some(var) => {
                let var_was_initialized = var.is_initialized;
                var.is_used = true;
                if let InitializedState::No = var.is_initialized {
                    var.is_initialized = InitializedState::Yes;
                }

                let var = var.clone();
                
                if let InitializedState::Maybe = var_was_initialized {
                    self.error(ErrType::TyperCantUseMaybeInitializedVar(var.var_id), span);
                }
                if let InitializedState::Yes = var_was_initialized && !var.is_declared_mut {
                    self.error(ErrType::TyperVarIsntDeclaredMut(var.var_id), span);
                }
                var.typ
            }
            None => {
                self.error(ErrType::TyperUndefinedIdentifier(name.to_string()), span);
                TypeKind::TypeError
            }
        }
    }

    pub(super) fn use_variable(&mut self, name: &str, mutable: bool, span: Span, var_id_to_fill: &mut Option<VarID>) -> TypeKind {
        match self.lookup_variable(name, var_id_to_fill) {
            Some(var) => {
                var.is_used = true;
                if mutable { var.is_used_mut = true; }

                let var = var.clone();

                match var.is_initialized {
                    InitializedState::No => { self.error(ErrType::TyperCantUseUninitializedVar(var.var_id), span); },
                    InitializedState::Maybe => { self.error(ErrType::TyperCantUseMaybeInitializedVar(var.var_id), span); },
                    InitializedState::Yes => { /* All good */ }
                }
                if mutable && !var.is_declared_mut {
                    self.error(ErrType::TyperVarIsntDeclaredMut(var.var_id), span);
                }
                var.typ
            }
            None => {
                self.error(ErrType::TyperUndefinedIdentifier(name.to_string()), span);
                TypeKind::TypeError
            }
        }
    }

    pub(super) fn define_type(&mut self, name: String, typ: ThrumType, span: Span) {
        if self.name_exists_already(&name) {
            self.error(ErrType::TyperNameAlreadyDefined(name.clone()), span);
        }
        self.scopes.last_mut().unwrap().types.insert(name, typ);
    }
    pub(super) fn lookup_type(&mut self, name: &str) -> Option<ThrumType> {
        for scope in self.scopes.iter().rev() {
            if let Some(t) = scope.types.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    pub(super) fn name_exists_already(&mut self, name: &str) -> bool {
        self.lookup_variable(name, &mut None).is_some() || self.lookup_type(name).is_some()
    }




    // logic for "definite assignment analysis", for example:
    // let x
    // if false { x = 5 }
    // println!("{x}")  // should Error, because x was not initialized in all branches of the if statement
    fn snapshot_vars_init_state(&mut self, only_uninitialized: bool) -> SnapshotInitVars {
        // iterate over all currently in scope variables
        let mut vars_state = HashMap::new();
        for scope in &self.scopes {
            for var_id in scope.vars.values() {
                let var = self.var_lookup.get(var_id).unwrap();
                if !only_uninitialized || var.is_initialized == InitializedState::No {
                    vars_state.insert(*var_id, var.is_initialized);
                }
            }
        }
        vars_state
    }

    pub(super) fn snapshot_first_vars_init_state(&mut self) -> SnapshotInitVars {
        self.snapshot_vars_init_state(true)
    }
    pub(super) fn snapshot_branch_vars_init_state(&mut self, branch_was_never: bool) -> Option<SnapshotInitVars> {
        // if the branch has type Never it should not be included in the later merge
        if branch_was_never {
            None
        } else {
            Some(self.snapshot_vars_init_state(false))
        }
    }

    pub(super) fn restore_vars_init_state(&mut self, snap: &SnapshotInitVars) {
        for (var_id, is_init) in snap {
            self.var_lookup.get_mut(var_id).unwrap().is_initialized = *is_init;
        }
    }

    pub(super) fn merge_vars_init_states(&mut self, original_snap: SnapshotInitVars, branch_snaps: &[Option<SnapshotInitVars>]) {
        // filter the None's out (the branches that had Never type)
        let actual_branch_snaps = branch_snaps.iter().filter_map(|x| x.as_ref()).collect::<Vec<_>>();

        for (var_id, _old_is_init) in original_snap {
            let mut any_branch_init = false;
            let mut all_branches_init = true;

            for branch_snap in &actual_branch_snaps {
                let branch_state = branch_snap.get(&var_id).unwrap();
                match branch_state {
                    InitializedState::Yes => {
                        any_branch_init = true; 
                    }
                    InitializedState::Maybe => {
                        any_branch_init = true;
                        all_branches_init = false; // "Maybe" kills definite initialization
                    }
                    InitializedState::No => {
                        all_branches_init = false; // "No" kills definite initialization
                    }
                }                
            }

            let final_state = if all_branches_init {
                // Every single branch initialized this var -> var is initialized.
                InitializedState::Yes
            } else if any_branch_init {
                // Some branches did, some didn't (or were Maybe) -> uncertain...
                InitializedState::Maybe
            } else {
                // No branch touched it -> uninitialized
                InitializedState::No
            };
            self.var_lookup.get_mut(&var_id).unwrap().is_initialized = final_state;
        }
    }
    
    pub(super) fn snap_label_before(&mut self, label: &mut Option<String>) -> Option<SnapshotInitVars> {
        if let Some(label) = label {
            let block_break_type = self.new_inference_type();
            self.current_break_types.push(BreakTypeInfo {
                label: label.to_string(),
                typ: block_break_type,
                snapshots_from_breaks: Vec::new(),
            });
            Some(self.snapshot_first_vars_init_state())
        } else {
            None
        }
    }



    pub(super) fn load_prelude_from_lib(&mut self, module: &ThrumModule) {
        for (name, value) in &module.values {
            if value.is_prelude {
                self.define_variable(name.clone(), false, true, value.typ.clone(), Span::invalid());
            }
        }
        // Recursion
        for sub_module in module.sub_modules.values() {
            self.load_prelude_from_lib(sub_module);
        }
    }
}