use std::collections::HashMap;

use crate::{ErrType, nativelib::{ThrumModule, ThrumType}, parsing::ast_structure::{Span, TypeKind}, typing::{BreakTypeInfo, Typechecker, VarID}};




#[derive(Default)]
pub struct TypecheckScope {
    pub vars: HashMap<String, VarID>,
    pub types: HashMap<String, ThrumType>,
}
#[derive(Clone, Debug)]
pub struct TypecheckVar {
    pub var_id: VarID,
    pub name: String,
    pub typ: TypeKind,
    
    // Source code location - for error messages
    pub declared_at: Span,
    pub is_declared_mut: bool,
    pub is_initialized: InitState,
    pub is_used: bool,
    pub is_used_mut: bool,
    pub borrows_count: usize,
    pub mut_borrows_count: usize,
    pub is_moved: bool,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum InitState {
    No, Yes, Maybe
}


pub type SnapshotInitVars = HashMap<VarID, InitState>;

impl Typechecker<'_> {
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
            self.error(ErrType::TyperNameAlreadyDefined { name: name.clone() }, span);
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
            is_initialized: if is_init { InitState::Yes } else { InitState::No },
            is_used: false,
            is_used_mut: false,
            borrows_count: 0,
            mut_borrows_count: 0,
            is_moved: false,
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

    pub(super) fn make_variable_ref(&mut self, name: &str, mutable: bool, span: Span, var_id_to_fill: &mut Option<VarID>) -> TypeKind {
        if let Some(var) = self.lookup_variable(name, var_id_to_fill) {
            let mut errors = Vec::new();

            if mutable {
                if var.borrows_count > 0 { errors.push(ErrType::TyperCantBorrowMutBecauseAlreadyBorrowed); }
                if var.mut_borrows_count > 0 { errors.push(ErrType::TyperCantBorrowMutBecauseAlreadyBorrowedMut); }
                var.mut_borrows_count += 1;
            } else {
                if var.mut_borrows_count > 0 { errors.push(ErrType::TyperCantBorrowBecauseAlreadyBorrowedMut); }
                var.borrows_count += 1;
            }
            let var_typ = TypeKind::Pointer {
                mutable,
                inner: Box::new(var.typ.clone()),
                borrows_var: *var_id_to_fill
            };            
            for e in errors { self.error(e, span); }
            var_typ
        } else {
            self.error(ErrType::TyperUndefinedIdentifier { name: name.to_string() }, span)
        }
    }


    pub(super) fn update_variable(&mut self, var_id: Option<VarID>, span: Span) {
        let var = self.var_lookup.get_mut(&var_id.unwrap()).unwrap();
        let mut errors = Vec::new();
        
        var.is_used = true;
        var.is_used_mut = true;
        var.is_moved = false;  // if variable was moved, doing `a = ...` unmoves it again.

        match var.is_initialized {
            InitState::No => var.is_initialized = InitState::Yes,
            InitState::Maybe => errors.push(ErrType::TyperCantUseMaybeInitializedVar { var: var.var_id }),
            InitState::Yes => if !var.is_declared_mut { errors.push(ErrType::TyperVarIsntDeclaredMut { var: var.var_id }); }
        }
        
        for e in errors { self.error(e, span); }
    }

    pub(super) fn move_variable(&mut self, var_id: &Option<VarID>, span: Span) {
        let var = self.var_lookup.get_mut(&var_id.unwrap()).unwrap();
        let mut errors = Vec::new();

        if var.is_moved {
            errors.push(ErrType::TyperCantUseMovedVar { var: var.var_id });
        }
        var.is_moved = true;

        for e in errors { self.error(e, span); }
    }



    pub(super) fn define_type(&mut self, name: String, typ: ThrumType, span: Span) {
        if self.name_exists_already(&name) {
            self.error(ErrType::TyperNameAlreadyDefined { name: name.clone() }, span);
        }
        self.scopes.last_mut().unwrap().types.insert(name, typ);
    }
    pub(super) fn lookup_type(&mut self, name: &str) -> Option<&mut ThrumType> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(t) = scope.types.get_mut(name) {
                return Some(t);
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
    fn snapshot_vars_init_state(&self, only_uninitialized: bool) -> SnapshotInitVars {
        // iterate over all currently in scope variables
        let mut vars_state = HashMap::new();
        for scope in &self.scopes {
            for var_id in scope.vars.values() {
                let var = self.var_lookup.get(var_id).unwrap();
                if !only_uninitialized || var.is_initialized == InitState::No {
                    vars_state.insert(*var_id, var.is_initialized);
                }
            }
        }
        vars_state
    }

    pub(super) fn snapshot_first_vars_init_state(&self) -> SnapshotInitVars {
        self.snapshot_vars_init_state(true)
    }
    pub(super) fn snapshot_branch_vars_init_state(&self, branch_was_never: bool) -> Option<SnapshotInitVars> {
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
                    InitState::Yes => {
                        any_branch_init = true; 
                    }
                    InitState::Maybe => {
                        any_branch_init = true;
                        all_branches_init = false; // "Maybe" kills definite initialization
                    }
                    InitState::No => {
                        all_branches_init = false; // "No" kills definite initialization
                    }
                }                
            }

            let final_state = if all_branches_init {
                // Every single branch initialized this var -> var is initialized.
                InitState::Yes
            } else if any_branch_init {
                // Some branches did, some didn't (or were Maybe) -> uncertain...
                InitState::Maybe
            } else {
                // No branch touched it -> uninitialized
                InitState::No
            };
            self.var_lookup.get_mut(&var_id).unwrap().is_initialized = final_state;
        }
    }
    
    pub(super) fn snap_label_before(&mut self, label: &mut Option<String>) -> Option<SnapshotInitVars> {
        if let Some(label) = label {
            let block_break_type = self.new_inference_type();
            self.current_break_types.push(BreakTypeInfo {
                label: label.clone(),
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