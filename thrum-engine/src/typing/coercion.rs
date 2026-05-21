use crate::{
    ErrType, lexing::tokens::Span,
    parsing::ast::ExprId,
    typing::{Type, TypeChecker, TypeId, TypeVarId, }
};


#[derive(Default, Clone, Copy)]
pub enum AutoDerefMode {
    #[default]
    None, Once, Fully, LeaveOnePointer
}

impl TypeChecker<'_> {
    /// this function allows auto-deref to autoclone types
    /// e.g. expr.typ: &&bool, expected: bool -> works, inserts 2 moves
    /// e.g. expr.typ: &&Vec, expected: &Vec -> works, inserts 1 move
    pub(super) fn auto_deref_to_expected_type(&mut self, expr: ExprId, expected_type: TypeId) -> TypeId {
        let expr_type = self.typed_ast.get_expr_type(expr);
        
        if let Some((expr_p_count, is_auto_clone_after)) = self.count_initial_pointers(expr_type)
        && let Some((expected_p_count, _)) = self.count_initial_pointers(expected_type) {

            // if we have more pointers than the expected_type, AND everything that needs to be derefed is auto_clone
            if expr_p_count > expected_p_count && (expected_p_count != 0 || is_auto_clone_after) {
                for _ in 0..(expr_p_count - expected_p_count) {
                    assert!(self.deref_if_pointer(expr));
                }
            }
        }

        self.typed_ast.get_expr_type(expr)
    }

    fn count_initial_pointers(&self, typ: TypeId) -> Option<(usize, bool)> {
        let mut curr_typ = typ;
        let mut count = 0;

        while let Type::Borrow { inner, .. } = self.prune_type_once(curr_typ) {
            curr_typ = inner;
            count += 1;
        }

        self.is_auto_clone(curr_typ)
            .map(|final_auto_clone| (count, final_auto_clone))
    }



    pub(super) fn handle_deref_mode(&mut self, deref_mode: AutoDerefMode, check_expr: ExprId) -> TypeId {
        match deref_mode {
            AutoDerefMode::None => {/* do nothing */}
            AutoDerefMode::Once => {
                self.deref_if_pointer(check_expr);
            }
            AutoDerefMode::Fully => {
                while self.deref_if_pointer(check_expr) { }
            }
            AutoDerefMode::LeaveOnePointer => {
                while let Some((2.., _)) = self.count_initial_pointers(self.typed_ast.get_expr_type(check_expr)) {
                    assert!(self.deref_if_pointer(check_expr));
                }
            }
        }
        self.typed_ast.get_expr_type(check_expr)
    }


    pub(super) fn deref_if_pointer(&mut self, expr: ExprId) -> bool {
        let span = self.ast.get_expr_span(expr);
        let typ = self.typed_ast.get_expr_type(expr);

        match self.prune_type_once_infer_err(typ, span) {
            Type::Borrow { inner, mutable: _, borrows_var } => {

                *self.typed_ast.auto_derefs.entry(expr).or_default() += 1;
                self.typed_ast.expr_types[expr.0 as usize] = inner;

                let auto_clone = self.check_deref_memory_rules(inner, borrows_var, span);
                if !auto_clone {
                    self.typed_ast.move_expr.insert(expr);
                }
                // println!("derefed: {:?}", self.ast.get_expr(expr));
                true
            }
            _ => false
        }
    }

    pub(super) fn check_deref_memory_rules(&mut self, inner: TypeId, borrows_var: Option<TypeVarId>, span: Span) -> bool {
        if let Some(auto_clone) = self.is_auto_clone(inner) {
            if auto_clone {
                if let Some(x) = borrows_var {
                    self.clone_variable(x, span);
                }
            } else {
                // if it isn't autoclone, it needs to move it
                if let Some(x) = borrows_var {
                    self.move_variable(x, span);
                }
                else {
                    self.error(ErrType::TyperCantDerefUnknownPointerType, span); 
                }
            }
            auto_clone
        } else {
            self.error(ErrType::TyperTypeMustBeKnownHere { typ: self.fmt_type(inner) }, span);
            true  // just return true here for less chained errors
        }
    }

    /// Some(true) - is autoclone
    /// Some(false) - isn't autoclone
    /// None - infer error
    fn is_auto_clone(&self, typ: TypeId) -> Option<bool> {
        match self.prune_type_once(typ) {
            Type::Num
            | Type::Bool
            | Type::Borrow { .. }
            | Type::Fn { .. }
            | Type::MetaType
            | Type::Void
            | Type::Never
            | Type::Error => Some(true),

            Type::Str
            | Type::TupArr(_, _)
            | Type::Tup(_)
            | Type::Enum(_) => Some(false),

            Type::CustomType(_, inner) | Type::FlowType(inner, _) | Type::RefinedEnum(inner, _) => {
                self.is_auto_clone(inner)
            }

            Type::Infer(_) => None
        }
    }
}