use crate::{parsing::ast_structure::{Span, TypeKind}, typing::{Typechecker, TypeID}};





impl Typechecker<'_> {
    // its so cursed that this function can be const
    // but cool i guess :)
    pub const fn new_inference_type(&mut self) -> TypeKind {
        let id = self.next_inference_id;
        self.next_inference_id += 1;
        TypeKind::Inference(TypeID(id))
    }

    pub fn prune(&mut self, typ: &TypeKind, error_on_inference: Option<Span>) -> TypeKind {
        let pruned = typ.prune(&self.type_lookup);
        if let TypeKind::Inference(_) = pruned && let Some(err_span) = error_on_inference {
            self.error(crate::ErrType::TyperTypeMustBeKnownHere { typ: pruned }, err_span)
        } else {
            pruned
        }
    }


    pub fn unify_types(&mut self, a: &TypeKind, b: &TypeKind, span: Span) {
        let type_a = self.prune(a, None);
        let type_b = self.prune(b, None);
        
        match (type_a.clone(), type_b.clone()) {
            _ if type_a == type_b => { /* Do nothing */ }

            // if one is an inference variable, bind it to the other type.
            (TypeKind::Inference(id), _) => { self.type_lookup.insert(id, type_b); }
            (_, TypeKind::Inference(id)) => { self.type_lookup.insert(id, type_a); }
            
            (TypeKind::Never | TypeKind::TypeError, _)
            | (_, TypeKind::Never | TypeKind::TypeError) => { /* Do nothing */ }

            (TypeKind::Pointer { mutable: mut_a, inner: inner_a, borrows_var: _ },
            TypeKind::Pointer { mutable: mut_b, inner: inner_b, borrows_var: _ }) => {
                self.unify_types(&inner_a, &inner_b, span);
                // if mut_a != mut_b {
                //     self.type_mismatch(type_a, type_b, span);
                // }
            }
            (TypeKind::Arr(inner_a), TypeKind::Arr(inner_b)) => {
                self.unify_types(&inner_a, &inner_b, span);
            }
            (TypeKind::Tup(elements_a), TypeKind::Tup(elements_b)) => {
                if elements_a.len() == elements_b.len() {
                    for (ia, ib) in elements_a.iter().zip(elements_b.iter()) {
                        // types have to match
                        self.unify_types(&ia.typ, &ib.typ, span);
                        // labels can't mismatch (if both labels are non number labels)
                        if ia.label != ib.label && [ia, ib].iter().all(|x| x.label.chars().any(|c| !c.is_ascii_digit())) {
                            self.type_mismatch(type_a, type_b, span);
                            break;
                        } 
                    }
                }
                else { self.type_mismatch(type_a, type_b, span); }
            }
            (TypeKind::Fn { param_types: params_a, return_type: return_a },
            TypeKind::Fn { param_types: params_b, return_type: return_b }) => {
                if params_a.len() == params_b.len() {
                    for (ia, ib) in params_a.iter().zip(params_b.iter()) {
                        self.unify_types(ia, ib, span);
                    }
                }
                else { self.type_mismatch(type_a, type_b, span); }
                self.unify_types(&return_a, &return_b, span);
            }

            _ => { self.type_mismatch(type_a, type_b, span); }
        }
    }

    pub fn unify_type_vec(&mut self, vec: &[TypeKind], span: Span) -> TypeKind {
        if let Some((first, others)) = vec.split_first() {
            let mut is_never = false;
            for other in others {
                self.unify_types(first, other, span);
                if self.prune(other, None).is_never() {
                    is_never = true;
                }
            }
            if is_never { TypeKind::Never } else { first.clone() }
        }
        else { self.new_inference_type() }
    }
}