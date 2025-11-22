use crate::{parsing::ast_structure::TypeKind, typing::TypeChecker};





impl TypeChecker {
    pub fn new_inference_type(&mut self) -> TypeKind {
        self.next_inference_id += 1;
        TypeKind::Inference(self.next_inference_id)
    }

    pub fn prune(&mut self, typ: &TypeKind) -> TypeKind {
        if let TypeKind::Inference(id) = typ {
            if let Some(entry) = self.inference_id_lookup.get(&id) {
                let pruned = self.prune(&entry.clone());
                return pruned;
            }
        }
        typ.clone()
    }


    pub fn unify_types(&mut self, a: &TypeKind, b: &TypeKind) {
        let type_a = self.prune(a);
        let type_b = self.prune(b);
        
        match (&type_a, &type_b) {
            _ if type_a == type_b => { /* Do nothing */ }

            // if one is an inference variable, bind it to the other type.
            (TypeKind::Inference(id), _) => { self.inference_id_lookup.insert(*id, type_b.clone()); }
            (_, TypeKind::Inference(id)) => { self.inference_id_lookup.insert(*id, type_a.clone()); }
            
            (TypeKind::Never, _) => { /* Do nothing */ }
            (_, TypeKind::Never) => { /* Do nothing */ }

            (TypeKind::MutPointer(inner_a), TypeKind::MutPointer(inner_b))
            | (TypeKind::Arr(inner_a), TypeKind::Arr(inner_b)) => {
                self.unify_types(inner_a, inner_b);
            }
            (TypeKind::Tup(inners_a), TypeKind::Tup(inners_b)) => {
                if inners_a.len() == inners_b.len() {
                    for (ia, ib) in inners_a.iter().zip(inners_b.iter()) {
                        self.unify_types(ia, ib);
                    }
                }
                else { self.type_mismatch(&type_a, &type_b); }
            }
            (TypeKind::Fn { param_types: params_a, return_type: return_a },
            TypeKind::Fn { param_types: params_b, return_type: return_b }) => {
                if params_a.len() == params_b.len() {
                    for (ia, ib) in params_a.iter().zip(params_b.iter()) {
                        self.unify_types(ia, ib);
                    }
                }
                else { self.type_mismatch(&type_a, &type_b); }
                self.unify_types(&return_a, &return_b);
            }

            _ => { self.type_mismatch(&type_a, &type_b); }
        }
    }

    pub fn unify_type_vec(&mut self, vec: &[TypeKind]) -> TypeKind {
        if let Some((first, others)) = vec.split_first() {
            let mut is_never = false;
            for other in others {
                self.unify_types(first, other);
                if self.prune(other) == TypeKind::Never {
                    is_never = true;
                }
            }
            if is_never { TypeKind::Never } else { first.clone() }
        }
        else { self.new_inference_type() }
    }
}