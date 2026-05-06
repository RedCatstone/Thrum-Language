use crate::typing::{EnumDefinition, EnumId};


/// `PatternSpace` is the exhaustive-checker of this compiler.\
/// The math in here did fry my brain when i was writing this, but im happy that it actually works.
/// 
/// If we are still missing all cases, that is represented as missing the set: `PatternSpace::All` (the Universe)\
/// Now lets say we want to check for exhaustiveness on a simple bool, we will only cover the true case.\
/// It adds a covered case: `Bool(true)`. Done.\
/// now to convert the `covered_cases` to `missing_cases` it computes `All - covered_cases`, in this case that is just `Bool(false)`.
/// 
/// A wildcard pattern covers `All`.
/// Converting that to missing would be: `All - All` which is {/} an empty set, meaning that there are no `missing_cases` left.
/// 
/// To cover an Or-pattern/match-expr, it simply puts all `covered_cases` into one set.
/// So its `All - (pat1 ∪ pat2 ∪ pat3)` to get the missing cases.
/// 
/// Tuple cases are a bit more complicated, but the logic is written below (where its handled) ;p

#[derive(Debug, Clone)]
pub enum PatternSpace {
    // Num { from: f64, to: f64 },
    Bool(bool),

    // if i cover (false, false) missing will be [true, All], [false, true]
    // if i also coverered (_, true) missing will be [true, false] (from first missingcase, the second one results in no case)
    Tup { inners: Vec<Self> },

    EnumVariant { enum_id: EnumId, variant_index: usize, attached_tuple: Box<Self /*this is always a Tup or All */> },

    // this represents ALL cases
    // if we are missing ALL and a wildcard subtracts ALL from that -> empty (covered)
    All,
}

impl PatternSpace {
    pub(super) fn display_patterns(patterns: &[Self], enum_defs: &[EnumDefinition]) -> String {
        patterns.iter()
            .map(|x| x.display(enum_defs))
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub(super) fn display(&self, enum_defs: &[EnumDefinition]) -> String {
        match self {
            Self::Bool(bool) => format!("{bool}"),
            Self::Tup { inners } => {
                format!("({})", inners.iter().map(|x| x.display(enum_defs)).collect::<Vec<_>>().join(", "))
            }
            Self::EnumVariant { enum_id, variant_index, attached_tuple, .. } => {
                format!(".{}{}",
                    enum_defs[enum_id.0 as usize].variants[*variant_index].0,
                    if let Self::All = **attached_tuple { String::new() } else { attached_tuple.display(enum_defs) }
                )
            }
            Self::All => "_".to_string(),
        }
    }

    /// basically computes `All - covered_cases` to get the missing cases.
    /// Returns the exact minimal set of patterns that are still missing.
    pub(super) fn covered_to_missing_cases(covered_cases: &[Self], enum_defs: &[EnumDefinition]) -> Vec<Self> {
        let mut missing_cases = vec![Self::All];

        // for every case that is still missing, it subtracts the current covered one.
        for covered in covered_cases {
            missing_cases = missing_cases.into_iter()
                .flat_map(|missing| missing.subtract(covered, enum_defs))
                .collect();
        }
        missing_cases
    }


    /// computes `self - now_covered` (set difference).
    /// this is the core logic!!1
    /// 
    /// Returns a list of `PatternSpace`s that are covered by `self`, but NOT by `now_covered`.
    /// It's a list, because subtracting can "scatter" patterns.
    /// e.g. subtracting `(All, All) - (true, true)` results in `(false, All)` and `(true, false)`
    fn subtract(&self, now_covered: &Self, enum_defs: &[EnumDefinition]) -> Vec<Self> {
        let mut new_missing_cases = Vec::new();

        match (self, now_covered) {
            (_, Self::All) => {
                // anything - All = nothing
                // even All - All = nothing
                // so just add nothing here
            }

            (Self::All, Self::Bool(covered_bool)) => {
                // All - false = true
                // All - true = false
                new_missing_cases.push(Self::Bool(!covered_bool));
            }
            (Self::Bool(missing_bool), Self::Bool(covered_bool)) => {
                if missing_bool != covered_bool {
                    // missing the same case again
                    new_missing_cases.push(self.clone());
                }
                // else: fully covered
            }

            (Self::All, Self::Tup { inners: covered_inners }) => {
                let all_tup_vec = Self::Tup { inners: vec![Self::All; covered_inners.len()] };
                new_missing_cases.extend(all_tup_vec.subtract(now_covered, enum_defs));
            }
            (Self::Tup { inners: missing_inners }, Self::Tup { inners: covered_inners }) => {
                // the generalized logic for this with missing_inners: (M1, M2, ..., Mn) and covered_inners (C1, C2, ..., Cn)
                // 1. (M1 - C1, M2, M3, ...)
                // 2. (M1 ∩ C1, M2 - C2, M3, ...)
                // 3. (M1 ∩ C1, M2 ∩ C2, M3 - C3, ...)
                // ...

                // with missing_inners: (All, All, All) and covered_inners: (false, false, false)
                // this would generate: (true, All, All), (false, true, All), (false, false, true)

                let mut curr_vec = missing_inners.clone();

                debug_assert_eq!(missing_inners.len(), covered_inners.len());
                for (i, (mi, ci)) in missing_inners.iter().zip(covered_inners).enumerate() {
                    // Calculate Mi - Ci (this can return multiple things)
                    for subtract in mi.subtract(ci, enum_defs) {
                        let mut new_inners = curr_vec.clone();
                        new_inners[i] = subtract;
                        new_missing_cases.push(Self::Tup { inners: new_inners });
                    }

                    // Calculate Mi ∩ Ci and put it in the curr_vec for the other cases.
                    if let Some(intersect) = mi.intersect(ci) {
                        curr_vec[i] = intersect;
                    } else {
                        // if the intersection didn't match (e.g. false ∩ true) we stop
                        // e.g. with missing_inners: (All, true, All) and covered_inners: (false, false, false)
                        // this would generate: (true, true, All), (false, true, All), nothing here anymore
                        break;
                    }
                }
            }

            (Self::All, Self::EnumVariant { enum_id, .. }) => {
                let enum_def = &enum_defs[enum_id.0 as usize];

                // expand All into every variant of this enum and subtract
                for (i, _) in enum_def.variants.iter().enumerate() {
                    new_missing_cases.extend(
                        Self::EnumVariant { enum_id: *enum_id, variant_index: i, attached_tuple: Box::new(Self::All) }
                            .subtract(now_covered, enum_defs)
                    );
                }
            }
            (Self::EnumVariant { enum_id, variant_index: missing_index, attached_tuple: missing_tuple },
            Self::EnumVariant { enum_id: id2, variant_index: covered_index, attached_tuple: covered_tuple }) => {
                debug_assert_eq!(enum_id, id2);
                if missing_index == covered_index {
                    // same variant => just subtract the tuple datas
                    let tup_result = missing_tuple.subtract(covered_tuple, enum_defs);
                    for t in tup_result {
                        new_missing_cases.push(Self::EnumVariant { enum_id: *enum_id, variant_index: *missing_index, attached_tuple: Box::new(t) });
                    }
                } else {
                    // if they are different variants => missing the same case again
                    new_missing_cases.push(self.clone());
                }
            }

            (a, b) => unreachable!("pattern subtraction is not defined for {a:?} - {b:?}")
        }

        new_missing_cases
    }


    /// Computes `self ∩ other` (set intersection).
    /// Returns `Some` if the patterns actually overlapped, (e.g. `2..5 ∩ 4..10 = 4..5`)\
    /// otherwise `None`. (e.g. `true ∩ false` would return `None`)
    fn intersect(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::All, x) | (x, Self::All) => {
                // All intersect with anything, is that anything
                Some(x.clone())
            }

            (Self::Bool(a), Self::Bool(b)) => {
                if a == b { Some(Self::Bool(*a)) } else { None }
            }

            (Self::Tup { inners: a }, Self::Tup { inners: b }) => {
                a.iter().zip(b)
                    // intersect every pair from a and b with each other.
                    .map(|(a, b)| a.intersect(b))
                    // if any pair-intersection resulted in None, the whole thing results in None.
                    // e.g. `(true, 2..5) ∩ (false, 4..10)` will result in None.
                    .collect::<Option<Vec<_>>>()
                    .map(|inners| Self::Tup { inners })
            }

            (Self::EnumVariant { enum_id, variant_index: a_idx, attached_tuple: a_tup, .. },
            Self::EnumVariant { enum_id: b_id, variant_index: b_idx, attached_tuple: b_tup, .. }) => {
                debug_assert_eq!(enum_id, b_id);
                if a_idx == b_idx {
                    // same variant => just intersect the tuple datas
                    a_tup.intersect(b_tup).map(|intersection_pat| {
                        Self::EnumVariant { enum_id: *enum_id, variant_index: *a_idx, attached_tuple: Box::new(intersection_pat) }
                    })
                } else {
                    // if they are different variants, there is no intersection.
                    None
                }
            }

            _ => unreachable!("mismatched PatternSpace types for intersect {self:?} and {other:?}")
        }
    }


    /// with [a, b], [c, d]
    /// this should generate [a, c], [a, d], [b, c], [b, d]
    pub(super) fn tuple_cartesian_product(missing_cases: &[Vec<Self>]) -> Vec<Self> {
        let mut result = Vec::with_capacity(missing_cases.iter().map(Vec::len).product());
        result.push(Vec::new());  // start with one empty tuple

        for cases in missing_cases {
            let mut new_result = Vec::new();
            for existing in result {
                for case in cases {
                    let mut new_inner = existing.clone();
                    new_inner.push(case.clone());
                    new_result.push(new_inner);
                }
            }
            result = new_result;
        }

        result.into_iter().map(|inners| Self::Tup { inners }).collect()
    }
}