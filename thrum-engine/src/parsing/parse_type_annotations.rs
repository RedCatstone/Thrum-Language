use crate::{lexing::tokens::TokenType, parsing::{Parser, ast_structure::{Span, TupleType, TypeKind, TypeKindInfo}}};


impl Parser<'_> {
    pub(super) fn parse_type_annotation(&mut self, default_to_ref: bool) -> TypeKindInfo {
        // 'own'-keyword can cancel the default_to_ref thing out.
        if !default_to_ref || self.optional_token(TokenType::Own).is_none() {
            // mut
            if default_to_ref || self.optional_token(TokenType::Ref).is_some() {
                let mutable = self.optional_token(TokenType::Mut).is_some();
                let pointer_type = self.parse_type_annotation(false);

                return TypeKindInfo {
                    span: pointer_type.span,
                    typ: TypeKind::Pointer { mutable, inner: Box::new(pointer_type.typ), borrows_var: None },
                }
            }
        }

        // '?x' Option wrapper
        if let Some(span) = self.optional_token(TokenType::Quest) {
            let inner = self.parse_type_annotation(false);
            return TypeKindInfo {
                span: span.merge(inner.span),
                typ: TypeKind::CustomType { name: "Option".to_string(), generic_types: vec![inner.typ] }
            }
        }

        // '(x: num, y: str)' tuple type
        if let Some(span) = self.optional_token(TokenType::LeftParen) {
            return self.parse_tuple_type_annotation(span, false);
        }

        // normal case: 'str' or 'arr<T>'.
        let (span, str_type) = self.expect_identifier("to name a type");

        // '<' start of an inner type
        let (span, generic_types) = if self.optional_token(TokenType::Less).is_some() {
            // parse comma seperated types here
            self.parse_comma_separated(
                TokenType::Greater,
                |p, _| p.parse_type_annotation(false).typ,
                "to close the generic types list"
            )
        } else {
            (span, Vec::new())
        };

        TypeKindInfo {
            span,
            typ: TypeKind::CustomType { name: str_type, generic_types }.from_custom_type()
        }
    }


    fn parse_one_tuple_type_annotation(&mut self, default_label: String, default_to_ref: bool) -> TupleType {
        let (label, typ) = self.parse_tuple_item(
            default_label,
            |p| p.parse_type_annotation(default_to_ref).typ,
            |typ| match typ {
                TypeKind::CustomType { name, generic_types: g } if g.is_empty() => Some(name.clone()),
                _ => None
            }
        );
        TupleType { label, typ }
    }

    pub(super) fn parse_tuple_type_annotation(&mut self, s_span: Span, default_to_ref: bool) -> TypeKindInfo {
        let (end_span, elements) = self.parse_comma_separated(
            TokenType::RightParen,
            |p, i| p.parse_one_tuple_type_annotation((i+1).to_string(), default_to_ref),
            "to close the tuple type"
        );

        TypeKindInfo { span: s_span.merge(end_span), typ: TypeKind::Tup(elements) }
    }
}