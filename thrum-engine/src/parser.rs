use std::rc::Rc;

use crate::ast_structure::*;
use crate::tokens::{Token, TokenType};


pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    pub errors: Vec<String>,
    prev_token_line: usize,
    pipe_operators_active: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0, errors: Vec::new(), prev_token_line: 0, pipe_operators_active: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or(&Token {
            token_type: TokenType::EndOfFile, line: usize::MAX,
        })
    }

    fn advance(&mut self) {
        self.prev_token_line = self.peek().line;
        self.position += 1;
    }

    fn peek_is_on_same_line(&self) -> bool {
        self.peek().line == self.prev_token_line
    }

    fn expect_token(&mut self, expected: TokenType, error_msg: &str) -> Result<Token, String> {
        let token = self.peek();
        if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) {
            let cloned_token = token.clone();
            self.advance();
            Ok(cloned_token)
        } else {
            Err(format!("{} Found '{}' instead.", error_msg, self.peek()))
        }
    }
    fn expect_identifier(&mut self, error_msg: &str) -> Result<String, String> {
        let TokenType::Identifier(name) = self.expect_token(TokenType::Identifier(String::new()), error_msg)?.token_type else { unreachable!() };
        Ok(name)
    }

    fn optional_token(&mut self, expected: TokenType) -> bool {
        if std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(&expected) {
            self.advance();
            return true;
        }
        false
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::get_precedence(&self.peek().token_type)
    }

    fn at_end(&self) -> bool {
        self.peek().token_type == TokenType::EndOfFile
    }

    pub fn parse_program(&mut self) -> Vec<TypedExpr> {
        let block_expression = self.parse_block_expression(TokenType::EndOfFile);
        match block_expression.expression {
            Expr::Block(body) => body,
            _ => unreachable!()
        }
    }

    fn parse_block_expression(&mut self, end_token: TokenType) -> TypedExpr {
        // '{' already consumed.
        let mut expression_body = Vec::new();
        while self.peek().token_type != TokenType::EndOfFile && self.peek().token_type != end_token {
            match self.parse_expression(Precedence::Lowest) {
                Ok(expr) => {
                    expression_body.push(expr);
                    if self.optional_token(TokenType::Semicolon) {
                        expression_body.push(Expr::Void.into());
                    }
                    else {
                        // no semicolon -> next expression can't be on the same line.
                        if self.peek_is_on_same_line() && self.peek().token_type != end_token {
                            self.errors.push(format!("2 expressions cannot be on the same line without a ';'. Found '{}'.", self.peek()))
                        }
                    }
                }
                Err(e) => {
                    self.errors.push(e);
                    self.recover(); // Recover from the error
                }
            }
        }
        if let Err(msg) = self.expect_token(end_token.clone(), &format!("Expected '{}' to close the block.", end_token)) {
            self.errors.push(msg);
        }
        Expr::Block(expression_body).into()
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<TypedExpr, String> {
        // could be number, identifier, '-', 'let', ...
        let mut left_expr = self.parse_prefix()?;

        // core Pratt parser loop
        // it continues as long as the next token is an infix operator with a higher precedence than the current level.
        while precedence < self.peek_precedence() {
            let operator = self.peek().token_type.clone();

            let operator_is_call_like = matches!(operator,
                TokenType::LeftParen | TokenType::LeftBracket | TokenType::LeftBrace
            );
            // call-like operator but not on the same line -> break the loop.
            if operator_is_call_like && !self.peek_is_on_same_line() { break; }

            let mut operator_precedence = Precedence::get_precedence(&operator);
            if operator_precedence == Precedence::Lowest { break; } // Not an infix operator.
            if operator == TokenType::StarStar { operator_precedence = Precedence::Product }  // 1 level lower for right associativity

            self.advance();

            // parse infix/postfix
            left_expr = match operator {
                TokenType::LeftParen => self.parse_call_operator(left_expr)?,
                TokenType::RightArrow => self.parse_closure_operator(left_expr)?,
                TokenType::LeftBracket => self.parse_index_operator(left_expr)?,
                TokenType::Colon => self.parse_type_annotation_operator(left_expr)?,
                TokenType::ColonColon => self.parse_path_operator(left_expr)?,
                TokenType::PipeGreater => self.parse_pipe_greater_operator(left_expr)?,
                TokenType::Caret => Expr::Deref { expr: Box::new(left_expr) }.into(),
                
                _ => {  // other operators
                    let right_expr = self.parse_expression(operator_precedence)?;
                    if operator_precedence == Precedence::Assign {
                        let assign_pattern = self.convert_assign_expr_into_pattern(left_expr)?;
                        let extra_operator = Precedence::convert_assign_operator_to_operator(&operator).unwrap();
                        Expr::Assign { left: Box::new(assign_pattern), extra_operator, right: Box::new(right_expr) }.into()
                    }
                    else { Expr::Infix { left: Box::new(left_expr), operator, right: Box::new(right_expr) }.into() }
                }
            }
            // loop again to see if there's another operator to the right
        }
        Ok(left_expr)
    }


    fn parse_prefix(&mut self) -> Result<TypedExpr, String> {
        let token_type = self.peek().token_type.clone();
        // before advancing:
        match token_type {
            TokenType::ColonColon => Ok(Expr::Path(vec!["".to_string()]).into()),
            _ => {
                self.advance();
                // after advancing:
                match token_type {
                    TokenType::Identifier(name) => Ok(Expr::Identifier { name }.into()),
                    TokenType::Number(val) => Ok(Expr::Literal(Value::Num(val)).into()),
                    TokenType::StringFrag(val) => Ok(Expr::Literal(Value::Str(val)).into()),
                    TokenType::Bool(val) => Ok(Expr::Literal(Value::Bool(val)).into()),

                    // Prefix operators
                    TokenType::Minus | TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => self.parse_prefix_operator(token_type),
                    TokenType::Caret => self.parse_caret_expression(),
                    TokenType::LeftBrace => Ok(self.parse_block_expression(TokenType::RightBrace)),
                    TokenType::LeftParen => self.parse_grouped_expression(),
                    TokenType::LeftBracket => self.parse_array_expression(),
                    TokenType::StringStart => self.parse_template_string(),
                    TokenType::Let => self.parse_let_expression(),
                    TokenType::If => self.parse_if_expression(),
                    TokenType::While => self.parse_while_expression(),
                    TokenType::Match => self.parse_match_expression(),
                    TokenType::Enum => self.parse_enum_expression(),
                    TokenType::Fn => self.parse_fn_definition(),
                    TokenType::Return => self.parse_return_epxression(),
                    TokenType::Break => Ok(Expr::Break.into()),
                    TokenType::Mut => {
                        let name = self.expect_identifier("Expected identifier after mut.")?;
                        Ok(Expr::MutRef { expr: Box::new(Expr::Identifier { name }.into()) }.into())
                    }

                    _ => Err(format!("Expected an expression. Found '{}' instead", token_type)),
                }
            }
        }
    }

    fn parse_prefix_operator(&mut self, operator: TokenType) -> Result<TypedExpr, String> {
        // ('-', '!', '~!', '...') already consumed.
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expr::Prefix { operator, right: Box::new(right) }.into())
    }


    fn parse_caret_expression(&mut self) -> Result<TypedExpr, String> {
        let name = format!("_pipe_{}", self.pipe_operators_active);
        Ok(Expr::Identifier { name }.into())
    }



    fn peek_is_expression_start(&self) -> bool {
        matches!(
            self.peek().token_type,
            // these should match parse_prefix function
            TokenType::Identifier(_) | TokenType::Number(_) | TokenType::StringFrag(_) | TokenType::StringStart | TokenType::Bool(_) | TokenType::Null
            | TokenType::Minus | TokenType::Exclamation | TokenType::BitNot
            | TokenType::LeftBrace | TokenType::LeftParen
            | TokenType::Let | TokenType::If | TokenType::Fn
        )
    }


    fn parse_closure_operator(&mut self, params_expr: TypedExpr) -> Result<TypedExpr, String> {
        // '->' already consumed.
        let params = self.convert_param_exprs_into_patterns(params_expr)?;
        let body = self.parse_expression(Precedence::Lowest)?.into();
        Ok(Expr::Closure { params, return_type: TypeKind::ParserUnknown, body }.into())
    }

    fn parse_call_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, String> {
        // '(' already consumed.
        let params = self.parse_expression_list(TokenType::RightParen, "Expected ')' to close argument list.")?;
        Ok(Expr::Call { callee: Box::new(left_expr), arguments: params }.into())
    }

    fn parse_index_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, String> {
        // '[' already consumed.
        let right_index_expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(TokenType::RightBracket, "Expected ']' to close index expression.")?;
        Ok(Expr::Index { left: Box::new(left_expr), index: Box::new(right_index_expr) }.into())
    }

    fn parse_type_annotation_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, String> {
        match left_expr.expression {
            Expr::Identifier { name } => {
                let right_type_expr = self.parse_type_expression()?;
                Ok(Expr::ParserTempTypeAnnotation(AssignablePattern::Binding { name, typ: right_type_expr }).into())
            }
            _ => return Err(format!("Type annotations are only allowed after identifiers. Found after: {:?}", left_expr)),
        }
    }

    fn parse_path_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, String> {
        match left_expr.expression {
            Expr::Identifier { name } => {
                let next_path_segment = self.expect_identifier("Expected identifier after '::'")?;
                Ok(Expr::Path(vec![name, next_path_segment]).into())
            }
            Expr::Path(mut segments) => {
                let next_path_segment = self.expect_identifier("Expected identifier after '::'")?;
                segments.push(next_path_segment);
                Ok(Expr::Path(segments).into())
            }
            _ => return Err(format!("'::'-paths are only allowed after identifiers. Found after: {:?}", left_expr)),
        }
    }

    fn parse_pipe_greater_operator(&mut self, left_expr: TypedExpr) -> Result<TypedExpr, String> {
        self.pipe_operators_active += 1;
        let right_expr_result = self.parse_expression(Precedence::Lowest);
        let pipe_identifier_name = format!("_pipe_{}", self.pipe_operators_active);
        self.pipe_operators_active -= 1;

        Ok(Expr::Block(vec![
            Expr::Let {
                pattern: AssignablePattern::Binding { name: pipe_identifier_name, typ: TypeKind::ParserUnknown },
                value: Box::new(left_expr),
                alternative: None,
            }.into(),
            right_expr_result?
        ]).into())
    }








    fn parse_let_expression(&mut self) -> Result<TypedExpr, String> {
        // 'let' already consumed.
        let pattern = self.parse_binding_pattern(false)?;

        // `=`
        self.expect_token(TokenType::Equal, "Expected '=' after variable name.")?;

        // expression
        let value = Box::new(self.parse_expression(Precedence::Lowest)?);

        // optional else
        let alternative = if self.optional_token(TokenType::Else) {
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        }
        else { None };

        Ok(Expr::Let { pattern, value, alternative }.into())
    }



    fn parse_type_expression(&mut self) -> Result<TypeKind, String> {
        // '?' Option wrapper
        if self.optional_token(TokenType::Quest) {
            return Ok(TypeKind::Struct { name: "Option".to_string(), inner_types: vec![self.parse_type_expression()?] })
        }

        // 'str' or 'arr<T>'.
        let str_type = if self.optional_token(TokenType::Null) { String::from("null") }
        else if self.optional_token(TokenType::Minus) { String::from("-") }
        else if self.optional_token(TokenType::Exclamation) { String::from("!") }
        else { self.expect_identifier("Expected a type name.")? };

        // '<' start of an inner type
        let inner_types = if self.optional_token(TokenType::Less) { self.parse_type_list()? }
        else { Vec::new() };

        Ok(self.type_from_str(&str_type, inner_types)?)
    }

    fn type_from_str(&mut self, str: &str, inner_types: Vec<TypeKind>) -> Result<TypeKind, String> {
        match str {
            "num" => Ok(TypeKind::Num),
            "str" => Ok(TypeKind::Str),
            "bool" => Ok(TypeKind::Bool),
            "tup" => Ok(TypeKind::Tup(inner_types)),
            "arr" => {
                if inner_types.len() == 1 { Ok(TypeKind::Arr(Box::new(inner_types.into_iter().next().unwrap()))) }
                else { Err(format!("Invalid types for arr. arr needs exactly 1 inner type.")) }
            }
            "-" => Ok(TypeKind::Void),
            "!" => Ok(TypeKind::Never),
            "_" => Ok(TypeKind::ParserUnknown),
            &_ => Ok(TypeKind::Struct { name: str.to_string(), inner_types })
        }
    }


    fn parse_type_list(&mut self) -> Result<Vec<TypeKind>, String> {
        let mut types = Vec::new();
        
        while self.peek().token_type != TokenType::Greater {
            types.push(self.parse_type_expression()?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(TokenType::Greater, "Expected '>' to close generic type list.")?;
        Ok(types)
    }




    fn convert_param_exprs_into_patterns(&mut self, expr: TypedExpr) -> Result<Vec<AssignablePattern>, String> {
        // this function takes what was to the left of `->` and validates it.
        let params = match expr.expression {
            Expr::Tuple(body) => body,
            Expr::Identifier { .. } | Expr::ParserTempTypeAnnotation(_) | Expr::Array(_) => vec![expr],
            _ => return Err(format!("Invalid parameter list for function. Found {:?}.", expr)),
        };

        // recursive validation!!!
        params
            .into_iter()
            .map(|p| self.convert_param_expr_into_pattern(p))
            .collect()
    }

    fn convert_param_expr_into_pattern(&mut self, param_expr: TypedExpr) -> Result<AssignablePattern, String> {
        match param_expr.expression {
            // 'x: int'
            Expr::ParserTempTypeAnnotation(pattern) => Ok(pattern),
            // 'x'
            Expr::Identifier { name } => Ok(AssignablePattern::Binding { name, typ: TypeKind::ParserUnknown }),

            // '[x, y]'
            Expr::Array(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_param_expr_into_pattern(element)?);
                }
                Ok(AssignablePattern::Array(converted_elements))
            },

            // '(x, y)'
            Expr::Tuple(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_param_expr_into_pattern(element)?);
                }
                Ok(AssignablePattern::Tuple(converted_elements))
            }

            _ => Err(format!("Invalid syntax in parameter list. Found {:?}.", param_expr)),
        }
    }



    fn convert_assign_expr_into_pattern(&mut self, assign_expr: TypedExpr) -> Result<AssignablePattern, String> {
        match assign_expr.expression {
            Expr::Identifier { name } => {
                Ok(AssignablePattern::Place(PlaceExpr::Identifier(name)))
            }
            Expr::Index { left, index } => {
                Ok(AssignablePattern::Place(PlaceExpr::Index { left: Rc::new(*left), index: Rc::new(*index) }))
            }

            Expr::Array(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_assign_expr_into_pattern(element)?);
                }
                Ok(AssignablePattern::Array(converted_elements))
            }
            Expr::Tuple(elements) => {
                let mut converted_elements = Vec::new();
                for element in elements {
                    converted_elements.push(self.convert_assign_expr_into_pattern(element)?);
                }
                Ok(AssignablePattern::Tuple(converted_elements))
            }
            Expr::Deref { expr } => {
                match (*expr).expression {
                    Expr::Identifier { name } => {
                        Ok(AssignablePattern::Place(PlaceExpr::Deref(name)))
                    }
                    _ => Err(format!("^ is only allowed after identifiers in place expressions."))
                }
            }

            _ => Err(format!("Invalid syntax in assignment expression. Found {:?}.", assign_expr)),
        }
    }






    
    fn parse_if_expression(&mut self) -> Result<TypedExpr, String> {
        // 'if' already consumed.
        if self.optional_token(TokenType::Let) {
            let pattern = self.parse_binding_pattern(false)?;
            self.expect_token(TokenType::Equal, "Expected '=' after 'if let <pattern>'")?;
            let value = Box::new(self.parse_expression(Precedence::Lowest)?);
            let (consequence, alternative) = self.parse_if_and_else()?;
            Ok(Expr::IfLet { pattern, value, consequence, alternative }.into())
        }
        else {
            let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
            let (consequence, alternative) = self.parse_if_and_else()?;
            Ok(Expr::If { condition, consequence, alternative }.into())
        }
    }
    fn parse_if_and_else(&mut self) -> Result<(Box<TypedExpr>, Option<Box<TypedExpr>>), String> {
        self.expect_token( TokenType::LeftBrace, "Expected '{' after if condition.")?;

        let consequence = Box::new(self.parse_block_expression(TokenType::RightBrace));
        let alternative = if self.optional_token(TokenType::Else) {
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        }
        else { None };

        Ok((consequence, alternative))
    }


    fn parse_while_expression(&mut self) -> Result<TypedExpr, String> {
        // 'while' already consumed.
        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
        
        self.expect_token( TokenType::LeftBrace, "Expected '{' after while condition.")?;
        let body = Box::new(self.parse_block_expression(TokenType::RightBrace));

        Ok(Expr::While { condition, body }.into())
    }


    fn parse_match_expression(&mut self) -> Result<TypedExpr, String> {
        // 'match' already consumed.
        let matcher = self.parse_expression(Precedence::Lowest)?;

        self.expect_token( TokenType::LeftBrace, "Expected '{' to open match block.")?;
        let mut arms = Vec::new();

        while !matches!(self.peek().token_type, TokenType::RightBrace | TokenType::EndOfFile) {
            let pattern = self.parse_binding_pattern(false)?;

            self.expect_token( TokenType::RightArrow, "Expected '->' after match block case.")?;            
            let body = self.parse_expression(Precedence::Lowest)?;
            self.optional_token(TokenType::Comma);
            arms.push(MatchArm { pattern, body });
        }
        self.expect_token( TokenType::RightBrace, "Expected '}' to close match block.")?;

        Ok(Expr::Match { match_value: Box::new(matcher), arms }.into())                
    }


    fn parse_enum_expression(&mut self) -> Result<TypedExpr, String> {
        // 'enum' already consumed.
        let name = self.expect_identifier("Expected enum name after 'enum'.")?;
        
        self.expect_token( TokenType::LeftBrace, "Expected '{' to open enum block.")?;
        let mut enums = Vec::new();

        
        while self.peek().token_type != TokenType::RightBrace {
            let enum_name = self.expect_identifier("Expected identifier name inside enum block.")?;
            let mut inner_types = Vec::new();

            if self.optional_token(TokenType::LeftParen) {
                while self.peek().token_type != TokenType::RightParen {
                    inner_types.push(self.parse_binding_pattern(true)?);
                    if !self.optional_token(TokenType::Comma) { break }
                }
                self.expect_token(TokenType::RightParen, "Expected ')' to close enum parameter list.")?;
            }

            enums.push(EnumExpression { name: enum_name, inner_types });
            if !self.optional_token(TokenType::Comma) { break }
        }
        self.expect_token(TokenType::RightBrace, "Expected '}' to close enum block.")?;

        Ok(Expr::EnumDefinition { name, enums }.into())     
    }





    fn parse_fn_definition(&mut self) -> Result<TypedExpr, String> {
        // 'fn' already consumed.
        let name = self.expect_identifier("Expected function name after 'fn'.")?;
        self.expect_token(TokenType::LeftParen, &format!("Expected '(' after 'fn {}'", name))?;
        let params = self.parse_binding_pattern_list(TokenType::RightParen, true, "Expected ')' to close fn parameter list.")?;

        let return_type = if self.optional_token(TokenType::RightArrow) {
            self.parse_type_expression()?
        }
        else { TypeKind::Void };

        self.expect_token(TokenType::LeftBrace, "Expected '{' after fn definition.")?;
        let body = Rc::new(self.parse_block_expression(TokenType::RightBrace));
        
        Ok(Expr::FnDefinition { name, params, return_type, body }.into())
    }


    fn parse_return_epxression(&mut self) -> Result<TypedExpr, String> {
        // 'return' already consumed.
        let return_expression = if self.peek_is_expression_start() && self.peek_is_on_same_line() {
            Box::new(self.parse_expression(Precedence::Lowest)?)
        }
        else { Box::new(Expr::Void.into()) };
        Ok(Expr::Return(return_expression).into())
    }


    fn parse_grouped_expression(&mut self) -> Result<TypedExpr, String> {
        // '(' already consumed.

        // empty tuple case '()'
        if self.optional_token(TokenType::RightParen) { return Ok(Expr::Tuple(Vec::new()).into()); }

        let first_expr = self.parse_expression(Precedence::Lowest)?;
        if self.optional_token(TokenType::Comma) {
            // Tuple!
            let mut tuple_body = vec![first_expr];
            tuple_body.extend(self.parse_expression_list(TokenType::RightParen, "Expected ')' to close tuple.")?);
            Ok(Expr::Tuple(tuple_body).into())
        }
        else {
            self.expect_token(TokenType::RightParen, "Expected ')' to close grouped expression.")?;
            Ok(first_expr)
        }
    }

    fn parse_array_expression(&mut self) -> Result<TypedExpr, String> {
        // '[' already consumed
        Ok(Expr::Array(self.parse_expression_list(TokenType::RightBracket, "Expected ']' to close array.")?).into())
    }

    fn parse_expression_list(&mut self, end_token: TokenType, error_msg: &str) -> Result<Vec<TypedExpr>, String> {
        // '[1, 2, 3]',   '(1, 2)',   dict { 1, 2 }
        let mut list = Vec::new();

        while self.peek().token_type != end_token {
            list.push(self.parse_expression(Precedence::Lowest)?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(end_token, error_msg)?;
        Ok(list)
    }


    fn parse_template_string(&mut self) -> Result<TypedExpr, String> {
        // 'StringStart' already consumed.
        let mut parts = Vec::new();

        loop {
            match self.peek().token_type.clone() {
                TokenType::StringFrag(s) => {
                    self.advance();
                    parts.push(Expr::Literal(Value::Str(s)).into());
                }
                TokenType::LeftBrace => {
                    self.advance();
                    let expression_part = self.parse_expression(Precedence::Lowest)?;
                    parts.push(expression_part);
                    self.expect_token(TokenType::RightBrace, "Expected '}' after expression in template string.")?;
                }
                TokenType::StringEnd => {
                    self.advance();
                    return Ok(Expr::TemplateString(parts).into())
                }
                _ => { return Err(format!("Unexpected token inside string: '{}'", self.peek())); }
            }
        }
    }



    fn parse_binding_pattern(&mut self, type_annotation_required: bool) -> Result<AssignablePattern, String> {
        let pattern = match self.peek().token_type.clone() {
            TokenType::Number(num) => { self.advance(); Ok(AssignablePattern::Literal(Value::Num(num))) }
            TokenType::Bool(bool) => { self.advance(); Ok(AssignablePattern::Literal(Value::Bool(bool))) }
            TokenType::StringStart => {
                self.advance();
                if let TokenType::StringFrag(str) = self.peek().token_type.clone() {
                    self.advance();
                    self.expect_token(TokenType::StringEnd, "String literals are not allowed in match patterns.")?;
                    Ok(AssignablePattern::Literal(Value::Str(str)))
                }
                else { Err(format!("Complex string literals are not allowed in match patterns.")) }
            }

            TokenType::Identifier(name) => {
                self.advance();
                if name.chars().nth(0).unwrap() == '_' { Ok(AssignablePattern::Wildcard) }
                else { self.parse_binding_path(name, type_annotation_required) }
            }
            TokenType::LeftBracket => {
                self.advance();
                Ok(AssignablePattern::Array(self.parse_binding_pattern_list(
                    TokenType::RightBracket, type_annotation_required, "Expected ']' to close array pattern."
                )?))
            }
            TokenType::LeftParen => {
                self.advance();

                let first_pattern = self.parse_binding_pattern(type_annotation_required)?;
                if self.optional_token(TokenType::Comma) {
                    // Tuple!
                    let mut tuple_body = vec![first_pattern];
                    tuple_body.extend(self.parse_binding_pattern_list(
                        TokenType::RightParen, type_annotation_required, "Expected ')' to close tuple pattern."
                    )?);
                    Ok(AssignablePattern::Tuple(tuple_body))
                }
                else {
                    self.expect_token(TokenType::RightParen, "Expected ')' to close grouped pattern.")?;
                    Ok(first_pattern)
                }
            }
            TokenType::ColonColon => self.parse_binding_path("".to_string(), type_annotation_required),
            _ => Err(format!("Expected pattern. Found '{}'", self.peek()))
        }?;

        if self.optional_token(TokenType::If) {
            let body = self.parse_expression(Precedence::Arrow)?;
            Ok(AssignablePattern::Conditional { pattern: Box::new(pattern), body: Rc::new(body) })
        }
        else { Ok(pattern) }
    }

    fn parse_binding_pattern_list(&mut self, end_token: TokenType, type_annotation_required: bool, error_msg: &str) -> Result<Vec<AssignablePattern>, String> {
        let mut list = Vec::new();
        while self.peek().token_type != end_token {
            list.push(self.parse_binding_pattern(type_annotation_required)?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(end_token, error_msg)?;
        Ok(list)
    }

    fn parse_binding_path(&mut self, path_start: String, type_annotation_required: bool) ->  Result<AssignablePattern, String> {
        let mut segments = vec![path_start];
        while self.optional_token(TokenType::ColonColon) {
            segments.push(self.expect_identifier("Expected identifier after '::' in pattern.")?);
        }

        // Binding-pattern
        if segments.len() == 1 {
            let type_annotation = if self.optional_token(TokenType::Colon) {
                self.parse_type_expression()?
            } else { TypeKind::ParserUnknown };
            return Ok(AssignablePattern::Binding { name: segments.pop().unwrap(), typ: type_annotation })
        }

        let inner_patterns = if self.optional_token(TokenType::LeftParen) {
            self.parse_binding_pattern_list(TokenType::RightParen, type_annotation_required, "Expected ')' to close enum variant pattern.")?
        }
        else { Vec::new() };

        let name = segments.pop().unwrap();
        Ok(AssignablePattern::EnumVariant { path: segments, name, inner_patterns })
    }











    fn recover(&mut self) {
        while !self.at_end() {
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
                return;
            }
            if self.peek_is_expression_start() {
                return;
            }
            else { self.advance(); }
        }
    }
}



#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Colon,      // :
    Arrow,      // ->
    Assign,     // =, +=, -=, etc.
    Pipe,       // |>
    Range,      // 1..2
    Nullish,    // ??
    Or,         // |
    And,        // &
    BitwiseOr,  // ~|
    BitwiseXor, // ~^
    BitwiseAnd, // ~&
    Equals,     // ==, !=
    LessGreater,// <, >, <=, >=
    Shift,      // <<, >>
    Sum,        // +, -
    Product,    // *, /, %
    Power,      // **
    Postfix,    // ^
    Prefix,     // !, ~!, -
    CallIndex,  // square(X), array[i], dict {1, 2}
}
impl Precedence {
    fn get_precedence(token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Colon => Precedence::Colon,
            TokenType::RightArrow => Precedence::Arrow,
            x if Precedence::convert_assign_operator_to_operator(x).is_some() => Precedence::Assign,
            TokenType::DotDot | TokenType::DotDotLess => Precedence::Range,
            TokenType::Quest => Precedence::Nullish,
            TokenType::PipeGreater => Precedence::Pipe,
            TokenType::Pipe => Precedence::Or,
            TokenType::Ampersand => Precedence::And,
            TokenType::BitOr => Precedence::BitwiseOr,
            TokenType::BitXor => Precedence::BitwiseXor,
            TokenType::BitAnd => Precedence::BitwiseAnd,
            TokenType::EqualEqual | TokenType::NotEqual => Precedence::Equals,
            TokenType::Less | TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual => Precedence::LessGreater,
            TokenType::LeftShift | TokenType::RightShift => Precedence::Shift,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Star | TokenType::Slash | TokenType::Percent => Precedence::Product,
            TokenType::StarStar => Precedence::Power,
            TokenType::Exclamation | TokenType::BitNot | TokenType::DotDotDot => Precedence::Prefix,
            TokenType::Caret => Precedence::Postfix,
            TokenType::LeftParen | TokenType::LeftBracket | TokenType::Dot | TokenType::ColonColon | TokenType::QuestDot => Precedence::CallIndex,
            _ => Precedence::Lowest,
        }
    }

    fn convert_assign_operator_to_operator(assign_operator: &TokenType) -> Option<TokenType> {
        match assign_operator {
            TokenType::Equal => Some(TokenType::Equal),
            TokenType::PlusEqual => Some(TokenType::Plus),
            TokenType::MinusEqual => Some(TokenType::Minus),
            TokenType::StarEqual => Some(TokenType::Star),
            TokenType::SlashEqual => Some(TokenType::Slash),
            TokenType::StarStarEqual => Some(TokenType::StarStar),
            TokenType::PercentEqual => Some(TokenType::Percent),
            TokenType::QuestEqual => Some(TokenType::Quest),
            TokenType::BitAndEqual => Some(TokenType::BitAnd),
            TokenType::BitOrEqual => Some(TokenType::BitOr),
            TokenType::BitXorEqual => Some(TokenType::BitXor),
            TokenType::LeftShiftEqual => Some(TokenType::LeftShift),
            TokenType::RightShiftEqual => Some(TokenType::RightShift),
            _ => None
        }
    }
}