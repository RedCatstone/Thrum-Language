use crate::ast_structure::*;
use crate::tokens::{Token, TokenType};
use crate::type_checker::TypeKind;

// The precedence levels for our operators. This is crucial.
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
    Prefix,     // !, ~!, -
    CallIndex,  // square(X), array[i], dict {1, 2}
}



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
        get_precedence(&self.peek().token_type)
    }

    fn at_end(&self) -> bool {
        self.peek().token_type == TokenType::EndOfFile
    }

    pub fn parse_program(&mut self) -> Vec<TypedExpression> {
        let block_expression = self.parse_block_expression(TokenType::EndOfFile);
        match block_expression.expression {
            Expression::Block(body) => body,
            _ => unreachable!()
        }
    }

    fn parse_block_expression(&mut self, end_token: TokenType) -> TypedExpression {
        // '{' already consumed.
        let mut expression_body = Vec::new();
        while self.peek().token_type != TokenType::EndOfFile && self.peek().token_type != end_token {
            match self.parse_expression(Precedence::Lowest) {
                Ok(expr) => {
                    expression_body.push(expr);
                    if !self.optional_token(TokenType::Semicolon) {
                        // no semicolon -> next expression can't be on the same line.
                        if self.peek_is_on_same_line() {
                            self.errors.push(format!("2 seperate expressions can't be on the same line without a ';'. Found '{}'.", self.peek()))
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
        Expression::Block(expression_body).into()
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<TypedExpression, String> {
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

            self.advance();

            // parse infix
            left_expr = match operator {
                TokenType::LeftParen => self.parse_function_call_operator(left_expr)?,
                TokenType::RightArrow => self.parse_function_operator(left_expr)?,
                TokenType::LeftBracket => self.parse_index_operator(left_expr)?,
                TokenType::LeftBrace => self.parse_curly_new_operator(left_expr)?,
                TokenType::Colon => self.parse_type_annotation_operator(left_expr)?,
                TokenType::PipeGreater => self.parse_pipe_greater_operator(left_expr)?,
                
                _ => {  // other operators
                    let mut operator_precedence = get_precedence(&operator);
                    if operator_precedence == Precedence::Lowest { break; } // Not an infix operator.
                    if operator == TokenType::StarStar { operator_precedence = Precedence::Product }  // 1 level lower for right associativity

                    let right_expr = self.parse_expression(operator_precedence)?;
                    if operator_precedence == Precedence::Assign {
                        Expression::Assign { left: Box::new(left_expr), operator, right: Box::new(right_expr) }.into()
                    }
                    else { Expression::Infix { left: Box::new(left_expr), operator, right: Box::new(right_expr) }.into() }
                }
            }
            // loop again to see if there's another operator to the right
        }
        Ok(left_expr)
    }


    fn parse_prefix(&mut self) -> Result<TypedExpression, String> {
        let token_type = self.peek().token_type.clone();
        self.advance();
        match token_type {
            TokenType::Identifier(name) => Ok(Expression::Identifier(name).into()),
            TokenType::Number(val) => Ok(Expression::Literal(LiteralValue::Number(val)).into()),
            TokenType::StringFrag(val) => Ok(Expression::Literal(LiteralValue::String(val)).into()),
            TokenType::Bool(val) => Ok(Expression::Literal(LiteralValue::Bool(val)).into()),

            // Prefix operators
            TokenType::Minus | TokenType::Not | TokenType::BitNot | TokenType::DotDotDot => self.parse_prefix_operator(token_type),
            TokenType::Caret => self.parse_caret_expression(),
            TokenType::LeftBrace => Ok(self.parse_block_expression(TokenType::RightBrace)),
            TokenType::LeftParen => self.parse_grouped_expression(),
            TokenType::LeftBracket => self.parse_array_expression(),
            TokenType::StringStart => self.parse_template_string(),
            TokenType::Let => self.parse_let_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Match => self.parse_match_expression(),
            TokenType::Fn => self.parse_fn_definition(),

            _ => Err(format!("Expected an expression. Found '{}' instead", token_type)),
        }
    }

    fn parse_prefix_operator(&mut self, operator: TokenType) -> Result<TypedExpression, String> {
        // ('-', '!', '~!', '...') already consumed.
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix { operator, right: Box::new(right) }.into())
    }


    fn parse_caret_expression(&mut self) -> Result<TypedExpression, String> {
        Ok(Expression::Identifier(format!("_pipe_{}", self.pipe_operators_active)).into())
    }



    fn peek_is_expression_start(&self) -> bool {
        matches!(
            self.peek().token_type,
            // these should match parse_prefix function
            TokenType::Identifier(_) | TokenType::Number(_) | TokenType::StringFrag(_) | TokenType::StringStart | TokenType::Bool(_) | TokenType::Null
            | TokenType::Minus | TokenType::Not | TokenType::BitNot
            | TokenType::LeftBrace | TokenType::LeftParen
            | TokenType::Let | TokenType::If | TokenType::Fn
        )
    }


    fn parse_function_operator(&mut self, params_expr: TypedExpression) -> Result<TypedExpression, String> {
        // '->' already consumed.
        let params = self.validate_and_build_function_params(params_expr)?;
        let body = self.parse_expression(Precedence::Lowest)?;
        Ok(Expression::Fn { params, body: Box::new(body), return_value: None }.into())
    }

    fn parse_function_call_operator(&mut self, left_expr: TypedExpression) -> Result<TypedExpression, String> {
        // '(' already consumed.
        let params = self.parse_expression_list(TokenType::RightParen, "Expected ')' to close argument list.")?;
        Ok(Expression::FnCall { function: Box::new(left_expr), arguments: params }.into())
    }

    fn parse_index_operator(&mut self, left_expr: TypedExpression) -> Result<TypedExpression, String> {
        // '[' already consumed.
        let right_index_expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(TokenType::RightBracket, "Expected ']' to close index expression.")?;
        Ok(Expression::Index { left: Box::new(left_expr), index: Box::new(right_index_expr) }.into())
    }

    fn parse_curly_new_operator(&mut self, left_expr: TypedExpression) -> Result<TypedExpression, String> {
        // '{' already consumed
        let name = match left_expr.expression {
            Expression::Identifier(name) => name,
            _ => return Err(format!("Curly new is only allowed after an identifier"))
        };
        let right_params_expr = self.parse_expression_list(TokenType::RightBrace, "Expected '}' to close curly new expression.")?;
        Ok(Expression::CurlyNew { name, params: right_params_expr }.into())
    }

    fn parse_type_annotation_operator(&mut self, left_expr: TypedExpression) -> Result<TypedExpression, String> {
        match left_expr.expression {
            Expression::Identifier(name) => {
                let right_type_expr = self.parse_type_expression()?;
                Ok(Expression::ParserTempPattern(ExpressionPattern::NameAndType { name, typ: right_type_expr }).into())
            }
            _ => return Err(format!("Type annotations are only allowed after identifiers. Found after: {:?}", left_expr)),
        }
    }

    fn parse_pipe_greater_operator(&mut self, left_expr: TypedExpression) -> Result<TypedExpression, String> {
        self.pipe_operators_active += 1;
        let right_expr_result = self.parse_expression(Precedence::Lowest);
        let pipe_identifier_name = format!("_pipe_{}", self.pipe_operators_active);
        self.pipe_operators_active -= 1;

        Ok(Expression::Block(vec![
            Expression::Let {
                pattern: ExpressionPattern::NameAndType { name: pipe_identifier_name, typ: TypeKind::ParserUnknown },
                value: Box::new(left_expr)
            }.into(),
            right_expr_result?
        ]).into())
    }








    fn parse_let_expression(&mut self) -> Result<TypedExpression, String> {
        // 'let' already consumed.
        let pattern = self.parse_pattern()?;

        // `=`
        self.expect_token(TokenType::Equal, "Expected '=' after variable name.")?;

        // expression
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Expression::Let { pattern: pattern, value: Box::new(value) }.into())
    }



    fn parse_type_expression(&mut self) -> Result<TypeKind, String> {
        // 'str' or 'arr<T>'.
        let str_type = if self.optional_token(TokenType::Null) { String::from("null") }
        else { self.expect_identifier("Expected a type name.")? };

        // '<' start of an inner type
        let inner_types = if self.optional_token(TokenType::Less) { self.parse_type_list()? }
        else { Vec::new() };

        let mut typ = TypeKind::from_str(&str_type, inner_types);

        // '?' Optional wrapper
        while self.optional_token(TokenType::Quest) {
            typ = TypeKind::Struct { name: "Optional".to_string(), inner_types: vec![typ] }
        }

        Ok(typ)
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

    fn validate_and_build_function_params(&self, expr: TypedExpression) -> Result<Vec<ExpressionPattern>, String> {
        // this function takes what was to the left of `->` and validates it.
        let params = match expr.expression {
            Expression::Tuple(body) => body,
            Expression::Identifier(_) | Expression::ParserTempPattern(_) | Expression::Array(_) => vec![expr],
            _ => return Err(format!("Invalid parameter list for function. Found {:?}.", expr)),
        };

        // recursive validation!!!
        params
            .into_iter()
            .map(|p| self.validate_single_param_pattern(p))
            .collect()
    }

    fn validate_single_param_pattern(&self, param_expr: TypedExpression) -> Result<ExpressionPattern, String> {
        match param_expr.expression {
            // 'x: int'
            Expression::ParserTempPattern(pattern) => Ok(pattern),
            // 'x'
            Expression::Identifier(name) => Ok(ExpressionPattern::NameAndType { name, typ: TypeKind::ParserUnknown }),

            // '[x, y]'
            Expression::Array(elements) => {
                let validated_elements = elements
                    .into_iter()
                    .map(|e| self.validate_single_param_pattern(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionPattern::Array(validated_elements))
            },

            // '(x, (y, z))'
            Expression::Tuple(elements) => {
                let validated_elements = elements
                    .into_iter()
                    .map(|e| self.validate_single_param_pattern(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionPattern::Tuple(validated_elements))
            }

            _ => Err(format!("Invalid syntax in parameter list. Found {:?}.", param_expr)),
        }
    }


    
    fn parse_if_expression(&mut self) -> Result<TypedExpression, String> {
        // 'if' already consumed.
        self.expect_token( TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_token( TokenType::RightParen, "Expected ')' after if condition.")?;

        let consequence = self.parse_expression(Precedence::Lowest)?;
        let alternative = if self.optional_token(TokenType::Else) {
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        }
        else { None };

        Ok(Expression::If { condition: Box::new(condition), consequence: Box::new(consequence), alternative }.into())
    }


    fn parse_match_expression(&mut self) -> Result<TypedExpression, String> {
        // 'match' already consumed.
        self.expect_token( TokenType::LeftParen, "Expected '(' after 'match'.")?;
        let matcher = self.parse_expression(Precedence::Lowest)?;
        self.expect_token( TokenType::RightParen, "Expected ')' after match condition.")?;

        self.expect_token( TokenType::LeftBrace, "Expected '{' to open match block.")?;
        let mut cases = Vec::new();

        while !matches!(self.peek().token_type, TokenType::RightBrace | TokenType::EndOfFile) {
            let case = self.parse_expression(Precedence::Arrow)?;
            self.expect_token( TokenType::RightArrow, "Expected '->' after match block case.")?;            
            let case_consequence = self.parse_expression(Precedence::Lowest)?;
            cases.push((case, case_consequence));
        }
        self.expect_token( TokenType::RightBrace, "Expected '}' to close match block.")?;

        Ok(Expression::Match { matcher: Box::new(matcher), cases }.into())                
    }


    fn parse_fn_definition(&mut self) -> Result<TypedExpression, String> {
        // 'fn' already consumed.
        let name = self.expect_identifier("Expected function name after 'fn'.")?;

        let function = self.parse_expression(Precedence::Lowest)?;
        if !matches!(function.expression, Expression::Fn{ .. }) { return Err(format!("Expected function expression after 'fn {}'. Found {:?} instead,", name, function)); }

        Ok(Expression::FnDefinition { name, function: Box::new(function) }.into())
    }

    fn parse_grouped_expression(&mut self) -> Result<TypedExpression, String> {
        // '(' already consumed.

        // empty tuple case '()'
        if self.optional_token(TokenType::RightParen) { return Ok(Expression::Tuple(Vec::new()).into()); }

        let first_expr = self.parse_expression(Precedence::Lowest)?;
        if self.optional_token(TokenType::Comma) {
            // Tuple!
            let other_tuple_expressions = self.parse_expression_list(TokenType::RightParen, "Expected ')' to close tuple.")?;
            let mut tuple_body = vec![first_expr];
            tuple_body.extend(other_tuple_expressions);
            Ok(Expression::Tuple(tuple_body).into())
        }
        else {
            self.expect_token(TokenType::RightParen, "Expected ')' after expression in parentheses.")?;
            Ok(first_expr)
        }
    }

    fn parse_array_expression(&mut self) -> Result<TypedExpression, String> {
        // '[' already consumed
        Ok(Expression::Array(self.parse_expression_list(TokenType::RightBracket, "Expected ']' to close array.")?).into())
    }

    fn parse_expression_list(&mut self, end_token: TokenType, error_msg: &str) -> Result<Vec<TypedExpression>, String> {
        // '[1, 2, 3]',   '(1, 2)',   dict { 1, 2 }
        let mut list = Vec::new();

        while self.peek().token_type != end_token {
            list.push(self.parse_expression(Precedence::Lowest)?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(end_token, error_msg)?;
        Ok(list)
    }


    fn parse_template_string(&mut self) -> Result<TypedExpression, String> {
        // 'StringStart' already consumed.
        let mut parts = Vec::new();

        loop {
            match self.peek().token_type.clone() {
                TokenType::StringFrag(s) => {
                    self.advance();
                    parts.push(Expression::Literal(LiteralValue::String(s)).into());
                }
                TokenType::LeftBrace => {
                    self.advance();
                    let expression_part = self.parse_expression(Precedence::Lowest)?;
                    parts.push(expression_part);
                    self.expect_token(TokenType::RightBrace, "Expected '}' after expression in template string.")?;
                }
                TokenType::StringEnd => {
                    self.advance();
                    return Ok(Expression::TemplateString(parts).into())
                }
                _ => { return Err(format!("Unexpected token inside string: '{}'", self.peek())); }
            }
        }
    }





    fn parse_pattern(&mut self) -> Result<ExpressionPattern, String> {
        match self.peek().token_type.clone() {
            TokenType::Identifier(name) => {
                self.advance();
                let type_annotation = if self.optional_token(TokenType::Colon) {
                    self.parse_type_expression()?
                } else { TypeKind::ParserUnknown };
                Ok(ExpressionPattern::NameAndType { name, typ: type_annotation })
            }
            TokenType::LeftBracket => {
                self.advance();
                Ok(ExpressionPattern::Array(self.parse_pattern_list(TokenType::RightBracket, "Expected ']' to close array pattern.")?))
            }
            TokenType::LeftParen => {
                self.advance();
                Ok(ExpressionPattern::Tuple(self.parse_pattern_list(TokenType::RightParen, "Expected ')' to close tuple pattern.")?))
            }
            _ => Err(format!("Expected pattern. Found '{}'", self.peek()))
        }
    }

    fn parse_pattern_list(&mut self, end_token: TokenType, error_msg: &str) -> Result<Vec<ExpressionPattern>, String> {
        let mut list = Vec::new();
        loop {
            list.push(self.parse_pattern()?);
            if !self.optional_token(TokenType::Comma) { break; }
        }
        self.expect_token(end_token, error_msg)?;
        Ok(list)
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


fn get_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Colon => Precedence::Colon,
        TokenType::RightArrow => Precedence::Arrow,
        TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual | TokenType::StarEqual | TokenType::SlashEqual
        | TokenType::StarStarEqual | TokenType::PercentEqual | TokenType::QuestQuestEqual | TokenType::BitAndEqual
        | TokenType::BitOrEqual | TokenType::BitXorEqual | TokenType::LeftShiftEqual | TokenType::RightShiftEqual => Precedence::Assign,
        TokenType::DotDot | TokenType::DotDotEqual => Precedence::Range,
        TokenType::QuestQuest => Precedence::Nullish,
        TokenType::PipeGreater => Precedence::Pipe,
        TokenType::Or => Precedence::Or,
        TokenType::And => Precedence::And,
        TokenType::BitOr => Precedence::BitwiseOr,
        TokenType::BitXor => Precedence::BitwiseXor,
        TokenType::BitAnd => Precedence::BitwiseAnd,
        TokenType::EqualEqual | TokenType::NotEqual => Precedence::Equals,
        TokenType::Less | TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual => Precedence::LessGreater,
        TokenType::LeftShift | TokenType::RightShift => Precedence::Shift,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Star | TokenType::Slash | TokenType::Percent => Precedence::Product,
        TokenType::StarStar => Precedence::Power,
        /* TokenType::Minus |*/ TokenType::Not | TokenType::BitNot | TokenType::DotDotDot => Precedence::Prefix,
        TokenType::LeftParen | TokenType::LeftBracket | TokenType::LeftBrace | TokenType::Dot | TokenType::QuestDot => Precedence::CallIndex,
        _ => Precedence::Lowest,
    }
}