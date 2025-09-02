use std::{u8, usize};

use num_enum::TryFromPrimitive;

use crate::{ast_structure::{AssignablePattern, Expr, MatchArm, PlaceExpr, TypeKind, TypedExpr, Value}, tokens::TokenType};


#[repr(u8)]
#[derive(Debug, TryFromPrimitive)]
pub enum OpCode {
    GetConstant,
    GetLocal, SetLocal,
    PopValue, DropValue,
    DuplicateTop,

    // operators / types
    CmpEqual, CmpLess, CmpGreater,
    AddNum, SubtractNum, MultiplyNum, DivideNum, ModuloNum, ExponentNum, NegateNum,
    NegateBool, AndBool, OrBool,
    AddStr, TemplateString,
    CreateArray, ArrIndex, GetArrIndex,
    CreateTuple, GetTupIndex,

    // control flow
    Jump, JumpBack, JumpIfFalse,


    // end of function / program
    Return,
}

#[derive(Default, Debug, Clone)]  // Clone is only for printing
pub struct Bytecode {
    pub codes: Vec<u8>,
    pub constants: Vec<Value>,
}



pub struct CompileBytecode {
    pub bytecode: Bytecode,
    curr_scope_depth: usize,
    locals: Vec<Local>
}
pub struct Local {
    name: String,
    depth: usize,
}

impl CompileBytecode {
    pub fn new() -> Self {
        CompileBytecode {
            bytecode: Bytecode::default(),
            curr_scope_depth: 0,
            locals: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &[TypedExpr]) {
        self.compile_block_expression(program);
        self.push_op(OpCode::Return);
    }

    fn compile_expression(&mut self, expr: &TypedExpr) {
        match &expr.expression {
            Expr::Literal(val) => self.push_get_constant_op(val.clone()),
            Expr::TemplateString(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_operand(OpCode::TemplateString, elements.len());
            }
            Expr::Array(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_operand(OpCode::CreateArray, elements.len());
            }
            Expr::Tuple(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_operand(OpCode::CreateTuple, elements.len());
            }


            Expr::Infix { left, operator, right } => {
                self.compile_expression(left);
                self.compile_expression(right);
                self.compile_infix(&left.typ, operator, &right.typ);
            }
            Expr::Prefix { operator, right } => {
                self.compile_expression(right);
                self.compile_prefix(operator, &right.typ)
            }

            Expr::Block(body) => self.compile_block_expression(body),

            Expr::Let { pattern, value } => {
                self.compile_expression(value);
                self.compile_binding_pattern(pattern);
            }
            Expr::Identifier(name) => {
                self.push_get_local_op(name);
            }

            Expr::Assign { left, extra_operator, right } => {
                if *extra_operator != TokenType::Equal {
                    match *left.clone() {
                        AssignablePattern::Place(place) => {
                            self.push_place_expr_value(place);
                            self.compile_expression(right);
                            self.compile_infix(&right.typ, extra_operator, &right.typ);
                        }
                        _ => unreachable!("Infix assignments are only allowed for place patterns.")
                    }
                }
                else {
                    self.compile_expression(right);
                }

                self.compile_binding_pattern(&left);
            }


            Expr::If { condition, consequence, alternative } => {
                // what it should look like:
                // condition
                // JumpIfFalse 2
                //     "yes"
                // Jump 1
                //     "no"
                // ...
                self.compile_expression(condition);
                let jump_true_to_patch = self.push_op_for_patching(OpCode::JumpIfFalse);

                self.compile_expression(consequence);
                let jump_false_to_patch = if alternative.is_some() { self.push_op_for_patching(OpCode::Jump) }
                else { 0 };

                self.patch_jump_op(jump_true_to_patch);

                if let Some(alt) = alternative {
                    self.compile_expression(alt);
                    self.patch_jump_op(jump_false_to_patch);
                }
            }

            Expr::While { condition, body } => {
                // what it should look like:
                // condition
                // JumpIfFalse 2
                //     while-loop
                // Jump -4
                // ...
                let loop_jump_location = self.bytecode.codes.len();
                self.compile_expression(condition);
                let exit_jump_to_patch = self.push_op_for_patching(OpCode::JumpIfFalse);
                self.compile_expression(body);
                self.push_backwards_jump_op(loop_jump_location);
                self.patch_jump_op(exit_jump_to_patch);
            }



            Expr::Void => { }

            Expr::Index { left, index } => {
                self.compile_expression(left);
                self.compile_expression(index);
                self.push_op(OpCode::ArrIndex);
            }

            Expr::Match { match_value, arms } => {
                self.compile_expression(match_value);
                for MatchArm { pattern, extra_condition, body } in arms {

                }    
            }

            _ => { panic!("{expr:?} not yet implemented") }
        }
    }

    fn push_op(&mut self, op: OpCode) { self.bytecode.codes.push(op as u8); }
    fn push_ops<I>(&mut self, ops: I) where I: IntoIterator<Item = OpCode> {
        self.bytecode.codes.extend(ops.into_iter().map(|op| op as u8));
    }
    fn push_operand(&mut self, operand: usize) {
        if operand < u8::MAX as usize {
            self.bytecode.codes.push(operand as u8);
        }
        else {
            self.bytecode.codes.push(u8::MAX);
            self.bytecode.codes.extend(operand.to_ne_bytes());
        }
    }
    fn push_op_with_operand(&mut self, op: OpCode, operand: usize) {
        self.push_op(op);
        self.push_operand(operand);
    }
    fn push_op_with_operands(&mut self, op: OpCode, operands: &[usize]) {
        self.push_op(op);
        for &operand in operands { self.push_operand(operand); }
    }

    fn push_op_for_patching(&mut self, op: OpCode) -> usize {
        self.bytecode.codes.push(op as u8);
        self.bytecode.codes.push(u8::MAX);
        let patch_location = self.bytecode.codes.len();
        self.bytecode.codes.extend((usize::MAX).to_ne_bytes());
        patch_location
    }

    fn patch_jump_op(&mut self, to_patch_location: usize) {
        // The jump should be relative to the instruction after the jump itself.
        let jump_instruction_end = to_patch_location + std::mem::size_of::<usize>();
        let jump_target = self.bytecode.codes.len();
        
        let offset = jump_target - jump_instruction_end;
        let offset_bytes = offset.to_ne_bytes();
        
        let placeholder_slice = &mut self.bytecode.codes[to_patch_location..(to_patch_location + std::mem::size_of::<usize>())];
        placeholder_slice.copy_from_slice(&offset_bytes);
    }

    fn push_backwards_jump_op(&mut self, jump_location: usize) {
        self.push_op_with_operand(OpCode::JumpBack, self.bytecode.codes.len() - (jump_location - 2));
    }


    fn push_get_constant_op(&mut self, val: Value) {
        // let index = match self.bytecode.constants.iter().position(|constant| val == constant.clone()) {
        //     Some(i) => i,
        //     None => {
        //         self.bytecode.constants.push(val);
        //         self.bytecode.constants.len() - 1
        //     }
        // };
        self.bytecode.constants.push(val);
        let index = self.bytecode.constants.len() - 1;
        self.push_op_with_operand(OpCode::GetConstant, index);
    }

    fn enter_scope(&mut self) { self.curr_scope_depth += 1; }
    fn exit_scope(&mut self, should_drop_value: bool) {
        self.curr_scope_depth -= 1;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.curr_scope_depth {
            self.locals.pop();
            self.push_op(if should_drop_value { OpCode::PopValue } else { OpCode::DropValue });
        }
    }

    fn define_local(&mut self, name: String) {
        self.locals.push(Local { name, depth: self.curr_scope_depth });
    }
    fn push_get_local_op(&mut self, name: &str) {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                self.push_op_with_operand(OpCode::GetLocal, i);
                return;
            }
        }
        unreachable!("could not find {name} in the locals vec...")
    }
    fn push_set_local_op(&mut self, name: &str) {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                self.push_op_with_operand(OpCode::SetLocal, i);
                return;
            }
        }
        unreachable!("could not find {name} in the locals vec...")
    }






    fn compile_block_expression(&mut self, body: &[TypedExpr]) {
        if let Some((last_expr, preceding_exprs)) = body.split_last() {
            self.enter_scope();

            for expr in preceding_exprs {
                self.compile_expression(expr);
                // if it is not the last expression of the block, the return value is always useless, so pop it.
                if expr.typ != TypeKind::Void {
                    self.push_op(OpCode::PopValue);
                }
            }
            self.compile_expression(last_expr);

            let should_drop_value = last_expr.typ == TypeKind::Void;
            self.exit_scope(should_drop_value);
        }
    }

    fn compile_binding_pattern(&mut self, pattern: &AssignablePattern) {
        match pattern {
            // the value is already on the stack, just the compiler just needs a variable that points to it.
            AssignablePattern::Binding { name, .. } => self.define_local(name.clone()),
            AssignablePattern::Wildcard => self.push_op(OpCode::PopValue),

            AssignablePattern::Array(patterns) => {
                // 1. define the array as a local
                let arr_local_index = self.locals.len();
                self.define_local(format!("${}", arr_local_index));

                // 2. for each pattern pop the last value of the local array.
                for (i, pattern) in patterns.iter().enumerate() {
                    self.push_op_with_operands(OpCode::GetArrIndex, &[arr_local_index, i]);
                    self.compile_binding_pattern(pattern);
                }
            }
            AssignablePattern::Tuple(patterns) => {
                let tup_local_index = self.locals.len();
                self.define_local(format!("${}", tup_local_index));
                for (i, pattern) in patterns.iter().enumerate() {
                    self.push_op_with_operands(OpCode::GetTupIndex, &[tup_local_index, i]);
                    self.compile_binding_pattern(pattern);
                }
            }

            AssignablePattern::Place(PlaceExpr::Identifier(name)) => self.push_set_local_op(name),
            _ => panic!("not implemented")
        }
    }

    fn push_place_expr_value(&mut self, place_expr: PlaceExpr) {
        match place_expr {
            PlaceExpr::Identifier(name) => self.push_get_local_op(&name),
            _ => todo!()
        }
    }


    fn compile_infix(&mut self, left: &TypeKind, operator: &TokenType, right: &TypeKind) {
        match (left, right) {
            (TypeKind::Num, TypeKind::Num) => match operator {
                TokenType::Plus => self.push_op(OpCode::AddNum),
                TokenType::Minus => self.push_op(OpCode::SubtractNum),
                TokenType::Star => self.push_op(OpCode::MultiplyNum),
                TokenType::Slash => self.push_op(OpCode::DivideNum),
                TokenType::Percent => self.push_op(OpCode::ModuloNum),
                TokenType::StarStar => self.push_op(OpCode::ExponentNum),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::NegateBool]),
                TokenType::Less => self.push_op(OpCode::CmpLess),
                TokenType::Greater => self.push_op(OpCode::CmpGreater),
                TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::NegateBool]),
                TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::NegateBool]),
                _ => unreachable!("Unsupported operator {} for type num", operator)
            },
            (TypeKind::Str, TypeKind::Str) => match operator {
                TokenType::Plus => self.push_op(OpCode::AddStr),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::NegateBool]),
                TokenType::Less => self.push_op(OpCode::CmpLess),
                TokenType::Greater => self.push_op(OpCode::CmpGreater),
                TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::NegateBool]),
                TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::NegateBool]),
                _ => unreachable!("Unsupported operator {} for type str", operator)
            }
            (TypeKind::Bool, TypeKind::Bool) => match operator {
                TokenType::Ampersand => self.push_op(OpCode::AndBool),
                TokenType::Pipe => self.push_op(OpCode::OrBool),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::NegateBool]),
                _ => unreachable!("Unsupported operator {} for type bool", operator)
            }
            (TypeKind::Arr(_), TypeKind::Arr(_)) => match operator {
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::NegateBool]),
                _ => unreachable!("Unsupported operator {} for type arr", operator)
            }
            (TypeKind::Tup(_), TypeKind::Tup(_)) => match operator {
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::NegateBool]),
                _ => unreachable!("Unsupported operator {} for type tup", operator)
            }
            (left, right) => unreachable!("Mismatched types for infix operation: ({left}, {right})")
        }
    }

    fn compile_prefix(&mut self, operator: &TokenType, right: &TypeKind) {
        match right {
            TypeKind::Bool => match operator {
                TokenType::Not => self.push_op(OpCode::NegateBool),
                _ => unreachable!("Unsupported operator {} for type bool", operator)
            }
            TypeKind::Num => match operator {
                TokenType::Minus => self.push_op(OpCode::NegateNum),
                _ => unreachable!("Unsupported operator {} for type num", operator)
            }
            _ => unreachable!("Mismatched types for prefix operation")
        }
    }
}