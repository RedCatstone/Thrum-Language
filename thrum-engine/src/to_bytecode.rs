use std::{u8, usize};

use num_enum::TryFromPrimitive;

use crate::{ast_structure::{AssignablePattern, Expr, MatchArm, PlaceExpr, TypeKind, TypedExpr, Value}, tokens::TokenType};


#[repr(u8)]
#[derive(Debug, TryFromPrimitive)]
pub enum OpCode {
    ConstGet,  // ConstantIndex

    // operations on the locals part of the stack
    LocalGet, // LocalIndex
    LocalSet, // LocalIndex
    LocalsFree, // LocalIndex AmountToFree

    ValueRefSet,  // set value to a pointer location
    FollowPointer,
    
    // operations on the temp part of the stack
    ValuePop,
    ValueDrop,
    ValueDup,

    // operators / types
    CmpEqual, CmpLess, CmpGreater,
    NumAdd, NumSubtract, NumMultiply, NumDivide, NumModulo, NumExponent, NumNegate,
    BoolNegate, BoolAnd, BoolOr,
    StrAdd, StrTemplate,
    ArrCreate, ArrGet, ArrRefSet, ArrUnpackCheckJump,
    TupCreate, TupUnpack,

    // control flow
    Jump, JumpBack, JumpIfFalse,


    // end of function / program
    ReturnVoid, Return,

    Panic,
}

#[derive(Default, Debug, Clone)]  // Clone is only for printing
pub struct BytecodeChunk {
    pub name: String,
    pub codes: Vec<u8>,
    pub constants: Vec<Value>,
    pub local_slots_needed: usize,
}


#[derive(Default)]
pub struct CompileFunction {
    // bytecodes[0] is what this CompileFunction writes to.
    // the others are extra functions compiled by other CompileFunction's.
    pub bytecodes: Vec<BytecodeChunk>,
    curr_scope_depth: usize,
    locals: Vec<Local>,
}
pub struct Local {
    name: String,
    scope_depth: usize,
}

impl CompileFunction {
    pub fn compile_function(program: &[TypedExpr], name: String) -> Vec<BytecodeChunk> {
        let mut compile_function = CompileFunction::default();
        compile_function.bytecodes.push(BytecodeChunk { name, ..Default::default() });

        compile_function.compile_block_expression(program);
        if program.last().expect("empty program").typ == TypeKind::Void {
            compile_function.push_op(OpCode::ReturnVoid);
        }
        else { compile_function.push_op(OpCode::Return); }
        
        
        compile_function.bytecodes
    }

    fn compile_expression(&mut self, expr: &TypedExpr) {
        match &expr.expression {
            Expr::Literal(val) => self.push_get_constant_op(val.clone()),
            Expr::TemplateString(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::StrTemplate, elements.len());
            }
            Expr::Array(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::ArrCreate, elements.len());
            }
            Expr::Tuple(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::TupCreate, elements.len());
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

            Expr::Let { pattern, value, alternative } => {
                self.compile_expression(value);

                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(pattern, &mut failure_jumps);

                // if AssignablePattern can fail, use the else block or panic
                if !failure_jumps.is_empty() {
                    self.push_op(OpCode::Jump);
                    let success_jump = self.push_opnum_for_patching();

                    for jump in failure_jumps {
                        self.patch_jump_op(jump);
                    }
                    if let Some(alt) = alternative {
                        self.compile_expression(alt);
                    }
                    else {
                        self.push_get_constant_op(Value::Str("Let assignment-pattern did not match.".to_string()));
                        self.push_op(OpCode::Panic);
                    }

                    self.patch_jump_op(success_jump);
                }
            }
            Expr::Identifier { name } => {
                self.push_get_local(name);
            }

            Expr::Assign { left, extra_operator, right } => {
                if *extra_operator == TokenType::Equal {
                    self.compile_expression(right);
                }
                else {
                    match *left.clone() {
                        AssignablePattern::Place(place) => {
                            self.push_place_expr_value(place);
                            self.compile_expression(right);
                            self.compile_infix(&right.typ, extra_operator, &right.typ);
                        }
                        _ => unreachable!("Infix assignments are only allowed for place patterns.")
                    }
                }
                
                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(&left, &mut failure_jumps);
            }

            Expr::MutRef { expr } => {
                match &expr.expression {
                    Expr::Identifier { name } => {
                        self.push_get_local_mut(&name);
                    }
                    _ => todo!()
                }
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
                self.push_op(OpCode::JumpIfFalse);
                let jump_true_to_patch = self.push_opnum_for_patching();

                self.compile_expression(consequence);
                self.push_op(OpCode::Jump);
                let jump_false_to_patch = if alternative.is_some() { self.push_opnum_for_patching() }
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
                let loop_jump_location = self.bytecodes[0].codes.len();
                self.compile_expression(condition);
                self.push_op(OpCode::JumpIfFalse);
                let exit_jump_to_patch = self.push_opnum_for_patching();
                self.compile_expression(body);
                self.push_backwards_jump_op(loop_jump_location);
                self.patch_jump_op(exit_jump_to_patch);
            }



            Expr::Void => { }

            Expr::Index { left, index } => {
                self.compile_expression(left);
                self.compile_expression(index);
                self.push_op(OpCode::ArrGet);
            }


            Expr::IfLet { pattern, value, consequence, alternative } => {
                self.enter_scope();
                
                self.compile_expression(value);

                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(pattern, &mut failure_jumps);

                // this part is only reached if it matched
                self.compile_expression(consequence);
                let cleanup_code = self.exit_scope();
                self.push_codes(&cleanup_code);

                if let Some(alt) = alternative {
                    self.push_op(OpCode::Jump);
                    let success_jump = self.push_opnum_for_patching();
                    
                    for jump in failure_jumps {
                        self.patch_jump_op(jump);
                    }
                    self.push_codes(&cleanup_code);
                    self.compile_expression(alt);

                    self.patch_jump_op(success_jump);
                }
                else {
                    for jump in failure_jumps {
                        self.patch_jump_op(jump);
                    }
                }
            }


            Expr::Match { match_value, arms } => {
                self.compile_expression(match_value);
                
                let mut success_jumps = Vec::new();

                for (i, MatchArm { pattern, body }) in arms.iter().enumerate() {
                    self.enter_scope();
                    let is_last_arm = i == arms.len() - 1;

                    // if not the last arm, duplicate the to match value
                    if !is_last_arm { self.push_op(OpCode::ValueDup); }

                    let mut failure_jumps = Vec::new();
                    self.compile_binding_pattern(pattern, &mut failure_jumps);

                    // this part is only reached if the pattern matched -> pop the duplicated patternmatch value
                    if !is_last_arm { self.push_op(OpCode::ValuePop); }
                    self.compile_expression(body);
                    
                    // last success jump / failure jumps aren't neccessary, its already at the end
                    if !is_last_arm {
                        self.push_op(OpCode::Jump);
                        success_jumps.push(self.push_opnum_for_patching());
                        
                        // all failure jumps from this pattern match attempt should point towards the next arm
                        for jump in failure_jumps {
                            self.patch_jump_op(jump);
                        }
                    }
                    self.exit_scope();
                }

                // end of match statement, point all the success jumps here
                for jump in success_jumps {
                    self.patch_jump_op(jump);
                }
            }

            _ => { panic!("{expr:?} not yet implemented") }
        }
    }

    fn push_op(&mut self, op: OpCode) { self.bytecodes[0].codes.push(op as u8); }
    fn push_ops<I>(&mut self, ops: I) where I: IntoIterator<Item = OpCode> {
        self.bytecodes[0].codes.extend(ops.into_iter().map(|op| op as u8));
    }
    fn push_codes(&mut self, codes: &[u8]) { self.bytecodes[0].codes.extend(codes); }
    fn push_opnum(&mut self, opnum: usize) {
        if opnum < u8::MAX as usize {
            self.bytecodes[0].codes.push(opnum as u8);
        }
        else {
            self.bytecodes[0].codes.push(u8::MAX);
            self.bytecodes[0].codes.extend(opnum.to_ne_bytes());
        }
    }
    fn push_op_with_opnum(&mut self, op: OpCode, opnum: usize) {
        self.push_op(op);
        self.push_opnum(opnum);
    }
    fn push_op_with_opnums(&mut self, op: OpCode, opnums: &[usize]) {
        self.push_op(op);
        for &opnum in opnums { self.push_opnum(opnum); }
    }

    fn push_opnum_for_patching(&mut self) -> usize {
        // e.g. Jump, but we dont know where to jump to yet
        let patch_location = self.bytecodes[0].codes.len() + 1;
        self.push_opnum(usize::MAX);
        patch_location
    }

    fn patch_jump_op(&mut self, to_patch_location: usize) {
        // The jump should be relative to the instruction after the jump itself.
        let jump_instruction_end = to_patch_location + std::mem::size_of::<usize>();
        let jump_target = self.bytecodes[0].codes.len();
        
        let offset = jump_target - jump_instruction_end;
        let offset_bytes = offset.to_ne_bytes();
        
        let placeholder_slice = &mut self.bytecodes[0].codes[to_patch_location..(to_patch_location + std::mem::size_of::<usize>())];
        placeholder_slice.copy_from_slice(&offset_bytes);
    }

    fn push_backwards_jump_op(&mut self, jump_location: usize) {
        self.push_op_with_opnum(OpCode::JumpBack, self.bytecodes[0].codes.len() - (jump_location - 2));
    }


    fn push_get_constant_op(&mut self, val: Value) {
        // let index = match self.bytecode.constants.iter().position(|constant| val == constant.clone()) {
        //     Some(i) => i,
        //     None => {
        //         self.bytecode.constants.push(val);
        //         self.bytecode.constants.len() - 1
        //     }
        // };
        let index = self.bytecodes[0].constants.len();
        self.bytecodes[0].constants.push(val);
        self.push_op_with_opnum(OpCode::ConstGet, index);
    }

    fn enter_scope(&mut self) { self.curr_scope_depth += 1; }
    fn exit_scope(&mut self) -> Vec<u8> {
        self.curr_scope_depth -= 1;

        // update the max locals needed variable
        let before_cleanup = self.locals.len();
        if before_cleanup > self.bytecodes[0].local_slots_needed { self.bytecodes[0].local_slots_needed = before_cleanup }

        // trash locals that are now out of scope
        while !self.locals.is_empty() && self.locals.last().unwrap().scope_depth > self.curr_scope_depth {
            self.locals.pop();
        }

        vec![
            OpCode::LocalsFree as u8,
            self.locals.len() as u8,
            (before_cleanup - self.locals.len()) as u8,
        ]
    }

    fn push_define_local(&mut self, name: String) {
        self.push_op_with_opnum(OpCode::LocalSet, self.locals.len());
        self.locals.push(Local { name, scope_depth: self.curr_scope_depth });
    }
    fn get_local_index(&mut self, name: &str) -> usize {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return i;
            }
        }
        unreachable!("could not find {name} in the locals vec...")
    }
    fn push_get_local(&mut self, name: &str) {
        let i = self.get_local_index(name);
        self.push_op_with_opnum(OpCode::LocalGet, i);
    }
    fn push_set_local(&mut self, name: &str) {
        let i = self.get_local_index(name);
        self.push_op_with_opnum(OpCode::LocalSet, i);
    }
    fn push_get_local_mut(&mut self, name: &str) {
        let i = self.get_local_index(name);
        self.push_get_constant_op(Value::ValueStackPointer(i));
    }





    fn compile_block_expression(&mut self, body: &[TypedExpr]) {
        if let Some((last_expr, preceding_exprs)) = body.split_last() {
            self.enter_scope();

            for expr in preceding_exprs {
                self.compile_expression(expr);
                // if it is not the last expression of the block, the return value is always useless, so pop it.
                if expr.typ != TypeKind::Void {
                    self.push_op(OpCode::ValuePop);
                }
            }
            self.compile_expression(last_expr);
            self.exit_scope();
        }
    }






    fn compile_binding_pattern(&mut self, pattern: &AssignablePattern, failure_jumps: &mut Vec<usize>) {
        match pattern {
            // the value is already on the stack, just the compiler just needs a variable that points to it.
            AssignablePattern::Binding { name, .. } => self.push_define_local(name.clone()),
            AssignablePattern::Wildcard => self.push_op(OpCode::ValuePop),

            AssignablePattern::Tuple(patterns) => {
                self.push_op(OpCode::TupUnpack);
                for pattern in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }
            AssignablePattern::Place(PlaceExpr::Identifier(name)) => self.push_set_local(name),

            // all patterns below this point can fail.
            AssignablePattern::Literal(lit) => {
                self.push_get_constant_op(lit.clone());
                self.push_op(OpCode::CmpEqual);
                self.push_op(OpCode::JumpIfFalse);
                failure_jumps.push(self.push_opnum_for_patching());
            }
            AssignablePattern::Array(patterns) => {
                self.push_op_with_opnum(OpCode::ArrUnpackCheckJump, patterns.len());
                failure_jumps.push(self.push_opnum_for_patching());

                for pattern in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }

            AssignablePattern::Conditional { pattern, body } => {
                self.compile_binding_pattern(pattern, failure_jumps);
                self.compile_expression(body);
                self.push_op(OpCode::JumpIfFalse);
                failure_jumps.push(self.push_opnum_for_patching());
            }

            AssignablePattern::Place(PlaceExpr::Index { left, index }) => {
                self.compile_expression(index);
                match &left.expression {
                    Expr::Identifier { name } => {
                        self.push_get_local_mut(name);
                    }
                    _ => todo!()
                }
                self.push_op(OpCode::ArrRefSet);
            }

            AssignablePattern::Place(PlaceExpr::Deref(name)) => {
                self.push_get_local(name);
                self.push_op(OpCode::ValueRefSet);
            }
            _ => panic!("not implemented")
        }
    }




    fn push_place_expr_value(&mut self, place_expr: PlaceExpr) {
        match place_expr {
            PlaceExpr::Identifier(name) => self.push_get_local(&name),
            PlaceExpr::Index { left, index } => {
                self.compile_expression(&left);
                self.compile_expression(&index);
                self.push_op(OpCode::ArrGet);
            }
            PlaceExpr::Deref(name) => {
                self.push_get_local(&name);
                self.push_op(OpCode::FollowPointer);
            }
        }
    }







    fn compile_infix(&mut self, left: &TypeKind, operator: &TokenType, right: &TypeKind) {
        match (left, right) {
            (TypeKind::Num, TypeKind::Num) => match operator {
                TokenType::Plus => self.push_op(OpCode::NumAdd),
                TokenType::Minus => self.push_op(OpCode::NumSubtract),
                TokenType::Star => self.push_op(OpCode::NumMultiply),
                TokenType::Slash => self.push_op(OpCode::NumDivide),
                TokenType::Percent => self.push_op(OpCode::NumModulo),
                TokenType::StarStar => self.push_op(OpCode::NumExponent),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate]),
                TokenType::Less => self.push_op(OpCode::CmpLess),
                TokenType::Greater => self.push_op(OpCode::CmpGreater),
                TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::BoolNegate]),
                TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::BoolNegate]),
                _ => unreachable!("Unsupported operator {} for type num", operator)
            },
            (TypeKind::Str, TypeKind::Str) => match operator {
                TokenType::Plus => self.push_op(OpCode::StrAdd),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate]),
                TokenType::Less => self.push_op(OpCode::CmpLess),
                TokenType::Greater => self.push_op(OpCode::CmpGreater),
                TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::BoolNegate]),
                TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::BoolNegate]),
                _ => unreachable!("Unsupported operator {} for type str", operator)
            }
            (TypeKind::Bool, TypeKind::Bool) => match operator {
                TokenType::Ampersand => self.push_op(OpCode::BoolAnd),
                TokenType::Pipe => self.push_op(OpCode::BoolOr),
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate]),
                _ => unreachable!("Unsupported operator {} for type bool", operator)
            }
            (TypeKind::Arr(_), TypeKind::Arr(_)) => match operator {
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate]),
                _ => unreachable!("Unsupported operator {} for type arr", operator)
            }
            (TypeKind::Tup(_), TypeKind::Tup(_)) => match operator {
                TokenType::EqualEqual => self.push_op(OpCode::CmpEqual),
                TokenType::NotEqual => self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate]),
                _ => unreachable!("Unsupported operator {} for type tup", operator)
            }
            (left, right) => unreachable!("Mismatched types for infix operation: ({left}, {right})")
        }
    }

    fn compile_prefix(&mut self, operator: &TokenType, right: &TypeKind) {
        match right {
            TypeKind::Bool => match operator {
                TokenType::Exclamation => self.push_op(OpCode::BoolNegate),
                _ => unreachable!("Unsupported operator {} for type bool", operator)
            }
            TypeKind::Num => match operator {
                TokenType::Minus => self.push_op(OpCode::NumNegate),
                _ => unreachable!("Unsupported operator {} for type num", operator)
            }
            _ => unreachable!("Mismatched types for prefix operation")
        }
    }
}