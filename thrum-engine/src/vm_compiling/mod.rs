use std::{u8, usize};

use num_enum::TryFromPrimitive;

use crate::{lexing::tokens::TokenType, nativelib::{ThrumModule, get_native_lib}, parsing::ast_structure::{AssignablePattern, Expr, MatchArm, PlaceExpr, TypeKind, TypedExpr, Value}};


#[repr(u8)]
#[derive(Debug, TryFromPrimitive)]
pub enum OpCode {
    ConstGet,  // ConstantIndex
    PushVoid,

    // operations on the locals part of the stack
    LocalGet, // LocalIndex
    LocalSet, // LocalIndex
    LocalsFree, // LocalIndex AmountToFree

    MakePointer, // LocalIndex
    ValueRefSet,  // set value to a pointer location
    FollowPointer,
    
    // operations on the temp part of the stack
    ValuePop,
    ValueDup,

    // operators / types
    CmpEqual, CmpLess, CmpGreater,
    NumAdd, NumSubtract, NumMultiply, NumDivide, NumModulo, NumExponent, NumNegate,
    BoolNegate,
    StrAdd, StrTemplate,
    ArrCreate, ArrGet, ArrRefSet, ArrUnpackCheckJump,
    TupCreate, TupUnpack,

    // control flow
    Jump, JumpBack, JumpIfFalse,

    CallFn, // arg count


    // end of function / program
    Return,

    Panic,
}

#[derive(Default, Debug)]
pub struct BytecodeChunk {
    pub name: String,
    pub codes: Vec<u8>,
    pub constants: Vec<Value>,
    pub local_slots_needed: usize,
}





pub struct Compiler {
}

impl Compiler {
    pub fn compile_program(program: &[TypedExpr]) -> Vec<BytecodeChunk> {
        let mut bytecode_chunks = Vec::new();

        let library = get_native_lib();
        let mut locals = vec![Vec::new()];
        Self::load_prelude_into_locals_vec(&library, &mut locals[0]);

        CompileFunction::compile_function(
            "<main>".to_string(),
            program,
            &[],
            &mut bytecode_chunks,
            &mut locals,
            &library,
        );
        bytecode_chunks
    }

    fn load_prelude_into_locals_vec(module: &ThrumModule, locals: &mut Vec<Local>) {
        for (name, value) in &module.values {
            if value.is_prelude {
                locals.push(Local { name: name.clone(), scope_depth: 0, const_value: Some(value.val.clone()) });
            }
        }
        // Recursion
        for sub_module in module.sub_modules.values() {
            Self::load_prelude_into_locals_vec(sub_module, locals);
        }
    }
}





struct FailureJump {
    temps: usize,
    jump_loc: usize,
}





struct CompileFunction<'a> {
    // bytecodes[curr_bytecode_index] is what this CompileFunction writes to.
    // the others are from other functions being compiled by other CompileFunction's.
    bytecode_chunks: &'a mut Vec<BytecodeChunk>,
    curr_bytecode_index: usize,

    // locals: vec![localvec_from_previous_compile_function, this_locals_vec]
    // if this CompileFunction needs to push/pop locals, its local vec is always .last()
    locals: &'a mut Vec<Vec<Local>>,
    curr_scope_depth: usize,

    library: &'a ThrumModule,
    
    // break / continue
    loop_jumps: Vec<Vec<usize>>,
    // compiling `let x = 2 + 2` first has 2 temps (2, 2) then goes to 1 temp (4) then goes to 0.
    cur_temp_amount: usize,
}
struct Local {
    name: String,
    scope_depth: usize,
    const_value: Option<Value>,
}

impl<'a> CompileFunction<'a> {
    fn compile_function(
        name: String,
        program: &[TypedExpr],
        params: &[AssignablePattern],
        bytecode_chunks: &mut Vec<BytecodeChunk>,
        locals: &mut Vec<Vec<Local>>,
        library: &ThrumModule,
    ) -> usize
        {
        let mut compile_function = CompileFunction {
            curr_bytecode_index: bytecode_chunks.len(),
            bytecode_chunks,
            locals,
            curr_scope_depth: 0,
            library,

            cur_temp_amount: params.len(),
            loop_jumps: Vec::new(),
        };
        // push the new bytecode_chunk and localsvec for this function
        compile_function.bytecode_chunks.push(BytecodeChunk { name, ..Default::default() });
        compile_function.locals.push(Vec::new());

        // compile the params first
        for param in params.iter().rev() {
            compile_function.compile_binding_pattern(param, &mut Vec::new());
        }

        // then compile the body!!
        compile_function.compile_block_expression(program);

        // finally, return
        compile_function.push_op(OpCode::Return);
        compile_function.locals.pop();
        compile_function.curr_bytecode_index
    }

    fn compile_expression(&mut self, expr: &TypedExpr) {
        let start_temps = self.cur_temp_amount;

        match &expr.expression {
            Expr::Literal(val) => {
                self.push_get_constant_op(val.clone());
            }
            Expr::TemplateString(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::StrTemplate, elements.len());
                self.cur_temp_amount -= elements.len() - 1;
            }
            Expr::Array(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::ArrCreate, elements.len());
                self.cur_temp_amount -= elements.len() - 1;
            }
            Expr::Tuple(elements) => {
                for element in elements { self.compile_expression(element); }
                self.push_op_with_opnum(OpCode::TupCreate, elements.len());
                self.cur_temp_amount -= elements.len() - 1;
            }


            Expr::Infix { left, operator, right } => {
                self.compile_expression(left);
                self.compile_infix(operator, &left.typ, right);
            }
            Expr::Prefix { operator, right } => {
                self.compile_expression(right);
                self.compile_prefix(operator, &right.typ)
            }

            Expr::Block(body) => self.compile_block_expression(body),

            Expr::Identifier { name } => {
                self.push_get_identifier(name);
            }

            Expr::Assign { pattern, extra_operator, value } => {
                // TODO
                let val = value.as_ref().unwrap();

                // push value to the stack
                if *extra_operator == TokenType::Equal {
                    self.compile_expression(val);
                }
                else {
                    match *pattern.clone() {
                        AssignablePattern::Place(place) => {
                            self.push_place_expr_value(place);
                            self.compile_infix(extra_operator, &val.typ, val);
                        }
                        _ => unreachable!("Infix assignments are only allowed for place patterns.")
                    }
                }
                // compile the binding pattern
                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(&pattern, &mut failure_jumps);

                // if AssignablePattern can fail, use the else block or panic
                if !failure_jumps.is_empty() {
                    unreachable!("assign pattern didn't match.")
                    // self.push_op(OpCode::Jump);
                    // let success_jump = self.push_opnum_for_patching();

                    // self.compile_binding_pattern_failure_jumps(&mut failure_jumps);
                    
                    // if let Some(alt) = alternative {
                    //     self.compile_expression(alt);
                    // }
                    // else {
                    //     self.push_get_constant_op(Value::Str("Assignment-pattern did not match.".to_string()));
                    //     self.push_op(OpCode::Panic);
                    // }

                    // self.patch_jump_op(success_jump);
                }

                self.push_void();
            }

            Expr::Case { pattern, value } => {
                self.compile_expression(value);  // (+1 temp)

                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(&pattern, &mut failure_jumps);  // consumes value (-1 temp)

                // its a match! 
                self.push_get_constant_op(Value::Bool(true)); // (+1 temp - success path)
                self.cur_temp_amount -= 1;

                // handle failure path, skip to else block
                if !failure_jumps.is_empty() {
                    self.push_op(OpCode::Jump);
                    let jump_over_failure = self.push_opnum_for_patching();

                    self.compile_binding_pattern_failure_jumps(&mut failure_jumps);
                    
                    self.push_get_constant_op(Value::Bool(false)); // (+1 temp - failure path)
                    self.cur_temp_amount -= 1;
                    
                    self.patch_jump_op(jump_over_failure);
                }
                self.cur_temp_amount += 1;
            }

            Expr::MutRef { expr } => {
                match &expr.expression {
                    Expr::Identifier { name } => {
                        self.push_get_local_mut(&name);
                    }
                    _ => todo!()
                }
            }

            Expr::Deref { expr } => {
                match &expr.expression {
                    Expr::Identifier { name } => {
                        self.push_get_identifier(name);
                        self.push_op(OpCode::FollowPointer);
                    }
                    _ => todo!()
                }
            }


            Expr::If { condition, consequence, alternative } => {
                // what it should look like:
                // ...condition...
                // JumpIfFalse 2
                //     ...if block...
                // Jump 1
                //     ...else block...
                self.compile_expression(condition);
                self.push_jump_if_false();
                let jump_to_else_block = self.push_opnum_for_patching();

                self.compile_expression(consequence);
                self.push_op(OpCode::Jump);
                let jump_over_else_block = self.push_opnum_for_patching();

                self.patch_jump_op(jump_to_else_block);

                // while processing the else block, the value from the if block should be ignored
                self.cur_temp_amount -= 1;

                self.compile_expression(alternative);
                self.patch_jump_op(jump_over_else_block);
            }

            Expr::Loop { body } => {
                // what it should look like:
                // ...loop...
                // ...loop...
                // Jump -3
                let loop_jump_location = self.bytecode_chunks[self.curr_bytecode_index].codes.len();

                self.loop_jumps.push(Vec::new());
                self.compile_expression(body);
                self.push_pop_value();
                self.push_backwards_jump_op(loop_jump_location);

                let break_jumps = self.loop_jumps.pop().unwrap();

                for jump in break_jumps {
                    self.patch_jump_op(jump);
                }

                // doesnt need to push void, because the only way to get out of the loop are the break jumps
                self.cur_temp_amount += 1;
            }

            Expr::Break { expr } => {
                let temp_pop_amount = self.cur_temp_amount;

                // pop all temp values
                for _ in 0..temp_pop_amount {
                    self.push_pop_value();
                }
                // then compile the break expression
                self.compile_expression(expr);

                // and then actually break
                self.push_op(OpCode::Jump);
                let break_jump = self.push_opnum_for_patching();
                self.loop_jumps.last_mut().unwrap().push(break_jump);

                self.cur_temp_amount += temp_pop_amount;
            }



            Expr::Void => {
                self.push_void();
            }

            Expr::Index { left, index } => {
                self.compile_expression(left);
                self.compile_expression(index);
                self.push_op(OpCode::ArrGet);
            }


            Expr::Match { match_value, arms } => {
                self.compile_expression(match_value);
                
                let mut jumps_to_end_of_match = Vec::new();

                for (i, MatchArm { pattern, body }) in arms.iter().enumerate() {
                    self.enter_scope();
                    let is_last_arm = i == arms.len() - 1;

                    // if not the last arm, duplicate the to match value
                    if !is_last_arm { self.push_dup_value(); }

                    let mut failure_jumps = Vec::new();
                    self.compile_binding_pattern(pattern, &mut failure_jumps);

                    // this part is only reached if the pattern matched -> pop original to match value
                    if !is_last_arm { self.push_pop_value(); }
                    self.compile_expression(body);
                    
                    // last success jump / failure jumps aren't neccessary, its already at the end
                    if !is_last_arm {
                        self.push_op(OpCode::Jump);
                        jumps_to_end_of_match.push(self.push_opnum_for_patching());
                        
                        // all failure jumps from this pattern match attempt should point towards the next arm
                        self.compile_binding_pattern_failure_jumps(&mut failure_jumps);
                    }
                    self.exit_scope();
                }

                // end of match statement, point all the success jumps here
                for jump in jumps_to_end_of_match {
                    self.patch_jump_op(jump);
                }
            }

            Expr::Call { callee, arguments } => {
                for argument in arguments {
                    self.compile_expression(argument);
                }
                self.compile_expression(&callee);
                self.push_op_with_opnum(OpCode::CallFn, arguments.len());

                self.cur_temp_amount -= arguments.len();
            }

            Expr::FnDefinition { .. } => {
                // already handled in compile_block_expression
                self.push_void();
            }

            Expr::Closure { params, body, .. } => {
                let fn_index = CompileFunction::compile_function(
                    "<closure>".to_string(),
                    std::slice::from_ref(body),
                    &params,
                    self.bytecode_chunks,
                    self.locals,
                    self.library
                );
                self.push_get_constant_op(Value::Closure { chunk_index: fn_index });
            }

            Expr::Return(expr) => {
                let temp_pop_amount = self.cur_temp_amount;

                // pop all temp values
                for _ in 0..temp_pop_amount {
                    self.push_pop_value();
                }
                // then compile the break expression
                self.compile_expression(expr);
                self.push_op(OpCode::Return);

                self.cur_temp_amount += temp_pop_amount;
            }


            Expr::TypePath(segments) => {
                let mut curr_module = self.library;

                for (i, segment) in segments.iter().enumerate() {
                    // try to descend into a sub module
                    if let Some(sub_module) = curr_module.sub_modules.get(segment) {
                        curr_module = sub_module;
                        continue;
                    }
        
                    // else check for types (e.g. str::len)
                    if let Some(module_type) = curr_module.types.get(segment) {
                        let remaining_segments = &segments[(i + 1)..];
                        match remaining_segments {
                            // 0 remaining segments -> type
                            [] => unreachable!("Cannot use a type ('{}') as a value.", segment),
                            // 1 remaining, meaning its a value defined on that type.
                            [last_segment] => {
                                if let Some(type_val) = module_type.values.get(last_segment) {
                                    self.push_get_constant_op(type_val.val.clone());
                                    break;
                                }
                            }
                            // 2 or more remaining segments
                            _ => unreachable!("type path had 2 or more remaining segments.")
                        }
                    }
        
                    // else check for consts/functions (e.g. io::print)
                    if let Some(module_val) = curr_module.values.get(segment) {
                        if i == segments.len() - 1 {
                            self.push_get_constant_op(module_val.val.clone());
                            break;
                        }
                        else {
                            unreachable!("value path too long.")
                        }
                    }
        
                    unreachable!("segment {segment} could not be found...");
                }
            }

            _ => { panic!("{expr:?} not yet implemented") }
        }

        if start_temps + 1 != self.cur_temp_amount {
            panic!("wrong temp number ({} -> {}) after processing {:?}", start_temps, self.cur_temp_amount, expr);
        }
    }






    fn push_op(&mut self, op: OpCode) { self.bytecode_chunks[self.curr_bytecode_index].codes.push(op as u8); }
    fn push_ops<I>(&mut self, ops: I) where I: IntoIterator<Item = OpCode> {
        self.bytecode_chunks[self.curr_bytecode_index].codes.extend(ops.into_iter().map(|op| op as u8));
    }
    fn push_codes(&mut self, codes: &[u8]) { self.bytecode_chunks[self.curr_bytecode_index].codes.extend(codes); }
    fn push_opnum(&mut self, opnum: usize) {
        if opnum < u8::MAX as usize {
            self.bytecode_chunks[self.curr_bytecode_index].codes.push(opnum as u8);
        }
        else {
            self.bytecode_chunks[self.curr_bytecode_index].codes.push(u8::MAX);
            self.bytecode_chunks[self.curr_bytecode_index].codes.extend(opnum.to_ne_bytes());
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
        let patch_location = self.bytecode_chunks[self.curr_bytecode_index].codes.len() + 1;
        self.push_opnum(usize::MAX);
        patch_location
    }

    fn patch_jump_op(&mut self, to_patch_location: usize) {
        // The jump should be relative to the instruction after the jump itself.
        let jump_instruction_end = to_patch_location + std::mem::size_of::<usize>();
        let jump_target = self.bytecode_chunks[self.curr_bytecode_index].codes.len();
        
        let offset = jump_target - jump_instruction_end;
        let offset_bytes = offset.to_ne_bytes();
        
        let placeholder_slice = &mut self.bytecode_chunks[self.curr_bytecode_index].codes[to_patch_location..(to_patch_location + std::mem::size_of::<usize>())];
        placeholder_slice.copy_from_slice(&offset_bytes);
    }

    fn push_backwards_jump_op(&mut self, jump_location: usize) {
        self.push_op_with_opnum(OpCode::JumpBack, self.bytecode_chunks[self.curr_bytecode_index].codes.len() - jump_location + 2);
    }


    fn push_get_constant_op(&mut self, val: Value) {
        // let index = match self.bytecode.constants.iter().position(|constant| val == constant.clone()) {
        //     Some(i) => i,
        //     None => {
        //         self.bytecode.constants.push(val);
        //         self.bytecode.constants.len() - 1
        //     }
        // };
        let index = self.bytecode_chunks[self.curr_bytecode_index].constants.len();
        self.bytecode_chunks[self.curr_bytecode_index].constants.push(val);
        self.push_op_with_opnum(OpCode::ConstGet, index);

        self.cur_temp_amount += 1;
    }

    fn enter_scope(&mut self) { self.curr_scope_depth += 1; }
    fn exit_scope(&mut self) -> Vec<u8> {
        self.curr_scope_depth -= 1;

        // update the max locals needed variable
        let before_cleanup = self.get_curr_locals().len();
        if before_cleanup > self.bytecode_chunks[self.curr_bytecode_index].local_slots_needed {
            self.bytecode_chunks[self.curr_bytecode_index].local_slots_needed = before_cleanup
        }

        // trash locals that are now out of scope
        while !self.get_curr_locals().is_empty() && self.get_curr_locals().last().unwrap().scope_depth > self.curr_scope_depth {
            self.get_curr_locals().pop();
        }

        let mut cleanup_code = Vec::new();
        let cleanup_amount = before_cleanup - self.get_curr_locals().len();
        if cleanup_amount > 0 {
            cleanup_code.push(OpCode::LocalsFree as u8);
            cleanup_code.push(self.get_curr_locals().len() as u8);
            cleanup_code.push(cleanup_amount as u8);
        }
        cleanup_code
    }

    fn get_curr_locals(&mut self) -> &mut Vec<Local> {
        self.locals.last_mut().unwrap()
    }

    fn push_define_local(&mut self, name: String, const_value: Option<Value>) {
        let curr_locals_len = self.get_curr_locals().len();
        self.push_op_with_opnum(OpCode::LocalSet, curr_locals_len);
        let new_local = Local { name, scope_depth: self.curr_scope_depth, const_value };
        self.get_curr_locals().push(new_local);
        
        self.cur_temp_amount -= 1;
    }
    fn get_local_index(&mut self, name: &str) -> Option<usize> {
        for (i, local) in self.get_curr_locals().iter().enumerate().rev() {
            if local.name == name {
                return Some(i);
            }
        }
        None
    }
    fn push_get_identifier(&mut self, name: &str) {
        for (curr_compiler_i, local_locals) in self.locals.iter().rev().enumerate() {
            for (index, local) in local_locals.iter().enumerate().rev() {
                if local.name == name {
                    // if it has a known value, just push it as a const
                    if let Some(val) = &local.const_value {
                        self.push_get_constant_op(val.clone());
                        return;
                    }

                    // if it is found within this CompileFunction's stack
                    else if curr_compiler_i == 0 {
                        self.push_op_with_opnum(OpCode::LocalGet, index);
                        self.cur_temp_amount += 1;
                        return;
                    }
                    else { panic!("capturing isn't implemented yet :((") }
                }
            }
        }
        unreachable!("could not find {name} in locals vec or globals...")
    }


    fn push_set_local(&mut self, name: &str) {
        let i = self.get_local_index(name).unwrap_or_else(|| panic!("could not find {name} in the locals vec..."));
        self.push_op_with_opnum(OpCode::LocalSet, i);
        self.cur_temp_amount -= 1;
    }
    fn push_get_local_mut(&mut self, name: &str) {
        let i = self.get_local_index(name).unwrap_or_else(|| panic!("could not find {name} in the locals vec..."));
        self.push_op_with_opnum(OpCode::MakePointer, i);
        self.cur_temp_amount += 1;
    }

    fn push_void(&mut self) {
        self.cur_temp_amount += 1;
        self.push_op(OpCode::PushVoid);
    }
    fn push_pop_value(&mut self) {
        self.cur_temp_amount -= 1;
        self.push_op(OpCode::ValuePop);
    }
    fn push_dup_value(&mut self) {
        self.cur_temp_amount += 1;
        self.push_op(OpCode::ValueDup);
    }
    fn push_cmp_equal(&mut self) {
        self.cur_temp_amount -= 1;
        self.push_op(OpCode::CmpEqual);
    }
    fn push_jump_if_false(&mut self) {
        self.cur_temp_amount -= 1;
        self.push_op(OpCode::JumpIfFalse);
    }
    fn push_arr_ref_set(&mut self) {
        self.cur_temp_amount -= 1;
        self.push_op(OpCode::ArrRefSet);
    }
    fn push_value_ref_set(&mut self) {
        self.cur_temp_amount -= 1;
        self.push_op(OpCode::ValueRefSet);
    }







    fn compile_block_expression(&mut self, body: &[TypedExpr]) {
        // define FnDefinitions first
        // they will actually be globals, so that another CompileFunction can also call it
        for expr in body {
            match &expr.expression {
                Expr::FnDefinition { name, params, body, .. } => {
                    let fn_index = self.bytecode_chunks.len();
                    let function_value = Value::Closure { chunk_index: fn_index };
                    self.push_get_constant_op(function_value.clone());
                    self.push_define_local(name.clone(), Some(function_value));
    
                    CompileFunction::compile_function(
                        name.to_string(),
                        std::slice::from_ref(body),
                        &params,
                        self.bytecode_chunks,
                        self.locals,
                        self.library,
                    );
                }
                _ => { /* Do nothing */}
            }
        }

        // process all other expressions
        if let Some((last_expr, preceding_exprs)) = body.split_last() {
            self.enter_scope();

            for expr in preceding_exprs {
                self.compile_expression(expr);
                // if it is not the last expression of the block, the return value is always useless, so pop it.
                self.push_pop_value();
            }
            self.compile_expression(last_expr);

            let cleanup_code = self.exit_scope();
            self.push_codes(&cleanup_code);
        }
        else {
            // if block is empty, just push void
            self.push_void();
        }
    }







    fn compile_binding_pattern(&mut self, pattern: &AssignablePattern, failure_jumps: &mut Vec<FailureJump>) {
        match pattern {
            // the value is already on the stack, just the compiler just needs a variable that points to it.
            AssignablePattern::Binding { name, .. } => self.push_define_local(name.clone(), None),
            AssignablePattern::Wildcard => self.push_pop_value(),

            AssignablePattern::Tuple(patterns) => {
                self.push_op(OpCode::TupUnpack);
                self.cur_temp_amount += patterns.len() - 1;

                for pattern in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }
            AssignablePattern::Place(PlaceExpr::Identifier(name)) => self.push_set_local(name),

            AssignablePattern::Place(PlaceExpr::Deref(name)) => {
                self.push_get_identifier(name);
                self.push_value_ref_set();
            }


            // all patterns below this point can fail.
            AssignablePattern::Literal(lit) => {
                self.push_get_constant_op(lit.clone());
                self.push_cmp_equal();
                self.push_jump_if_false();
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
            }
            AssignablePattern::Array(patterns) => {
                self.push_op_with_opnum(OpCode::ArrUnpackCheckJump, patterns.len());
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
                self.cur_temp_amount += patterns.len() - 1;

                for pattern in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }

            AssignablePattern::Conditional { pattern, body } => {
                self.compile_binding_pattern(pattern, failure_jumps);
                self.compile_expression(body);
                self.push_jump_if_false();
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
            }

            AssignablePattern::Place(PlaceExpr::Index { left, index }) => {
                self.compile_expression(index);
                match &left.expression {
                    Expr::Identifier { name } => self.push_get_local_mut(name),
                    _ => todo!("{:?}", left.expression)
                }
                self.push_arr_ref_set();
            }
            _ => panic!("not implemented")
        }
    }



    fn compile_binding_pattern_failure_jumps(&mut self, failure_jumps: &mut Vec<FailureJump>) {
        // complicated function but it does this:
        // if one failure jump had 5 temps
        // and another had 3 temps
        // and we need to get to 2 temps
        // then it compiles to this:
        // pop <- temp5 jump lands here
        // pop
        // pop <- temp 3 jump lands here

        // sort biggest to smallest
        failure_jumps.sort_by(|a, b| b.temps.cmp(&a.temps));
        
        let mut jumps_iter = failure_jumps.iter().peekable();

        while let Some(current_jump) = jumps_iter.next() {
            self.patch_jump_op(current_jump.jump_loc);

            // Determine the temp amount needed for the next failure jump
            let next_cleanup_depth = match jumps_iter.peek() {
                Some(next_jump) => next_jump.temps,
                // if this already was last jump, the next depth is the final target.
                None => self.cur_temp_amount,
            };

            // how many pops do we need to get from our current depth to the next?
            let pops_needed = current_jump.temps - next_cleanup_depth;
            for _ in 0..pops_needed {
                self.push_op(OpCode::ValuePop);
            }
        }
    }






    fn push_place_expr_value(&mut self, place_expr: PlaceExpr) {
        match place_expr {
            PlaceExpr::Identifier(name) => self.push_get_identifier(&name),
            PlaceExpr::Index { left, index } => {
                self.compile_expression(&left);
                self.compile_expression(&index);
                self.push_op(OpCode::ArrGet);
            }
            PlaceExpr::Deref(name) => {
                self.push_get_identifier(&name);
                self.push_op(OpCode::FollowPointer);
            }
        }
    }







    fn compile_infix(&mut self, operator: &TokenType, left_type: &TypeKind, right: &TypedExpr) {
        if TokenType::EqualEqual == *operator {
            self.compile_expression(right);
            self.push_op(OpCode::CmpEqual);
        }
        else if TokenType::NotEqual == *operator {
            self.compile_expression(right);
            self.push_ops([OpCode::CmpEqual, OpCode::BoolNegate])
        }
        else if TypeKind::Bool == *left_type {
            match operator {
                TokenType::Ampersand => {
                    self.push_jump_if_false();
                    let jump_over_right_expression = self.push_opnum_for_patching();
                    self.compile_expression(right);
                    self.push_op(OpCode::Jump);
                    let right_expression_jump_over_push_true = self.push_opnum_for_patching();
                    self.patch_jump_op(jump_over_right_expression);
                    self.push_get_constant_op(Value::Bool(false));
                    self.patch_jump_op(right_expression_jump_over_push_true);
                }
                TokenType::Pipe => {
                    self.push_op(OpCode::BoolNegate);
                    self.push_jump_if_false();
                    let jump_over_right_expression = self.push_opnum_for_patching();
                    self.compile_expression(right);
                    self.push_op(OpCode::Jump);
                    let right_expression_jump_over_push_true = self.push_opnum_for_patching();
                    self.patch_jump_op(jump_over_right_expression);
                    self.push_get_constant_op(Value::Bool(true));
                    self.patch_jump_op(right_expression_jump_over_push_true);
                }
                _ => unreachable!("Unsupported operator {} for type bool", operator)
            }
        }
        else {
            self.compile_expression(right);
            match left_type {
                TypeKind::Num => match operator {
                    TokenType::Plus => self.push_op(OpCode::NumAdd),
                    TokenType::Minus => self.push_op(OpCode::NumSubtract),
                    TokenType::Star => self.push_op(OpCode::NumMultiply),
                    TokenType::Slash => self.push_op(OpCode::NumDivide),
                    TokenType::Percent => self.push_op(OpCode::NumModulo),
                    TokenType::StarStar => self.push_op(OpCode::NumExponent),
                    TokenType::Less => self.push_op(OpCode::CmpLess),
                    TokenType::Greater => self.push_op(OpCode::CmpGreater),
                    TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::BoolNegate]),
                    TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::BoolNegate]),
                    _ => unreachable!("Unsupported operator {} for type num", operator)
                },
                TypeKind::Str => match operator {
                    TokenType::Plus => self.push_op(OpCode::StrAdd),
                    TokenType::Less => self.push_op(OpCode::CmpLess),
                    TokenType::Greater => self.push_op(OpCode::CmpGreater),
                    TokenType::LessEqual => self.push_ops([OpCode::CmpGreater, OpCode::BoolNegate]),
                    TokenType::GreaterEqual => self.push_ops([OpCode::CmpLess, OpCode::BoolNegate]),
                    _ => unreachable!("Unsupported operator {} for type str", operator)
                }
                TypeKind::Arr(_) => match operator {
                    _ => unreachable!("Unsupported operator {} for type arr", operator)
                }
                TypeKind::Tup(_) => match operator {
                    _ => unreachable!("Unsupported operator {} for type tup", operator)
                }

                TypeKind::Never => {
                    /* Do nothing */
                }

                _ => unreachable!("Mismatched type for infix operation: {left_type}")
            }
        }
        self.cur_temp_amount -= 1;
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