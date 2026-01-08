use std::collections::HashMap;

use strum_macros::FromRepr;

use crate::{Program, lexing::tokens::TokenType, nativelib::{ThrumModule, get_native_lib}, parsing::ast_structure::{Expr, ExprInfo, MatchArm, MatchPattern, MatchPatternInfo, TupleElement, TupleMatchPattern, TypeKind, Value}, typing::VarID};


#[repr(u8)]
#[derive(Debug, FromRepr)]
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
    TupCreate, TupGet, TupUnpack,

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





pub struct Compiler;

impl Compiler {
    pub fn compile_program(program: &mut Program) {
        let mut bytecode_chunks = Vec::new();
        let mut variables = HashMap::new();

        let library = get_native_lib();
        Self::load_prelude_into_variables(&library, &mut variables, &mut 1);

        CompileFunction::compile_function(
            "<main>".to_string(),
            // the ast will be gone after compilation!!!
            &std::mem::take(&mut program.ast).unwrap(),
            &[],
            &mut bytecode_chunks,
            &mut variables,
            &library,
        );
        program.compiled_bytecode = bytecode_chunks;
    }

    fn load_prelude_into_variables(module: &ThrumModule, variables: &mut HashMap<VarID, CompilerVar>, next_var_id: &mut usize /* incredibly ugly but just for now */) {
        for value in module.values.values() {
            if value.is_prelude {
                variables.insert(VarID(*next_var_id), CompilerVar::ConstValue(value.val.clone()));
                *next_var_id += 1;
            }
        }
        // Recursion
        for sub_module in module.sub_modules.values() {
            Self::load_prelude_into_variables(sub_module, variables, next_var_id);
        }
    }
}





struct FailureJump {
    temps: usize,
    jump_loc: usize,
}



enum CompilerVar {
    AtSlot(usize),
    ConstValue(Value)
}


struct CompileFunction<'a> {
    // bytecodes[curr_bytecode_index] is what this CompileFunction writes to.
    // the others are from other functions being compiled by other CompileFunction's.
    bytecode_chunks: &'a mut Vec<BytecodeChunk>,
    curr_bytecode_index: usize,

    // shared across all CompileFunctions.
    library: &'a ThrumModule,
    variables: &'a mut HashMap<VarID, CompilerVar>,


    // only for this CompileFunction:
    cur_var_amount: usize,

    // compiling `let x = 2 + 2`
    // - 2 temps (2, 2)
    // -> 1 temp (4)
    // -> 0 temps, because 4 got moved into a var slot (+1 var)
    // -> 1 temp, because the let-expr pushed void
    cur_temp_amount: usize,
    
    // break / continue
    loop_infos: Vec<LoopInfo>,

}

struct LoopInfo {
    // the end isn't known until the full loop is compiled.
    // so all break jumps temporarily hang out here
    break_jumps: Vec<usize>,
    // where the loop starts (for continue)
    start: usize,
    label: String,
}

impl<'a> CompileFunction<'a> {
    fn compile_function(
        name: String,
        program: &ExprInfo,
        params: &[MatchPatternInfo],
        bytecode_chunks: &mut Vec<BytecodeChunk>,
        variables: &mut HashMap<VarID, CompilerVar>,
        library: &ThrumModule,
    ) -> usize
        {
        let mut compile_function = CompileFunction {
            curr_bytecode_index: bytecode_chunks.len(),
            bytecode_chunks,
            library,
            variables,

            cur_var_amount: 0,
            cur_temp_amount: params.len(),

            loop_infos: Vec::new(),
        };
        // push the new bytecode_chunk and localsvec for this function
        compile_function.bytecode_chunks.push(BytecodeChunk { name, ..Default::default() });

        // compile the params first
        for param in params.iter().rev() {
            compile_function.compile_binding_pattern(param, &mut Vec::new());
        }

        // then compile the body!!
        compile_function.compile_expression(program);

        // drop all param variables (not neccessary, but good practice)
        for param in params {
            compile_function.drop_vars(
                &param.vars_defined.iter().map(|(_, x)| *x).collect::<Vec<_>>(),
                false
            );
        }

        // finally, return
        compile_function.push_op(OpCode::Return);
        compile_function.curr_bytecode_index
    }

    fn compile_expression(&mut self, expr: &ExprInfo) {
        let start_temps = self.cur_temp_amount;

        match &expr.expression {
            Expr::Literal(val) => {
                self.push_get_constant_op(val.clone());
            }
            Expr::TemplateString(elements) => {
                for element in elements {
                    self.compile_expression(element);
                }
                self.push_op_with_opnum(OpCode::StrTemplate, elements.len());
                self.cur_temp_amount = (self.cur_temp_amount + 1) - elements.len();
            }
            Expr::Array(elements) => {
                for element in elements {
                    self.compile_expression(element);
                }
                self.push_op_with_opnum(OpCode::ArrCreate, elements.len());
                self.cur_temp_amount = (self.cur_temp_amount + 1) - elements.len();
            }
            Expr::Tuple(exprs) => {
                for TupleElement { label: _, expr } in exprs {
                    self.compile_expression(expr);
                }
                self.push_op_with_opnum(OpCode::TupCreate, exprs.len());
                self.cur_temp_amount = (self.cur_temp_amount + 1) - exprs.len();
            }


            Expr::Infix { operator, left, right } => {
                self.compile_expression(left);
                self.compile_infix(&operator.token, &left.typ, right);
            }
            Expr::Prefix { operator, right } => {
                self.compile_expression(right);
                self.compile_prefix(operator, &right.typ)
            }

            Expr::Block { exprs, label, drops_vars } => {
                let drops_vars: &[VarID] = drops_vars;
                // define FnDefinitions first
                for expr in exprs {
                    match &expr.expression {
                        Expr::FnDefinition { name, var_id, params, body, ..  } => {
                            let chunk_index = self.bytecode_chunks.len();
                            let function_value = Value::Closure { chunk_index };
                            self.define_local(var_id.unwrap(), Some(function_value));

                            CompileFunction::compile_function(
                                name.to_string(),
                                body,
                                params,
                                self.bytecode_chunks,
                                self.variables,
                                self.library,
                            );
                        }
                        _ => { /* Do nothing */}
                    }
                }

                // process all other expressions
                if let Some((last_expr, preceding_exprs)) = exprs.split_last() {
                    // block label logic
                    if let Some(label) = label {
                        self.loop_infos.push(LoopInfo {
                            break_jumps: Vec::new(),
                            start: usize::MAX,
                            label: label.to_string()
                        });
                    }

                    // actual compiling of the expressions
                    for expr in preceding_exprs {
                        self.compile_expression(expr);
                        // if it is not the last expression of the block, the return value is discarded, so pop it.
                        self.push_pop_value();
                    }
                    self.compile_expression(last_expr);

                    
                    // all break jumps that are refering to this loop.
                    if let Some(label) = label {
                        let loop_info = self.loop_infos.pop().unwrap();
                        assert_eq!(label, &loop_info.label);

                        for jump in loop_info.break_jumps {
                            self.patch_jump_op(jump);
                        }                        
                    }
                }
                else {
                    // if block is empty, just push void
                    self.push_void();
                };

                self.drop_vars(drops_vars, true);
            }

            Expr::Identifier { var_id, .. } => {
                self.push_get_identifier(var_id.as_ref().unwrap());
            }

            Expr::Assign { pattern, extra_operator, value } => {
                for (_, var) in &pattern.vars_defined {
                    self.define_local(*var, None);
                }

                if let Some(val) = value {
                    if extra_operator.token == TokenType::Equal {
                        // push value to the stack
                        self.compile_expression(val);
                    }
                    else {
                        // example: x += 2
                        // it needs to first push the value of x, compute x + 2, then set x to that result
                        match &pattern.pattern {
                            MatchPattern::PlaceIdentifier { var_id, .. } => self.push_get_identifier(var_id.as_ref().unwrap()),
                            MatchPattern::PlaceIndex { left, index } => {
                                self.compile_expression(left);
                                self.compile_expression(index);
                                self.push_op(OpCode::ArrGet);
                            }
                            MatchPattern::PlaceDeref { var_id, .. } => {
                                self.push_get_identifier(var_id.as_ref().unwrap());
                                self.push_op(OpCode::FollowPointer);
                            }
                            _ => unreachable!("Infix assignments are only allowed for place patterns.")
                        }
                        self.compile_infix(&extra_operator.token, &val.typ, val);
                    }

                    // compile the binding pattern
                    let mut failure_jumps = Vec::new();
                    self.compile_binding_pattern(pattern, &mut failure_jumps);

                    // if AssignablePattern can fail, use the else block or panic
                    if !failure_jumps.is_empty() {
                        unreachable!("assign pattern didn't match in let expression, this should not be possible.")
                    }
                }

                self.push_void();
            }

            Expr::Case { pattern, value } => {
                self.compile_expression(value);  // (+1 temp)

                let mut failure_jumps = Vec::new();
                self.compile_binding_pattern(pattern, &mut failure_jumps);  // consumes value (-1 temp)

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
                    Expr::Identifier { var_id, .. } => {
                        self.push_get_local_mut(var_id.as_ref().unwrap());
                    }
                    _ => todo!()
                }
            }

            Expr::Deref { expr } => {
                match &expr.expression {
                    Expr::Identifier { var_id, .. } => {
                        self.push_get_identifier(var_id.as_ref().unwrap());
                        self.push_op(OpCode::FollowPointer);
                    }
                    _ => todo!()
                }
            }


            Expr::If { condition, then: consequence, alt: alternative } => {
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

            Expr::Loop { body, label } => {
                // what it should look like:
                // ...loop...
                // ...loop...
                // Jump -3
                let loop_start = self.bytecode_chunks[self.curr_bytecode_index].codes.len();

                self.loop_infos.push(LoopInfo {
                    break_jumps: Vec::new(),
                    start: loop_start,
                    label: label.to_string()
                });
                self.compile_expression(body);
                self.push_pop_value();
                self.push_backwards_jump_op(loop_start);

                // all break/continue jumps that are refering to this loop.
                let loop_info = self.loop_infos.pop().unwrap();
                assert_eq!(label, &loop_info.label);

                for jump in loop_info.break_jumps {
                    self.patch_jump_op(jump);
                }

                // doesnt need to push void, because it's an infinite loop
                self.cur_temp_amount += 1;
            }

            Expr::Break { expr, label } => {
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

                // find the correct loop to break to
                let loop_info = if let Some(break_label) = label {
                    self.loop_infos
                        .iter_mut().rev()
                        .find(|x| x.label == *break_label)
                        .expect("could not find label...")
                } else {
                    self.loop_infos.last_mut().unwrap()
                };
                loop_info.break_jumps.push(break_jump);

                // pretend like this break expression didn't happen, to compile expressions after this.
                self.cur_temp_amount += temp_pop_amount;
            }

            Expr::Continue { label } => {
                let temp_pop_amount = self.cur_temp_amount;

                // pop all temp values
                for _ in 0..temp_pop_amount {
                    self.push_pop_value();
                }

                // find the correct loop to break to
                let loop_info = if let Some(continue_label) = label {
                    self.loop_infos
                        .iter().rev()
                        .find(|x| x.label == *continue_label)
                        .expect("could not find label...")
                } else {
                    self.loop_infos.last_mut().unwrap()
                };
                let to_start = loop_info.start;
                // and then actually continue
                self.push_backwards_jump_op(to_start);
                
                // pretend like this break expression didn't happen, to compile expressions after this.
                self.cur_temp_amount += temp_pop_amount + 1;
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
                self.compile_expression(callee);
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
                    body,
                    params,
                    self.bytecode_chunks,
                    self.variables,
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

            Expr::MemberAccess { left, member: _, resolved_index } => {
                self.compile_expression(left);
                self.push_op_with_opnum(OpCode::TupGet, resolved_index.unwrap());
            }

            _ => { panic!("{expr:?} not yet implemented") }
        }

        if start_temps + 1 != self.cur_temp_amount {
            panic!("wrong temp number ({} -> {}) after processing {:?}", start_temps, self.cur_temp_amount, expr);
        }
    }






    fn push_op(&mut self, op: OpCode) { self.bytecode_chunks[self.curr_bytecode_index].codes.push(op as u8); }
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
    // fn push_op_with_opnums(&mut self, op: OpCode, opnums: &[usize]) {
    //     self.push_op(op);
    //     for &opnum in opnums { self.push_opnum(opnum); }
    // }

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

    fn define_local(&mut self, var_id: VarID, const_value: Option<Value>) -> usize {
        if let Some(var) = self.variables.get(&var_id) {
            match var {
                CompilerVar::AtSlot(slot) => *slot,
                CompilerVar::ConstValue(a) => unreachable!("Cannot get the slot of a constant {a}")
            }
        } else {
            // doesn't exist yet, define it!
            let to_insert = if let Some(const_val) = const_value {
                CompilerVar::ConstValue(const_val)
            } else {
                CompilerVar::AtSlot(self.cur_var_amount)
            };
            self.variables.insert(var_id, to_insert);
            let slot = self.cur_var_amount;

            self.cur_var_amount += 1;
            // if this is the highest amount of vars needed so far, store that
            if self.cur_var_amount > self.bytecode_chunks[self.curr_bytecode_index].local_slots_needed {
                self.bytecode_chunks[self.curr_bytecode_index].local_slots_needed = self.cur_var_amount
            }

            slot
        }
    }

    fn push_define_local(&mut self, var_id: VarID) {
        let slot = self.define_local(var_id, None);
        self.push_op_with_opnum(OpCode::LocalSet, slot);
        self.cur_temp_amount -= 1;
    }

    fn push_get_identifier(&mut self, var_id: &VarID) {
        match self.variables.get(var_id) {
            None => {
                unreachable!("{var_id:?} is not in the current variables...")
            }
            Some(CompilerVar::ConstValue(val)) => {
                self.push_get_constant_op(val.clone());
            }
            Some(CompilerVar::AtSlot(slot)) => {
                self.push_op_with_opnum(OpCode::LocalGet, *slot);
                self.cur_temp_amount += 1;
            }
        }
    }

    fn push_set_local(&mut self, var_id: &VarID) {
        match self.variables.get(var_id) {
            None => unreachable!("{var_id:?} is not in the current variables..."),
            Some(CompilerVar::ConstValue(val)) => unreachable!("tried to set to a constant value {var_id:?}, {val}"),
            Some(CompilerVar::AtSlot(slot)) => {
                self.push_op_with_opnum(OpCode::LocalSet, *slot);
            }
        }
        self.cur_temp_amount -= 1;
    }
    fn push_get_local_mut(&mut self, var_id: &VarID) {
        match self.variables.get(var_id) {
            None => unreachable!("{var_id:?} is not in the current variables..."),
            Some(CompilerVar::ConstValue(val)) => unreachable!("tried to set to a constant value {var_id:?}, {val}"),
            Some(CompilerVar::AtSlot(slot)) => {
                self.push_op_with_opnum(OpCode::MakePointer, *slot);
            }
        }
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



    fn drop_vars(&mut self, vars: &[VarID], push_code: bool) {
        for var in vars {
            self.variables.remove(var);
            self.cur_var_amount -= 1;
        }
        if push_code {
            self.push_op(OpCode::LocalsFree);
            self.push_opnum(self.cur_var_amount);
            self.push_opnum(vars.len());
        }
    }







    fn compile_binding_pattern(&mut self, pattern: &MatchPatternInfo, failure_jumps: &mut Vec<FailureJump>) {
        match &pattern.pattern {
            // the value is already on the stack, just the compiler just needs a variable that points to it.
            MatchPattern::Binding { var_id, .. } => self.push_define_local(var_id.unwrap()),
            MatchPattern::Wildcard => self.push_pop_value(),

            MatchPattern::Tuple(patterns) => {
                self.push_op(OpCode::TupUnpack);
                self.cur_temp_amount += patterns.len() - 1;

                for TupleMatchPattern { label: _, pattern } in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }
            MatchPattern::PlaceIdentifier { var_id, .. } => self.push_set_local(var_id.as_ref().unwrap()),

            MatchPattern::PlaceDeref { var_id, .. } => {
                self.push_get_identifier(var_id.as_ref().unwrap());
                self.push_value_ref_set();
            }


            // all patterns below this point can fail.
            MatchPattern::Literal(lit) => {
                self.push_get_constant_op(lit.clone());
                self.push_cmp_equal();
                self.push_jump_if_false();
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
            }
            MatchPattern::Array(patterns) => {
                self.push_op_with_opnum(OpCode::ArrUnpackCheckJump, patterns.len());
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
                self.cur_temp_amount += patterns.len() - 1;

                for pattern in patterns.iter().rev() {
                    self.compile_binding_pattern(pattern, failure_jumps);
                }
            }

            MatchPattern::Conditional { pattern, body } => {
                self.compile_binding_pattern(pattern, failure_jumps);
                self.compile_expression(body);
                self.push_jump_if_false();
                failure_jumps.push(FailureJump { temps: self.cur_temp_amount, jump_loc: self.push_opnum_for_patching() });
            }

            MatchPattern::PlaceIndex { left, index } => {
                self.compile_expression(index);
                match &left.expression {
                    Expr::Identifier { var_id, .. } => self.push_get_local_mut(var_id.as_ref().unwrap()),
                    _ => todo!("{:?}", left.expression)
                }
                self.push_arr_ref_set();
            }
            _ => panic!("not implemented")
        }
    }



    fn compile_binding_pattern_failure_jumps(&mut self, failure_jumps: &mut [FailureJump]) {
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





    fn compile_infix(&mut self, operator: &TokenType, left_type: &TypeKind, right: &ExprInfo) {
        if TokenType::EqualEqual == *operator {
            self.compile_expression(right);
            self.push_op(OpCode::CmpEqual);
        }
        else if TypeKind::Bool == *left_type {
            // short circuiting logic:
            match operator {
                TokenType::Ampersand => {
                    // evaluate left
                    // left is true => discard it and return right
                    // left is false => return false
                    self.push_jump_if_false();
                    let jump_over_right_expression = self.push_opnum_for_patching();
                    self.compile_expression(right);
                    self.push_op(OpCode::Jump);
                    let jump_to_end = self.push_opnum_for_patching();
                    self.patch_jump_op(jump_over_right_expression);
                    self.push_get_constant_op(Value::Bool(false));
                    self.patch_jump_op(jump_to_end);
                }
                TokenType::Pipe => {
                    // evaluate left
                    // left is true => return true
                    // left is false => discard it and return right
                    self.push_op(OpCode::BoolNegate);
                    self.push_jump_if_false();
                    let jump_over_right_expression = self.push_opnum_for_patching();
                    self.compile_expression(right);
                    self.push_op(OpCode::Jump);
                    let jump_to_end = self.push_opnum_for_patching();
                    self.patch_jump_op(jump_over_right_expression);
                    self.push_get_constant_op(Value::Bool(true));
                    self.patch_jump_op(jump_to_end);
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
                    _ => unreachable!("Unsupported operator {} for type num", operator)
                },
                TypeKind::Str => match operator {
                    TokenType::Plus => self.push_op(OpCode::StrAdd),
                    TokenType::Less => self.push_op(OpCode::CmpLess),
                    TokenType::Greater => self.push_op(OpCode::CmpGreater),
                    _ => unreachable!("Unsupported operator {} for type str", operator)
                }
                TypeKind::Arr(_) => unreachable!("Unsupported operator {} for type arr", operator),
                TypeKind::Tup { .. } => unreachable!("Unsupported operator {} for type tup", operator),

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