use std::{fmt, rc::Rc, u8};

use crate::{ast_structure::Value, pretty_printing::join_slice_to_string, to_bytecode::{BytecodeChunk, OpCode}};


pub struct RuntimeError {
    pub message: String
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl RuntimeError {
    fn error(message: String) -> RuntimeError { RuntimeError { message } }
}



macro_rules! run_infix_op {
    ($self:expr, $left_pattern:pat, $right_pattern:pat => $result_expr:expr) => {
        {
            if $self.value_stack.len() < 2 { unreachable!("Stack underflow for infix operation.") }
            let right = $self.value_stack.pop().unwrap();
            let left = $self.value_stack.pop().unwrap();
            match (left, right) {
                // (Value::Num(l), Value::Num(r)) => $self.value_stack.push(Value::Num(l + r))
                ($left_pattern, $right_pattern) => $self.value_stack.push($result_expr),
                (left, right) => unreachable!("Operands {left}, {right} cannot be {}-ed", stringify!($result_expr)),
            }
        }
    };
}
macro_rules! run_prefix_op {
    ($self:expr, $operand_pattern:pat => $result_expr:expr) => {
        {
            if $self.value_stack.is_empty() { unreachable!("Stack underflow for prefix operation.") }
            let operand = $self.value_stack.pop().unwrap();
            match operand {
                // Value::Num(n) => $self.value_stack.push(Value::Num(-n))
                $operand_pattern => $self.value_stack.push($result_expr),
                operand => unreachable!("Operand {operand} cannot be {}-ed", stringify!($result_expr)),
            }
        }
    };
}




// a new CallFrame is pushed when:
// - entering a function
#[derive(Default)]
pub struct CallFrame {
    // points to the current BytecodeChunk we are executing
    chunk_index: usize,

    // instruction pointer
    ip: usize,

    // the current location in the stack where temp values are being calculated with
    //                                                          v points here
    // [... values from functions lower on the callstack ..., LOCAL1, LOCAL2, LOCAL3, TEMP1, TEMP2 TEMP3]
    base_pointer: usize,
}



#[derive(Default)]
pub struct VM {
    bytecode_chunks: Vec<BytecodeChunk>,
    frames: Vec<CallFrame>,
    pub value_stack: Vec<Value>
}

impl VM {
    pub fn new() -> Self {
        VM::default()
    }
    
    pub fn load_bytecodes(&mut self, bytecode_chunks: Vec<BytecodeChunk>) {
        self.bytecode_chunks = bytecode_chunks;
        self.load_frame_from_index(0);
    }


    fn load_frame_from_index(&mut self, chunk_index: usize) {
        let chunk = &self.bytecode_chunks[chunk_index];
        let frame = CallFrame {
            chunk_index: chunk_index,
            ip: 0,
            base_pointer: self.value_stack.len(),
        };
        self.value_stack.resize_with(frame.base_pointer + chunk.local_slots_needed, || Value::Empty);
        self.frames.push(frame);
    }


    pub fn run(&mut self, print_debug_execution: bool) -> Result<(), RuntimeError> {
        loop {
            let frame = match self.frames.last_mut() {
                Some(x) => x,
                None => return Ok(())
            };
            let chunk = &self.bytecode_chunks[frame.chunk_index];

            let instruction = VM::read_next_instruction(frame, chunk);
            match instruction {
                OpCode::ReturnVoid => {
                    let new_len = self.value_stack.len() - chunk.local_slots_needed;
                    self.value_stack.truncate(new_len);
                    self.frames.pop();
                }
                OpCode::Return => {
                    let return_value = self.value_stack.pop().unwrap();
                    let new_len = self.value_stack.len() - chunk.local_slots_needed;
                    self.value_stack.truncate(new_len);
                    self.frames.pop();
                    self.value_stack.push(return_value);
                }



                OpCode::ConstGet => {
                    let slot = VM::read_next_opnum(frame, chunk);
                    let constant = chunk.constants[slot].clone();
                    self.value_stack.push(constant);
                }

                // Numbers
                OpCode::NumAdd => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l + r)),
                OpCode::NumSubtract => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l - r)),
                OpCode::NumMultiply => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l * r)),
                OpCode::NumDivide => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l / r)),
                OpCode::NumModulo => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l % r)),
                OpCode::NumExponent => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l.powf(r))),
                OpCode::NumNegate => run_prefix_op!(self, Value::Num(n) => Value::Num(-n)),

                // Strings
                OpCode::StrAdd => run_infix_op!(self, Value::Str(l), Value::Str(r) => Value::Str(l + &r)),
                OpCode::StrTemplate => {
                    let elements_count = VM::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let mut string = String::new();
                    for val in self.value_stack.drain(cut_off_index..) {
                        string += &VM::val_to_string(&val);
                    }
                    self.value_stack.push(Value::Str(string));
                }

                // bools
                OpCode::BoolAnd => run_infix_op!(self, Value::Bool(l), Value::Bool(r) => Value::Bool(l && r)),
                OpCode::BoolOr => run_infix_op!(self, Value::Bool(l), Value::Bool(r) => Value::Bool(l || r)),
                OpCode::BoolNegate => run_prefix_op!(self, Value::Bool(b) => Value::Bool(!b)),

                // Arrays
                OpCode::ArrCreate => {
                    let elements_count = VM::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let arr = self.value_stack.drain(cut_off_index..).collect();
                    self.value_stack.push(Value::Arr(Rc::new(arr)));
                }

                // Tuples
                OpCode::TupCreate => {
                    let elements_count = VM::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let tup = self.value_stack.drain(cut_off_index..).collect();
                    self.value_stack.push(Value::Tup(Rc::new(tup)));
                }

                // Any
                // relies on `impl PartialEq for Value`
                OpCode::CmpEqual => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.value_stack.push(Value::Bool(left == right));
                }
                // relies on `impl PartialOrd for Value`
                OpCode::CmpLess => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.value_stack.push(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Less)));
                }
                OpCode::CmpGreater => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.value_stack.push(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Greater)));
                }



                OpCode::LocalGet => {
                    let slot = VM::read_next_opnum(frame, chunk);
                    let value = self.value_stack[frame.base_pointer + slot].clone();
                    self.value_stack.push(value);
                }
                OpCode::LocalSet => {
                    let slot = VM::read_next_opnum(frame, chunk);
                    let value = self.value_stack.pop().unwrap();
                    self.value_stack[frame.base_pointer + slot] = value;
                }
                OpCode::LocalsFree => {
                    let slot = VM::read_next_opnum(frame, chunk);
                    let amount = VM::read_next_opnum(frame, chunk);
                    let start = frame.base_pointer + slot;
                    for i in start..(start + amount) {
                        self.value_stack[i] = Value::Empty;
                    }
                }

                OpCode::ValuePop => {
                    self.value_stack.pop().unwrap();
                }
                OpCode::ValueDrop => {
                    // [1, 2, 3, 4] -> [1, 2, 4]
                    let index = self.value_stack.len() - 2;
                    self.value_stack.swap_remove(index);
                }

                OpCode::ValueDup => {
                    let value = self.value_stack.last().unwrap().clone();
                    self.value_stack.push(value);
                }

                OpCode::FollowPointer => {
                    let Value::ValueStackPointer(pointer) = self.value_stack.pop().unwrap()
                    else { unreachable!() };

                    let val = self.value_stack[frame.base_pointer + pointer].clone();
                    self.value_stack.push(val);
                }

                OpCode::ValueRefSet => {
                    let Value::ValueStackPointer(pointer) = self.value_stack.pop().unwrap()
                    else { unreachable!("Value was not a pointer") };
                    let value = self.value_stack.pop().unwrap();
                    
                    self.value_stack[frame.base_pointer + pointer] = value;
                }


                OpCode::Jump => {
                    let offset = VM::read_next_opnum(frame, chunk);
                    frame.ip += offset;
                }
                OpCode::JumpBack => {
                    let offset = VM::read_next_opnum(frame, chunk);
                    frame.ip -= offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = VM::read_next_opnum(frame, chunk);
                    if let Value::Bool(bool) = self.value_stack.pop().unwrap() {
                        if !bool { frame.ip += offset }
                    }
                    else { unreachable!("Encountered a non-boolean value...") }
                }


                OpCode::ArrGet => {
                    let index = self.value_stack.pop().unwrap();
                    let arr = self.value_stack.pop().unwrap();

                    let (Value::Arr(a), Value::Num(i)) = (arr, index)
                    else { unreachable!() };

                    if i.fract() > 0.0 {
                        return Err(RuntimeError::error(format!("Cannot index arr with a non-integer number: {}", i)))
                    }
                    let corrected_index = i as usize;
                    let arr_element = match a.get(corrected_index) {
                        Some(x) => x.clone(),
                        None => return Err(RuntimeError::error(format!("Index {corrected_index} is out of bounds for arr of length {}.", a.len())))
                    };
                    self.value_stack.push(arr_element);
                }

                OpCode::ArrRefSet => {
                    let arr_pointer = self.value_stack.pop().unwrap();
                    let index = self.value_stack.pop().unwrap();
                    let val = self.value_stack.pop().unwrap();

                    let (Value::ValueStackPointer(pointer_loc), Value::Num(i)) = (arr_pointer, index)
                    else { unreachable!() };

                    let Value::Arr(a) = self.value_stack.get_mut(frame.base_pointer + pointer_loc).unwrap()
                    else { unreachable!("Value was not an array") };

                    if i.fract() > 0.0 {
                        return Err(RuntimeError::error(format!("Cannot index arr with a non-integer number: {}", i)))
                    }
                    let i_usize = i as usize;
                    if i_usize >= a.len() {
                        return Err(RuntimeError::error(format!("Index {i_usize} is out of bounds for arr of length {}.", a.len())));
                    }

                    match Rc::get_mut(a) {
                        Some(a_mut) => {
                            a_mut[i_usize] = val;
                        }
                        // other references exist -> clone the entire array
                        None => {
                            let mut new_arr = (**a).clone();
                            new_arr[i_usize] = val;
                            *a = Rc::new(new_arr);
                        }
                    }
                }

                OpCode::ArrUnpackCheckJump => {
                    let length_required = VM::read_next_opnum(frame, chunk);
                    let jump_offset = VM::read_next_opnum(frame, chunk);
                    let Value::Arr(arr) = self.value_stack.pop().unwrap()
                        else { unreachable!("last value was not an array") };

                    // do the length jump, wrong -> jump, correct -> unpack
                    if arr.len() < length_required {
                        frame.ip += jump_offset
                    }
                    else {
                        match Rc::try_unwrap(arr) {
                            Ok(vec) => { self.value_stack.extend(vec); }
                            // other references exist -> clone each Value
                            Err(vec_rc) => { self.value_stack.extend(vec_rc.iter().cloned()); }
                        }
                    }
                }


                OpCode::TupUnpack => {
                    let Value::Tup(tup) = self.value_stack.pop().unwrap()
                        else { unreachable!("last value was not a tuple") };
                                
                    match Rc::try_unwrap(tup) {
                        Ok(vec) => { self.value_stack.extend(vec); }
                        // other references exist -> clone each Value
                        Err(vec_rc) => { self.value_stack.extend(vec_rc.iter().cloned()); }
                    }
                }


                OpCode::CallFn => {
                    let arg_count = VM::read_next_opnum(frame, chunk);
                    let callee = self.value_stack.pop().unwrap();

                    let first_arg_index = self.value_stack.len() - arg_count;
                    let mut args: Vec<Value> = self.value_stack.drain(first_arg_index..).collect();

                    match callee {
                        Value::NativeFn(native_fn) => {
                            let result = native_fn(&mut args)?;
                            match result {
                                Value::Void => { /* do nothing */}
                                _ => self.value_stack.push(result),
                            }
                        }

                        Value::Closure { chunk_index } => {
                            self.load_frame_from_index(chunk_index);
                            self.value_stack.extend(args);
                        }

                        _ => unreachable!("tried to call {callee}...")
                    }
                }




                OpCode::Panic => {
                    let Value::Str(message) = self.value_stack.pop().unwrap()
                    else { unreachable!("last value was not a str") };
                    return Err(RuntimeError { message })
                }
            }
            if print_debug_execution {
                let instruction_name = format!("{:?}", instruction);
                println!("{:<12}-> {}", instruction_name, join_slice_to_string(&self.value_stack, ", "));
            }
        }
    }


    fn read_byte(frame: &mut CallFrame, chunk: &BytecodeChunk) -> u8 {
        let byte = chunk.codes[frame.ip];
        frame.ip += 1;
        byte
    }
    fn read_bytes<const N: usize>(frame: &mut CallFrame, chunk: &BytecodeChunk) -> [u8; N] {
        let end = frame.ip + N;
        let bytes = chunk.codes[frame.ip..end]
            .try_into()
            .expect("Bytecode ended unexpectedly");
        frame.ip = end;
        bytes
    }

    pub fn read_next_instruction(frame: &mut CallFrame, chunk: &BytecodeChunk) -> OpCode {
        VM::read_byte(frame, chunk).try_into().expect("Not a valid OpCode")
        // unsafe { std::mem::transmute(self.read_byte()) }
    }

    pub fn read_next_opnum(frame: &mut CallFrame, chunk: &BytecodeChunk) -> usize {
        let next_byte = VM::read_byte(frame, chunk);
        // small number
        if next_byte < u8::MAX { next_byte as usize }
        // wide number
        else { usize::from_ne_bytes(VM::read_bytes(frame, chunk)) }
    }




    fn val_to_string(val: &Value) -> String {
        match val {
            Value::Num(num) => num.to_string(),
            Value::Str(str) => str.clone(),
            Value::Bool(bool) => bool.to_string(),

            Value::Arr(arr) => {
                let str_results: Vec<String> = arr.iter().map(|x| VM::val_to_string(x)).collect();
                String::from("[") + &str_results.join(", ") + "]"
            }
            Value::Tup(tup) => {
                let str_results: Vec<String> = tup.iter().map(|x| VM::val_to_string(x)).collect();
                String::from("(") + &str_results.join(", ") + ")"
            }
            _ => panic!("Literal {:?} cannot be converted into a string.", val)
        }
    }
}