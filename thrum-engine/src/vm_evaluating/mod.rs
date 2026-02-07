use std::fmt;

use crate::{parsing::ast_structure::Value, pretty_printing::join_slice_to_string, vm_compiling::{BytecodeChunk, OpCode}};


pub struct RuntimeError {
    pub message: String
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
impl RuntimeError {
    const fn error(message: String) -> Self { Self { message } }
}



macro_rules! run_op {
    // Infix (2 operands)
    ($self:expr, $left_pattern:pat, $right_pattern:pat => $result_expr:expr) => {
        {
            let right = $self.value_stack.pop().unwrap();
            let left = $self.value_stack.pop().unwrap();
            match (left, right) {
                // (Value::Num(l), Value::Num(r)) => $self.push_val(Value::Num(l + r))
                ($left_pattern, $right_pattern) => $self.stack_push_val($result_expr),
                (left, right) => unreachable!("Operands {left}, {right} cannot be {}-ed", stringify!($result_expr)),
            }
        }
    };
    // Prefix (1 operand)
    ($self:expr, $operand_pattern:pat => $result_expr:expr) => {
        {
            let operand = $self.value_stack.pop().unwrap();
            match operand {
                // Value::Num(n) => $self.push_val(Value::Num(-n))
                $operand_pattern => $self.stack_push_val($result_expr),
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
    pub ip: usize,

    // the current location in the stack where temp values are being calculated with
    //                                                          v points here
    // [... values from functions lower on the callstack ..., LOCAL1, LOCAL2, LOCAL3, TEMP1, TEMP2 TEMP3]
    base_pointer: usize,
}



#[derive(Default)]
pub struct VM {
    bytecode_chunks: Vec<BytecodeChunk>,
    frames: Vec<CallFrame>,
    pub value_stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            // this stack is NOT allowed to reallocate.
            // if it does, every Value::ValuePointer(*mut Value) breaks and unsafe behaviour happens :(
            value_stack: Vec::with_capacity(1024),
            ..Default::default()
        }
    }
    
    pub fn load_bytecodes(&mut self, bytecode_chunks: Vec<BytecodeChunk>) {
        self.bytecode_chunks = bytecode_chunks;
        self.load_frame_from_index(0);
    }


    fn load_frame_from_index(&mut self, chunk_index: usize) {
        let chunk = &self.bytecode_chunks[chunk_index];
        let frame = CallFrame {
            chunk_index,
            ip: 0,
            base_pointer: self.value_stack.len(),
        };
        let length_needed = frame.base_pointer + chunk.local_slots_needed;
        // self.value_stack.resize_with(length_needed, || Value::Empty);
        while self.value_stack.len() < length_needed {
            self.stack_push_val(Value::Empty);
        }
        self.frames.push(frame);
    }

    /// # Safety
    /// The typechecker should make sure that ValuePointers are used correctly!
    /// If it fails, this will cause UB. :(
    pub unsafe fn run(&mut self, print_debug_execution: bool) -> Result<(), RuntimeError> {
        loop {
            let Some(frame) = self.frames.last_mut()
            // else the program finished!
            else { return Ok(()) };
            let chunk = &mut self.bytecode_chunks[frame.chunk_index];

            let instruction = Self::read_next_instruction(frame, chunk);
            if print_debug_execution {
                print!("{:<16}-> ", format!("{instruction:?}"));
                // value stack will be printed after the match vv
            }
            match instruction {
                OpCode::Return => {
                    let return_value = self.value_stack.pop().unwrap();
                    let new_len = self.value_stack.len() - chunk.local_slots_needed;
                    self.value_stack.truncate(new_len);
                    self.stack_push_val(return_value);
                    self.frames.pop();
                }


                OpCode::ConstGet => {
                    let slot = Self::read_next_opnum(frame, chunk);
                    let constant = chunk.constants[slot].clone();
                    self.stack_push_val(constant);
                }
                OpCode::ConstGetRef => {
                    let slot = Self::read_next_opnum(frame, chunk);
                    let constant = Value::ValuePointer(&raw mut chunk.constants[slot]);
                    self.stack_push_val(constant);
                }
                OpCode::PushVoid => {
                    self.stack_push_val(Value::Void);
                }

                // Numbers
                OpCode::NumAdd => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l + r)),
                OpCode::NumSubtract => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l - r)),
                OpCode::NumMultiply => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l * r)),
                OpCode::NumDivide => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l / r)),
                OpCode::NumModulo => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l % r)),
                OpCode::NumExponent => run_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l.powf(r))),
                OpCode::NumNegate => run_op!(self, Value::Num(n) => Value::Num(-n)),

                // Strings
                OpCode::StrAdd => run_op!(self, Value::Str(l), Value::Str(r) => Value::Str(l + &r)),
                OpCode::StrTemplate => {
                    let elements_count = Self::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let mut string = String::new();
                    for val in self.value_stack.drain(cut_off_index..) {
                        string += &Self::val_to_string(&val);
                    }
                    self.stack_push_val(Value::Str(string));
                }

                // bools
                OpCode::BoolNegate => run_op!(self, Value::Bool(b) => Value::Bool(!b)),

                // Arrays
                OpCode::ArrCreate => {
                    let elements_count = Self::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let arr = self.value_stack.drain(cut_off_index..).collect();
                    self.stack_push_val(Value::Arr(arr));
                }

                // Tuples
                OpCode::TupCreate => {
                    let elements_count = Self::read_next_opnum(frame, chunk);
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let tup = self.value_stack.drain(cut_off_index..).collect();
                    self.stack_push_val(Value::Tup(tup));
                }

                // Any
                // relies on `impl PartialEq for Value`
                OpCode::CmpEqual => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.stack_push_val(Value::Bool(left == right));
                }
                // relies on `impl PartialOrd for Value`
                OpCode::CmpLess => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.stack_push_val(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Less)));
                }
                OpCode::CmpGreater => {
                    let right = self.value_stack.pop().unwrap();
                    let left = self.value_stack.pop().unwrap();
                    self.stack_push_val(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Greater)));
                }


                OpCode::LocalSet => {
                    let slot = Self::read_next_opnum(frame, chunk);
                    let value = self.value_stack.pop().unwrap();
                    self.value_stack[frame.base_pointer + slot] = value;
                }
                OpCode::LocalsFree => {
                    let slot = Self::read_next_opnum(frame, chunk);
                    let amount = Self::read_next_opnum(frame, chunk);
                    for i in slot..(slot + amount) {
                        self.value_stack[frame.base_pointer + i] = Value::Empty;
                    }
                }

                OpCode::LocalMakePointer => {
                    let slot = Self::read_next_opnum(frame, chunk);
                    let pointer = Value::ValuePointer(&raw mut self.value_stack[frame.base_pointer + slot]);
                    self.stack_push_val(pointer);
                }
                
                OpCode::PointerGetClone => {
                    let Value::ValuePointer(raw_p) = self.value_stack.pop().unwrap()
                    else { unreachable!() };

                    // unsafe, spooky
                    let mut_p = unsafe { &mut *raw_p };

                    self.stack_push_val(mut_p.clone());
                }

                OpCode::PointerGetMove => {
                    let Value::ValuePointer(raw_p) = self.value_stack.pop().unwrap()
                    else { unreachable!() };

                    // unsafe, spooky
                    let mut_p = unsafe { &mut *raw_p };

                    let value = std::mem::replace(mut_p, Value::Empty);
                    self.stack_push_val(value);
                }
                
                OpCode::PointerSet => {
                    let Value::ValuePointer(p) = self.value_stack.pop().unwrap()
                    else { unreachable!("Value was not a pointer") };
                    let value = self.value_stack.pop().unwrap();
                    
                    unsafe { *p = value; }
                }

                OpCode::ValuePop => {
                    self.value_stack.pop().unwrap();
                }
                // OpCode::ValueDrop => {
                //     // [1, 2, 3, 4] -> [1, 2, 4]
                //     let index = self.value_stack.len() - 2;
                //     self.value_stack.swap_remove(index);
                // }

                OpCode::ValueDup => {
                    let value = self.value_stack.last().unwrap().clone();
                    self.stack_push_val(value);
                }


                OpCode::Jump => {
                    let offset = Self::read_next_opnum(frame, chunk);
                    frame.ip += offset;
                }
                OpCode::JumpBack => {
                    let offset = Self::read_next_opnum(frame, chunk);
                    frame.ip -= offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = Self::read_next_opnum(frame, chunk);
                    if let Value::Bool(bool) = self.value_stack.pop().unwrap() {
                        if !bool { frame.ip += offset }
                    }
                    else { unreachable!("Encountered a non-boolean value...") }
                }


                OpCode::TupGet => {
                    let index = Self::read_next_opnum(frame, chunk);

                    let Value::Tup(tup) = self.value_stack.pop().unwrap()
                    else { unreachable!() };

                    let indexed = tup[index].clone();
                    self.stack_push_val(indexed);
                }

                OpCode::TupPointerGet => {
                    let index = Self::read_next_opnum(frame, chunk);
                    
                    let Value::ValuePointer(tup_pointer) = self.value_stack.pop().unwrap() else { unreachable!() };
                    let mut_tup_pointer = unsafe { &mut *tup_pointer };
                    let Value::Tup(tup) = mut_tup_pointer else { unreachable!() };
                    
                    self.stack_push_val(Value::ValuePointer(&raw mut tup[index]));
                }

                OpCode::ArrGet => {
                    let index = self.value_stack.pop().unwrap();
                    let arr = self.value_stack.pop().unwrap();

                    let (Value::Arr(a), Value::Num(i)) = (arr, index)
                    else { unreachable!() };

                    if i.fract() > 0.0 {
                        return Err(RuntimeError::error(format!("Cannot index arr with a non-integer number: {i}")))
                    }
                    let corrected_index = i as usize;
                    let arr_element = match a.get(corrected_index) {
                        Some(x) => x.clone(),
                        None => return Err(RuntimeError::error(format!("Index {corrected_index} is out of bounds for arr of length {}.", a.len())))
                    };
                    self.stack_push_val(arr_element);
                }

                OpCode::ArrPointerGet => {
                    let Value::ValuePointer(arr_pointer) = self.value_stack.pop().unwrap() else { unreachable!() };
                    let mut_arr_pointer = unsafe { &mut *arr_pointer };
                    let Value::Arr(arr) = mut_arr_pointer else { unreachable!() };

                    let Value::Num(i) = self.value_stack.pop().unwrap() else { unreachable!() };

                    if i.fract() > 0.0 {
                        return Err(RuntimeError::error(format!("Cannot index arr with a non-integer number: {i}")))
                    }
                    let i_usize = i as usize;
                    if i_usize >= arr.len() {
                        return Err(RuntimeError::error(format!("Index {i_usize} is out of bounds for arr of length {}.", arr.len())));
                    }

                    self.stack_push_val(Value::ValuePointer(&raw mut arr[i_usize]));
                }

                OpCode::ArrUnpackCheckJump => {
                    let length_required = Self::read_next_opnum(frame, chunk);
                    let jump_offset = Self::read_next_opnum(frame, chunk);
                    let Value::Arr(arr) = self.value_stack.pop().unwrap() else { unreachable!("last value was not an array") };

                    // do the length jump, wrong -> jump, correct -> unpack
                    if arr.len() < length_required {
                        // jump path
                        frame.ip += jump_offset;
                    }
                    else {
                        // extend the value stack
                        self.stack_extend_from_slice(&arr[0..length_required]);
                    }
                }


                OpCode::TupUnpack => {
                    let Value::Tup(tup) = self.value_stack.pop().unwrap() else { unreachable!("last value was not a tuple") };

                    self.stack_extend_from_slice(&tup);
                }


                OpCode::CallFn => {
                    let arg_count = Self::read_next_opnum(frame, chunk);
                    let callee = self.value_stack.pop().unwrap();

                    let first_arg_index = self.value_stack.len() - arg_count;
                    let mut args: Vec<Value> = self.value_stack.drain(first_arg_index..).collect();

                    match callee {
                        Value::NativeFn(native_fn) => {
                            let result = native_fn(&mut args)?;
                            self.stack_push_val(result);
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
                println!("{}", join_slice_to_string(&self.value_stack, ", "));
            }
        }
    }


    fn read_byte(frame: &mut CallFrame, chunk: &BytecodeChunk) -> u8 {
        let byte = chunk.codes[frame.ip];
        frame.ip += 1;
        byte
    }
    #[inline(always)]
    fn read_bytes<const N: usize>(frame: &mut CallFrame, chunk: &BytecodeChunk) -> [u8; N] {
        let end = frame.ip + N;
        let bytes = chunk.codes[frame.ip..end]
            .try_into()
            .expect("Bytecode ended unexpectedly");
        frame.ip = end;
        bytes
    }

    pub fn read_next_instruction(frame: &mut CallFrame, chunk: &BytecodeChunk) -> OpCode {
        OpCode::from_repr(Self::read_byte(frame, chunk)).expect("Not a valid OpCode")
        // unsafe { std::mem::transmute(self.read_byte()) }
    }

    pub fn read_next_opnum(frame: &mut CallFrame, chunk: &BytecodeChunk) -> usize {
        let next_byte = Self::read_byte(frame, chunk);
        // small number
        if next_byte < u8::MAX { next_byte as usize }
        // wide number
        else { usize::from_ne_bytes(Self::read_bytes(frame, chunk)) }
    }

    fn stack_push_val(&mut self, val: Value) {
        let cap_before_push = self.value_stack.capacity();
        self.value_stack.push(val);
        assert!(cap_before_push <= self.value_stack.capacity(), "Stack Overflow limit of {cap_before_push} reached!");
    }
    fn stack_extend_from_slice(&mut self, slice: &[Value]) {
        let cap_before_push = self.value_stack.capacity();
        self.value_stack.extend_from_slice(slice);
        assert!(cap_before_push <= self.value_stack.capacity(), "Stack Overflow limit of {cap_before_push} reached!");
    }




    fn val_to_string(val: &Value) -> String {
        match val {
            Value::Num(num) => num.to_string(),
            Value::Str(str) => str.clone(),
            Value::Bool(bool) => bool.to_string(),

            Value::Arr(arr) => {
                let str_results: Vec<String> = arr.iter().map(Self::val_to_string).collect();
                String::from("[") + &str_results.join(", ") + "]"
            }
            Value::Tup(tup) => {
                let str_results: Vec<String> = tup.iter().map(Self::val_to_string).collect();
                String::from("(") + &str_results.join(", ") + ")"
            }

            Value::Void => "void".to_string(),
            _ => panic!("Literal {val:?} cannot be converted into a string.")
        }
    }
}