use std::{fmt, u8};

use crate::{ast_structure::Value, pretty_printing::join_slice_to_string, to_bytecode::{Bytecode, OpCode}};


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
            let right = $self.pop_value();
            let left = $self.pop_value();
            match (left, right) {
                // (Value::Num(l), Value::Num(r)) => self.push_value(Value::Num(l + r))
                ($left_pattern, $right_pattern) => $self.push_value($result_expr),
                (left, right) => unreachable!("Operands {left}, {right} cannot be ($result_expr)-ed"),
            }
        }
    };
}
macro_rules! run_prefix_op {
    ($self:expr, $operand_pattern:pat => $result_expr:expr) => {
        {
            if $self.value_stack.is_empty() { unreachable!("Stack underflow for prefix operation.") }
            let operand = $self.pop_value();
            match operand {
                // Value::Num(n) => self.push_value(Value::Num(-n))
                $operand_pattern => $self.push_value($result_expr),
                operand => unreachable!("Operand {operand} cannot be ($result_expr)-ed"),
            }
        }
    };
}


#[derive(Default)]
pub struct VM {
    bytecode: Bytecode,
    ip: usize,
    pub value_stack: Vec<Value>
}

impl VM {
    pub fn new() -> Self {
        VM::default()
    }
    
    pub fn load_bytecode(&mut self, bytecode: Bytecode) {
        self.bytecode = bytecode;
        self.ip = 0;
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let instruction = self.read_next_instruction();
            match instruction {
                OpCode::Return => {
                    return Ok(());
                }

                OpCode::GetConstant => {
                    let slot = self.read_next_operand();
                    let constant = self.bytecode.constants[slot].clone();
                    self.push_value(constant);
                }

                // Numbers
                OpCode::AddNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l + r)),
                OpCode::SubtractNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l - r)),
                OpCode::MultiplyNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l * r)),
                OpCode::DivideNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l / r)),
                OpCode::ModuloNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l % r)),
                OpCode::ExponentNum => run_infix_op!(self, Value::Num(l), Value::Num(r) => Value::Num(l.powf(r))),
                OpCode::NegateNum => run_prefix_op!(self, Value::Num(n) => Value::Num(-n)),

                // Strings
                OpCode::AddStr => run_infix_op!(self, Value::Str(l), Value::Str(r) => Value::Str(l + &r)),
                OpCode::TemplateString => {
                    let elements_count = self.read_next_operand();
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let mut string = String::new();
                    for val in self.value_stack.drain(cut_off_index..) {
                        string += &VM::val_to_string(val);
                    }
                    self.push_value(Value::Str(string));
                }

                // bools
                OpCode::AndBool => run_infix_op!(self, Value::Bool(l), Value::Bool(r) => Value::Bool(l && r)),
                OpCode::OrBool => run_infix_op!(self, Value::Bool(l), Value::Bool(r) => Value::Bool(l || r)),
                OpCode::NegateBool => run_prefix_op!(self, Value::Bool(b) => Value::Bool(!b)),

                // Arrays
                OpCode::CreateArray => {
                    let elements_count = self.read_next_operand();
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let arr = self.value_stack.drain(cut_off_index..).collect();
                    self.push_value(Value::Arr(arr));
                }

                // Tuples
                OpCode::CreateTuple => {
                    let elements_count = self.read_next_operand();
                    let cut_off_index = self.value_stack.len() - elements_count;
                    let tup = self.value_stack.drain(cut_off_index..).collect();
                    self.push_value(Value::Tup(tup));
                }

                // Any
                // relies on `impl PartialEq for Value`
                OpCode::CmpEqual => {
                    let right = self.pop_value();
                    let left = self.pop_value();
                    self.push_value(Value::Bool(left == right));
                }
                // relies on `impl PartialOrd for Value`
                OpCode::CmpLess => {
                    let right = self.pop_value();
                    let left = self.pop_value();
                    self.push_value(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Less)));
                }
                OpCode::CmpGreater => {
                    let right = self.pop_value();
                    let left = self.pop_value();
                    self.push_value(Value::Bool(left.partial_cmp(&right) == Some(std::cmp::Ordering::Greater)));
                }



                OpCode::GetLocal => {
                    let slot = self.read_next_operand();
                    let value = self.value_stack[slot].clone();
                    self.push_value(value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_next_operand();
                    let value = self.pop_value();
                    self.value_stack[slot] = value;
                }

                OpCode::PopValue => {
                    self.value_stack.pop();
                }
                OpCode::DropValue => {
                    let dropped_value = self.pop_value();
                    self.pop_value();
                    self.push_value(dropped_value);
                }

                OpCode::DuplicateTop => {
                    let value = self.value_stack.last().unwrap().clone();
                    self.push_value(value);
                }


                OpCode::Jump => {
                    let offset = self.read_next_operand();
                    self.ip += offset;
                }
                OpCode::JumpBack => {
                    let offset = self.read_next_operand();
                    self.ip -= offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_next_operand();
                    if let Value::Bool(bool) = self.pop_value() {
                        if !bool { self.ip += offset }
                    }
                    else { unreachable!("Encountered a non-boolean value...") }
                }


                OpCode::ArrIndex => {
                    let index = self.pop_value();
                    let arr = self.pop_value();
                    match (arr, index) {
                        (Value::Arr(a), Value::Num(i)) => {
                            if i.fract() > 0.0 {
                                return Err(RuntimeError::error(format!("Cannot index arr with a non-integer number: {}", i)))
                            }
                            let arr_element = {
                                if i.is_sign_positive() {
                                    let corrected_index = i as usize;
                                    match a.get(corrected_index) {
                                        Some(x) => x.clone(),
                                        None => return Err(RuntimeError::error(format!("Index {corrected_index} is out of bounds for arr of length {}.", a.len())))
                                    }
                                }
                                else {
                                    let x = -i as usize;
                                    if x > a.len() {
                                        return Err(RuntimeError::error(format!("Index {i} is out of bounds for arr of length {}.", a.len())))
                                    }
                                    else {
                                        a[a.len() - x].clone()
                                    }
                                }
                            };
                            self.push_value(arr_element);
                        }
                        (a, i) => unreachable!("not an array, index pair: {}, {}", a, i)
                    }
                }

                OpCode::GetArrIndex => {
                    let slot = self.read_next_operand();
                    let index = self.read_next_operand();
                    let Value::Arr(arr) = self.value_stack.get(slot).unwrap()
                    else { unreachable!("${slot} was not an array") };
                    if let Some(val) = arr.get(index) {
                        self.push_value(val.clone());
                    }
                    else {
                        return Err(RuntimeError::error(format!("Index {index} is out of bounds for arr {:?}", arr)));
                    }
                }
                OpCode::GetTupIndex => {
                    let slot = self.read_next_operand();
                    let index = self.read_next_operand();
                    let Value::Tup(tup) = self.value_stack.get(slot).unwrap()
                    else { unreachable!("${slot} was not a tuple") };
                    let val = tup[index].clone();
                    self.push_value(val);
                }
            }
            println!("{:?} -> {}", instruction, join_slice_to_string(&self.value_stack, ", "));
        }
    }


    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.codes[self.ip];
        self.ip += 1;
        byte
    }
    fn read_bytes<const N: usize>(&mut self) -> [u8; N] {
        let end = self.ip + N;
        let bytes = self.bytecode.codes[self.ip..end]
            .try_into()
            .expect("Bytecode ended unexpectedly");
        self.ip = end;
        bytes
    }

    pub fn read_next_instruction(&mut self) -> OpCode {
        self.read_byte().try_into().expect("Not a valid OpCode")
        // unsafe { std::mem::transmute(self.read_byte()) }
    }

    pub fn read_next_operand(&mut self) -> usize {
        let next_byte = self.read_byte();
        if next_byte < u8::MAX {
            // normal number, return it
            next_byte as usize
        }
        else {
            // wide number
            usize::from_ne_bytes(self.read_bytes())
        }
    }

    fn push_value(&mut self, val: Value) { self.value_stack.push(val); }
    fn pop_value(&mut self) -> Value    { self.value_stack.pop().unwrap() }




    fn val_to_string(val: Value) -> String {
        match val {
            Value::Num(num) => num.to_string(),
            Value::Str(str) => str,
            Value::Bool(bool) => bool.to_string(),

            Value::Arr(arr) => {
                let str_results: Vec<String> = arr.into_iter().map(|x| VM::val_to_string(x)).collect();
                String::from("[") + &str_results.join(", ") + "]"
            }
            Value::Tup(tup) => {
                let str_results: Vec<String> = tup.into_iter().map(|x| VM::val_to_string(x)).collect();
                String::from("(") + &str_results.join(", ") + ")"
            }
            _ => panic!("Literal {:?} cannot be converted into a string.", val)
        }
    }
}