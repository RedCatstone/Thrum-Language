use crate::{ast_structure::{TypeKind, Value}, vm::RuntimeError};

pub type NativeFn = fn(&mut [Value]) -> Result<Value, RuntimeError>;

pub fn get_native_lib() -> Vec<(String, TypeKind, Value)> {
    vec![
        ("print".to_string(), TypeKind::Fn { param_types: vec![TypeKind::Str], return_type: Box::new(TypeKind::Void) }, Value::NativeFn(native_print))
    ]
}



pub fn native_print(val: &mut [Value]) -> Result<Value, RuntimeError> {
    let Value::Str(str) = &val[0] else { unreachable!() };
    println!("{}", str);
    Ok(Value::Void)
}