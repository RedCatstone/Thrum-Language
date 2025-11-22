use std::collections::HashMap;

use crate::{parsing::ast_structure::{DefinedTypeKind, TypeKind, Value}, vm_evaluating::RuntimeError};

pub type NativeFn = fn(&mut [Value]) -> Result<Value, RuntimeError>;

// this can be a function or a const
#[derive(Clone)]
pub struct ThrumValue {
    pub typ: TypeKind,
    pub val: Value,
    pub is_prelude: bool,
}
#[derive(Clone)]
pub struct ThrumType {
    pub typ: DefinedTypeKind,
    pub values: HashMap<String, ThrumValue>
}

#[derive(Default)]
pub struct ThrumModule {
    pub sub_modules: HashMap<String, ThrumModule>,
    pub values: HashMap<String, ThrumValue>,
    pub types: HashMap<String, ThrumType>,
}







pub fn get_native_lib() -> ThrumModule {
    let mut std_module = ThrumModule::default();

    let mut io_module = ThrumModule::default();
    io_module.values.insert("print".to_string(), ThrumValue {
        typ: TypeKind::Fn { param_types: vec![TypeKind::Str], return_type: Box::new(TypeKind::Void) },
        val: Value::NativeFn(native_print),
        is_prelude: true,
    });
    std_module.sub_modules.insert("io".to_string(), io_module);

    
    let str_type = ThrumType {
        typ: DefinedTypeKind::Native(TypeKind::Str),
        values: vec![
            ("len".to_string(), ThrumValue {
                typ: TypeKind::Fn { param_types: vec![TypeKind::Str], return_type: Box::new(TypeKind::Num) },
                val: Value::NativeFn(native_str_len),
                is_prelude: false,
            })
        ].into_iter().collect(),
    };

    std_module.types.insert("str".to_string(), str_type);

    std_module
}



pub fn native_print(val: &mut [Value]) -> Result<Value, RuntimeError> {
    let Value::Str(str) = &val[0] else { unreachable!() };
    println!("{}", str);

    Ok(Value::Void)
}

pub fn native_str_len(val: &mut [Value]) -> Result<Value, RuntimeError> {
    let Value::Str(str) = &val[0] else { unreachable!() };
    Ok(Value::Num(str.len() as f64))
}