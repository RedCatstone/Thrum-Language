use std::collections::HashMap;

use crate::{ErrType, typing::{Type, TypeArena, TypeId}, vm_compiling::RuntimeValue};

pub struct ThrumValue {
    pub typ: Type,
    pub val: RuntimeValue,
    pub is_prelude: bool,
}

#[derive(Default)]
pub struct ThrumModule {
    pub sub_modules: HashMap<String, Self>,
    pub values: HashMap<String, ThrumValue>,
}



pub fn get_native_lib(_type_arena: &mut TypeArena) -> ThrumModule {
    let mut std_module = ThrumModule::default();

    let mut io_module = ThrumModule::default();
    io_module.values.insert("print".to_string(), ThrumValue {
        typ: Type::Fn { param_types: vec![TypeId::STR], return_type: TypeId::VOID },
        val: RuntimeValue::NativeFn(native_print),
        is_prelude: true,
    });
    io_module.values.insert("panic".to_string(), ThrumValue {
        typ: Type::Fn { param_types: vec![TypeId::STR], return_type: TypeId::NEVER },
        val: RuntimeValue::NativeFn(native_panic),
        is_prelude: true,
    });
    std_module.sub_modules.insert("io".to_string(), io_module);

    std_module.values.insert("num".to_string(), ThrumValue { typ: Type::MetaType, val: RuntimeValue::Type(TypeId::NUM), is_prelude: true });
    std_module.values.insert("bool".to_string(), ThrumValue { typ: Type::MetaType, val: RuntimeValue::Type(TypeId::BOOL), is_prelude: true });
    

    // let range_tup_type = type_arena.add_type(Type::Tup(vec![
    //     TypeTuple { label: "start".to_string(), typ: TypeId::NUM, },
    //     TypeTuple { label: "end".to_string(), typ: TypeId::NUM, },
    // ]));
    // let range_tup_custom_type = type_arena.add_type(Type::CustomType(range_tup_type));
    // std_module.values.insert("Range".to_string(), ThrumValue { typ: Type::MetaType, val: RuntimeValue::Type(range_tup_custom_type), is_prelude: true });

    std_module
}



pub fn native_print(val: &[RuntimeValue]) -> Result<RuntimeValue, ErrType> {
    let RuntimeValue::Str(str) = &val[0] else { panic!("function called with wrong argument...") };
    println!("{str}");

    Ok(RuntimeValue::Void)
}

pub fn native_panic(val: &[RuntimeValue]) -> Result<RuntimeValue, ErrType> {
    let RuntimeValue::Str(str) = &val[0] else { panic!("function called with wrong argument...") };
    println!("{str}");

    Err(ErrType::RuntimeError { msg: str.clone() })
}

pub fn native_str_len(val: &[RuntimeValue]) -> Result<RuntimeValue, ErrType> {
    let RuntimeValue::Str(str) = &val[0] else { panic!("function called with wrong argument...") };
    Ok(RuntimeValue::Num(str.len() as f64))
}