#![allow(clippy::needless_raw_string_hashes)]
pub use thrum_engine::run_code;

#[macro_export]
macro_rules! test {
    ($code:expr, $expected:expr) => {
        match common::run_code($code) {
            Ok(val) => assert_eq!(val, $expected),
            Err(errs) => panic!("Code failed to compile!\nErrors: {errs:#?}"),
        }
    };
}

#[macro_export]
macro_rules! test_err {
    ($code:expr, $err_pat:pat) => {
        match common::run_code($code) {
            Ok(val) => panic!("Code should have failed, but it successfully evaluated to: {val:?}"),
            Err(errs) => assert!(
                errs.iter().any(|e| matches!(e, $err_pat)),
                "Expected error matching {}, instead found errors:\n{:?}", stringify!($err_pat), errs
            )
        }
    };
}