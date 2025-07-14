pub fn assert_err<T, U: std::fmt::Debug>(value: Result<T, U>) -> T {
    match value {
        Ok(val) => val,
        Err(err) => {
            assert!(false, "{:?}", err);
            panic!("after assert!");
        }
    }
}
