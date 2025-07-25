//! Take a list of Results, and try to unwrap all of them. Returns a Result
//! which either contains every Result unwrapped, or every error that exists
//! within the array.

pub fn try_all<U, E>(list: Vec<Result<U, E>>) -> Result<Vec<U>, Vec<E>> {
    let mut successes = Vec::with_capacity(list.len());
    let mut failures = Vec::with_capacity(list.len());

    for item in list {
        match item {
            Ok(s) => successes.push(s),
            Err(e) => failures.push(e),
        }
    }

    if failures.len() > 0 {
        Err(failures)
    } else {
        Ok(successes)
    }
}

pub fn maybe<T, U>(lhs: Option<U>, rhs: Option<U>, fun: T) -> Option<U>
where
    T: FnOnce(U, U) -> U,
{
    if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
        Some(fun(lhs, rhs))
    } else {
        None
    }
}
