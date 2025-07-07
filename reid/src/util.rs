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
