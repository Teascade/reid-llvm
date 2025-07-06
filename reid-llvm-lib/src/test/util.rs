use super::{
    Type,
    builder::{Builder, InstructionValue},
};

pub fn match_types(
    lhs: &InstructionValue,
    rhs: &InstructionValue,
    builder: &Builder,
) -> Result<Type, ()> {
    let lhs_type = lhs.get_type(&builder);
    let rhs_type = rhs.get_type(&builder);
    if let (Ok(lhs_t), Ok(rhs_t)) = (lhs_type, rhs_type) {
        if lhs_t == rhs_t { Ok(lhs_t) } else { Err(()) }
    } else {
        Err(())
    }
}
