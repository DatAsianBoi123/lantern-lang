use crate::ScopeMut;

#[derive(Debug, Clone, PartialEq)]
pub struct LanternModule {
    pub name: String,
    pub scope: ScopeMut,
}

