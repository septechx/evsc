#[derive(Debug, Clone, Default)]
pub struct CheckOptions {
    pub no_link: bool,
}

impl CheckOptions {
    pub fn new(no_link: bool) -> Self {
        Self { no_link }
    }
}
