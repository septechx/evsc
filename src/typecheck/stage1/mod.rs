mod checks;
use checks::*;

use crate::{
    ast::Statement,
    stage,
    typecheck::{Check, Stage},
};

stage!(1 : InternalAttributeChecker);
