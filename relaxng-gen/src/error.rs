use core::fmt;

use crate::rustgen::FieldTy;

#[derive(Debug, Clone)]
pub(crate) enum Error {
    Reconcile(FieldTy, FieldTy),
    Other,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Reconcile(ty1, ty2) => {
                write!(f, "Type reconciliation error: {ty1:#?} and {ty2:#?}")
            }
            Error::Other => {
                write!(f, "Other error")
            }
        }
    }
}

pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;
