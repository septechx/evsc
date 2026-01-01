/// Creates a HashMap from the given field names.
/// ```rust,ignore
/// struct_fields!(a, b, c)
/// ```
#[macro_export]
macro_rules! struct_fields {
    ( $( $name:ident ),* $(,)? ) => {{
        let names = [ $( stringify!($name) ),* ];
        names.iter()
             .enumerate()
             .map(|(i, &s)| (s.into(), i as u32))
             .collect::<HashMap<Box<str>, u32>>()
    }};
}

#[macro_export]
macro_rules! logln {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| *e.borrow()) {
            println!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! log {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| *e.borrow()) {
            print!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! elogln {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| *e.borrow()) {
            eprintln!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! elog {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| *e.borrow()) {
            eprint!($fmt $(, $($arg)*)?);
        }
    };
}
