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
             .map(|(i, &s)| (s.to_string(), i as u32))
             .collect::<HashMap<String, u32>>()
    }};
}
