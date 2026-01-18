use std::fmt::Display;

use crate::span::{ModuleId, Span};

#[derive(Debug, Clone)]
pub struct TokenStream(pub Box<[Token]>);

#[derive(Debug, Clone, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub module_id: ModuleId,
    pub value: Box<str>,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

macro_rules! define_tokens {
    (
        reserved: [$( $reserved:ident ),* $(,)?],
        symbols: [$( $symbol:ident => $symbol_str:literal ),* $(,)?],
        literals: [$( $literal:ident => $literal_str:literal ),* $(,)?],
        special: [$( $special:ident => $special_str:literal ),* $(,)?]
    ) => {
        #[derive(Debug, Clone, PartialOrd, Ord, Hash, Eq, PartialEq, Copy)]
        pub enum TokenKind {
            $( $reserved ),*,
            $( $symbol ),*,
            $( $literal ),*,
            $( $special ),*
        }

        pub fn lookup_reserved(ident: &str) -> Option<TokenKind> {
            use TokenKind as T;
            static RESERVED_KEYWORDS: std::sync::OnceLock<std::collections::HashMap<Box<str>, TokenKind>> = std::sync::OnceLock::new();
            let lu = RESERVED_KEYWORDS.get_or_init(|| {
                let mut m = std::collections::HashMap::new();
                $(
                    m.insert(stringify!($reserved).to_lowercase().into_boxed_str(), T::$reserved);
                )*
                m
            });
            lu.get(ident).cloned()
        }

        impl std::fmt::Display for TokenKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use TokenKind as T;
                match self {
                    $( T::$reserved => write!(f, "{}", stringify!($reserved).to_lowercase()), )*
                    $( T::$symbol => write!(f, "{}", $symbol_str), )*
                    $( T::$literal => write!(f, "{}", $literal_str), )*
                    $( T::$special => write!(f, "{}", $special_str), )*
                }
            }
        }
    };
}

define_tokens! {
    reserved: [Let, True, False, Struct, Fn, Return, Pub, Static, Mut, Extern, Interface, Macro, If, Else, While, For, Break, Continue, As, Import],
    symbols: [
        Semicolon => ";",
        Bar => "|",
        Colon => ":",
        Arrow => "->",
        OpenCurly => "{",
        CloseCurly => "}",
        OpenParen => "(",
        CloseParen => ")",
        Dot => ".",
        Question => "?",
        Equals => "=",
        PlusEquals => "+=",
        MinusEquals => "-=",
        StarEquals => "*=",
        SlashEquals => "/=",
        PercentEquals => "%=",
        Underscore => "_",
        OpenBracket => "[",
        CloseBracket => "]",
        Hash => "#",
        Comma => ",",
        Plus => "+",
        Dash => "-",
        Star => "*",
        Slash => "/",
        Percent => "%",
        Reference => "&",
        DotDot => "..",
        EqualsEquals => "==",
        NotEquals => "!=",
        Less => "<",
        More => ">",
        LessEquals => "<=",
        MoreEquals => ">=",
        And => "&&",
        Or => "||",
        Pipe => "|>",
        ColonColon => "::",
        Dollar => "$"
    ],
    literals: [Identifier => "identifier", StringLiteral => "string literal", Number => "number"],
    special: [Eof => "<eof>", Illegal => "<illegal>"]
}
