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
             .collect::<std::collections::HashMap<Box<str>, u32>>()
    }};
}

#[macro_export]
macro_rules! logln {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| e.get()) {
            println!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! log {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| e.get()) {
            print!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! elogln {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| e.get()) {
            eprintln!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! elog {
    ($fmt:expr $(, $($arg:tt)*)?) => {
        if $crate::ENABLE_PRINTING.with(|e| e.get()) {
            eprint!($fmt $(, $($arg)*)?);
        }
    };
}

#[macro_export]
macro_rules! emit_at {
    ($builder:expr, $span:expr, $module_id:expr, $msg:expr, $info:expr, $highlight:expr) => {
        $crate::ERRORS.with(|e| -> anyhow::Result<()> {
            let span = $span;
            let module_id = $module_id;
            let msg = $msg;
            let builder = $builder(msg)
                .add_widget($crate::errors::widgets::LocationWidget::new(
                    span, module_id,
                )?)
                .add_widget($crate::errors::widgets::CodeWidget::new(
                    span,
                    module_id,
                    $highlight,
                )?);
            let builder = if let Some(info) = $info {
                builder.add_widget($crate::errors::widgets::InfoWidget::new(
                    span, module_id, info,
                )?)
            } else {
                builder
            };
            e.borrow_mut().add(builder);
            Ok(())
        })
    };
}

#[macro_export]
macro_rules! error_at {
    ($span:expr, $module_id:expr, $msg:expr $(,)?) => {
        $crate::emit_at!(
            $crate::errors::builders::error,
            $span,
            $module_id,
            $msg,
            None::<Box<str>>,
            $crate::errors::widgets::HighlightType::Error
        )
    };
    ($token:expr, $msg:expr $(,)?) => {{
        let token = $token;
        $crate::error_at!(token.span, token.module_id, $msg)
    }};
}

#[macro_export]
macro_rules! warning_at {
    ($span:expr, $module_id:expr, $msg:expr $(,)?) => {
        $crate::emit_at!(
            $crate::errors::builders::warning,
            $span,
            $module_id,
            $msg,
            None::<Box<str>>,
            $crate::errors::widgets::HighlightType::Warning
        )
    };
    ($token:expr, $msg:expr $(,)?) => {{
        let token = $token;
        $crate::warning_at!(token.span, token.module_id, $msg)
    }};
}

#[macro_export]
macro_rules! fatal_at {
    ($span:expr, $module_id:expr, $msg:expr $(,)?) => {{
        $crate::emit_at!(
            $crate::errors::builders::fatal,
            $span,
            $module_id,
            $msg,
            None::<Box<str>>,
            $crate::errors::widgets::HighlightType::Error
        )
        .expect("failed to create error");
        unreachable!()
    }};
    ($token:expr, $msg:expr $(,)?) => {{
        let token = $token;
        $crate::fatal_at!(token.span, token.module_id, $msg)
    }};
}

#[macro_export]
macro_rules! error_at_with_info {
    ($span:expr, $module_id:expr, $msg:expr, $info:expr $(,)?) => {
        $crate::emit_at!(
            $crate::errors::builders::error,
            $span,
            $module_id,
            $msg,
            Some($info),
            $crate::errors::widgets::HighlightType::Error
        )
    };
    ($token:expr, $msg:expr, $info:expr $(,)?) => {{
        let token = $token;
        $crate::error_at_with_info!(token.span, token.module_id, $msg, $info)
    }};
}

#[macro_export]
macro_rules! warning_at_with_info {
    ($span:expr, $module_id:expr, $msg:expr, $info:expr $(,)?) => {
        $crate::emit_at!(
            $crate::errors::builders::warning,
            $span,
            $module_id,
            $msg,
            Some($info),
            $crate::errors::widgets::HighlightType::Warning
        )
    };
    ($token:expr, $msg:expr, $info:expr $(,)?) => {{
        let token = $token;
        $crate::warning_at_with_info!(token.span, token.module_id, $msg, $info)
    }};
}

#[macro_export]
macro_rules! fatal_at_with_info {
    ($span:expr, $module_id:expr, $msg:expr, $info:expr $(,)?) => {{
        $crate::emit_at!(
            $crate::errors::builders::fatal,
            $span,
            $module_id,
            $msg,
            Some($info),
            $crate::errors::widgets::HighlightType::Error
        )
        .expect("failed to create error");
        unreachable!()
    }};
    ($token:expr, $msg:expr, $info:expr $(,)?) => {{
        let token = $token;
        $crate::fatal_at_with_info!(token.span, token.module_id, $msg, $info)
    }};
}

#[macro_export]
macro_rules! error {
    ($msg:expr $(,)?) => {
        $crate::ERRORS.with(|e| {
            e.borrow_mut().add($crate::errors::builders::error($msg));
        })
    };
}

#[macro_export]
macro_rules! warning {
    ($msg:expr $(,)?) => {
        $crate::ERRORS.with(|e| {
            e.borrow_mut().add($crate::errors::builders::warning($msg));
        })
    };
}

#[macro_export]
macro_rules! fatal {
    ($msg:expr $(,)?) => {{
        $crate::ERRORS.with(|e| {
            e.borrow_mut().add($crate::errors::builders::fatal($msg));
        });
        unreachable!()
    }};
}

#[macro_export]
macro_rules! warning_with_example {
    ($msg:expr, $code:expr, $line:expr, $code_type:expr $(,)?) => {
        $crate::ERRORS.with(|e| {
            e.borrow_mut()
                .add($crate::errors::builders::warning($msg).add_widget(
                    $crate::errors::widgets::CodeExampleWidget::new($code, $line, $code_type),
                ));
        })
    };
}
