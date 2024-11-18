use std::{cell::RefCell, collections::HashMap, fmt::{Display, Formatter}, rc::Rc};

use itertools::Itertools;
use lantern_parse::{error::MismatchedTypesError, tokenizer::Ident};

use crate::{runtime_error, Scope, LanternFunction, LanternFunctionArg, LanternFunctionBody, LanternType, LanternValue, RuntimeError, ScopeMut};

macro_rules! impl_type {
    ($ty: ty $( [ where $($g_ty: tt)* ] )? {
        $l_ty: expr,
        from $f_pat: pat => $f_expr: expr,
        from $($i_pat: pat)? => $i_expr: expr,
        $(
            $s_pat: pat = {$(
                $fi_name: literal : $fi_ty: expr => $fi_get: expr,
            )*},
            $fn_pat: pat = {$(
                $(<$fn_ty: ty>)? $fn_name: literal ( $($fn_arg_pat: pat in $fn_arg_name: literal : $fn_arg_ty: ty),* ) -> $fn_ret_type: ty
                        $fn_body: block,
            )*}
        )?
    }) => {
        impl$(<$($g_ty)*>)? LanternRecord for $ty {
            fn r#type() -> LanternType {
                $l_ty
            }
            fn into_value(self) -> LanternValue {
                $( let $i_pat = self; )?
                $i_expr
            }
            fn from_value(value: LanternValue) -> Option<Self> {
                match value {
                    $f_pat => $f_expr,
                    #[allow(unreachable_patterns)]
                    _ => None,
                }
            }
            fn field(name: &str) -> Option<LanternField<Self>> {
                match name {
                    $($(
                        $fi_name => Some(LanternField { r#type: $fi_ty, get: Box::new(|$s_pat: &Self| $fi_get) }),
                    )?)*
                    _ => None,
                }
            }
            fn method(name: &str) -> Option<LanternMethod> {
                match name {
                    $($(
                        $fn_name => {
                            fn inner(scope: ScopeMut) -> Result<LanternValue, RuntimeError> {
                                let scope = scope.borrow();
                                let $fn_pat = <impl_type!(@t $ty $(, $fn_ty)?)>::from_value(scope.variable("self").expect("self method arg").value)
                                    .expect("self arg type");
                                $(
                                    let $fn_arg_pat = <$fn_arg_ty as LanternRecord>::from_value(scope.variable($fn_arg_name).expect("method arg").value)
                                        .expect("method arg type");
                                )*
                                $fn_body.map(<$fn_ret_type as LanternRecord>::into_value)
                            }

                            let name = $fn_name.to_string();
                            let args = vec![
                                $(LanternFunctionArg { name: $fn_arg_name.to_string(), r#type: <$fn_arg_ty as LanternRecord>::r#type() },)*
                            ];

                            let body = LanternFunctionBody::Native(inner);
                            Some(LanternMethod {
                                function: LanternFunction {
                                    name,
                                    args,
                                    ret_type: <$fn_ret_type as LanternRecord>::r#type(),
                                    body,
                                    scope: Rc::new(RefCell::new(Scope::with_blank_head())),
                                }
                            })
                        },
                    )?)*
                    _ => None,
                }
            }
        }
    };

    (@t $ty: ty, $p_ty: ty) => {
        impl_type!(@t $p_ty)
    };

    (@t $ty: ty) => {
        $ty
    }
}

pub trait LanternRecord {
    fn r#type() -> LanternType;

    fn into_value(self) -> LanternValue;

    fn from_value(value: LanternValue) -> Option<Self> where Self: Sized;

    fn field(name: &str) -> Option<LanternField<Self>> where Self: Sized;

    fn method(name: &str) -> Option<LanternMethod>;
}

pub struct LanternField<T> {
    pub r#type: LanternType,
    pub get: Box<dyn Fn(&T) -> LanternValue>,
}

impl<T> std::fmt::Debug for LanternField<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LanternField")
            .field("type", &self.r#type)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternMethod {
    pub function: LanternFunction,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternRecordFrame {
    pub ident: Ident,
    pub fields: Vec<LanternFunctionArg>,
    pub methods: Vec<LanternMethod>,
    pub private_init: bool,
}

impl LanternRecordFrame {
    pub fn init(frame: Rc<LanternRecordFrame>, args: Vec<LanternValue>) -> Result<LanternCustomRecord, RuntimeError> {
        if frame.fields.len() != args.len() {
            return Err(runtime_error!("rec contains {} fields, but only {} was supplied", frame.fields.len(), args.len()));
        };

        let fields = frame.fields.iter()
            .zip(args)
            .map(|(field_arg, arg)| {
                if !arg.r#type().applies_to(&field_arg.r#type) {
                    return Err(RuntimeError::new(MismatchedTypesError { expected: field_arg.r#type.clone(), found: arg.r#type() }));
                };

                Ok((field_arg.name.clone(), arg))
            })
            .collect::<Result<HashMap<String, LanternValue>, RuntimeError>>()?;
        let methods = frame.methods.iter().map(|method| (method.function.name.clone(), method.clone())).collect();

        Ok(LanternCustomRecord { r#type: LanternType::Custom(frame.ident.name.clone()), fields, methods })
    }
}

#[derive(Debug, Clone)]
pub struct LanternCustomRecord {
    pub r#type: LanternType,
    pub fields: HashMap<String, LanternValue>,
    pub methods: HashMap<String, LanternMethod>,
}

impl PartialEq for LanternCustomRecord {
    fn eq(&self, other: &Self) -> bool {
        self.r#type == other.r#type &&
            self.fields == other.fields
    }
}

impl Display for LanternCustomRecord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.r#type)?;
        self.fields.iter()
            .map(|(name, value)| format!("{name}: {value}"))
            .intersperse(", ".to_string())
            .try_for_each(|err| f.write_str(&err))?;
        write!(f, ")")?;
        Ok(())
    }
}

impl LanternCustomRecord {
    pub fn into_value(self) -> LanternValue {
        LanternValue::Custom(self)
    }

    pub fn from_value(value: LanternValue) -> Option<Self> {
        if let LanternValue::Custom(custom) = value {
            Some(custom)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternAny(pub LanternValue);

impl From<LanternValue> for LanternAny {
    fn from(value: LanternValue) -> Self {
        Self(value)
    }
}

impl From<LanternAny> for LanternValue {
    fn from(value: LanternAny) -> Self {
        value.0
    }
}

impl_type!(LanternAny {
    LanternType::Any,
    from value => Some(value.into()),
    from value => value.into(),
});

impl_type!(String {
    LanternType::String,
    from LanternValue::String(str) => Some(str),
    from str => LanternValue::String(str),
    str = {
        "len": LanternType::Num => LanternValue::Num(str.len() as f64),
    },
    str = {
        "char_at"(i in "i": f64) -> String {
            str.chars().nth(i as usize).map(|char| char.to_string()).ok_or_else(|| runtime_error!("index out of bounds"))
        },
        "parse_int"() -> Option<f64> {
            Ok(str.parse().ok())
        },
    }
});

impl_type!(f64 {
    LanternType::Num,
    from LanternValue::Num(num) => Some(num),
    from num => LanternValue::Num(num),
    _ = { },
    num = {
        "sqrt"() -> f64 {
            Ok(num.sqrt())
        },
    }
});

impl_type!(bool {
    LanternType::Bool,
    from LanternValue::Bool(bool) => Some(bool),
    from bool => LanternValue::Bool(bool),
});

impl_type!(() {
    LanternType::Nil,
    from LanternValue::Null => Some(()),
    from => LanternValue::Null,
});

impl_type!(Option<T> [ where T: LanternRecord ] {
    LanternType::Option(Some(Box::new(T::r#type()))),
    from LanternValue::Option(option) => {
        match option {
            Some(value) => T::from_value(*value).map(Some),
            None => Some(None),
        }
    },
    from option => LanternValue::Option(option.map(|val| Box::new(val.into_value()))),
    _ = { },
    option = {
        <Option<LanternAny>>"is_some"() -> bool {
            Ok(option.is_some())
        },
        <Option<LanternAny>>"is_none"() -> bool {
            Ok(option.is_none())
        },
    }
});

impl_type!(Result<T, E> [ where T: LanternRecord, E: LanternRecord ] {
    LanternType::Result(Some(Box::new(T::r#type())), Some(Box::new(E::r#type()))),
    from LanternValue::Result(result) => {
        match result {
            Ok(ok) => T::from_value(*ok).map(Ok),
            Err(err) => E::from_value(*err).map(Err),
        }
    },
    from result => {
        match result {
            Ok(ok) => LanternValue::Result(Ok(Box::new(ok.into_value()))),
            Err(err) => LanternValue::Result(Err(Box::new(err.into_value()))),
        }
    },
    _ = { },
    result = {
        <Result<LanternAny, LanternAny>>"is_ok"() -> bool {
            Ok(result.is_ok())
        },
        <Result<LanternAny, LanternAny>>"is_err"() -> bool {
            Ok(result.is_err())
        },
    }
});

