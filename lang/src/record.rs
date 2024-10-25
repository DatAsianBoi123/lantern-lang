use std::{cell::RefCell, collections::HashMap, fmt::{Display, Formatter}, rc::Rc};

use itertools::Itertools;
use lantern_parse::tokenizer::Ident;

use crate::{error::MismatchedTypes, runtime_error, LanternFunction, LanternFunctionArg, LanternFunctionBody, LanternType, LanternValue, RuntimeError, Scope};

macro_rules! impl_type {
    ($ty: ty {
        $l_ty: expr,
        from $f_pat: pat => $f_expr: expr,
        from $($i_pat: pat)? => $i_expr: expr,
        $(
            $s_pat: pat = {$(
                $fi_name: literal : $fi_ty: expr => $fi_get: expr,
            )*},
            $fn_pat: pat = {$(
                $fn_name: literal ( $($fn_arg_pat: pat in $fn_arg_name: literal  : $fn_arg_ty: ty),* ) -> $fn_ret_type: ty
                    $fn_body: block,
            )*}
        )?
    }) => {
        impl LanternRecord for $ty {
            fn r#type() -> LanternType {
                $l_ty
            }
            fn into_value(self) -> LanternValue {
                $( let $i_pat = self; )?
                $i_expr
            }
            fn from_value(value: LanternValue) -> Option<Self> {
                match value {
                    $f_pat => Some($f_expr),
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
                            fn inner(scope: Rc<RefCell<Scope>>) -> Result<LanternValue, RuntimeError> {
                                let scope = scope.borrow();
                                let $fn_pat = <$ty>::from_value(scope.variable("self").expect("self method arg").value).expect("self arg type");
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
                                }
                            })
                        },
                    )?)*
                    _ => None,
                }
            }
        }
    };
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

// TODO: cloning
#[derive(Debug, Clone, PartialEq)]
pub struct LanternRecordFrame {
    pub ident: Ident,
    pub fields: Vec<LanternFunctionArg>,
    pub methods: Vec<LanternMethod>,
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
                    return Err(RuntimeError::new(MismatchedTypes(field_arg.r#type.clone(), arg.r#type())));
                };

                Ok((field_arg.name.clone(), arg))
            })
            .collect::<Result<HashMap<String, LanternValue>, RuntimeError>>()?;
        let methods = frame.methods.iter().map(|method| (method.function.name.clone(), method.clone())).collect();

        Ok(LanternCustomRecord { r#type: LanternType::Custom(frame.ident.name.clone()), fields, methods })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternCustomRecord {
    pub r#type: LanternType,
    pub fields: HashMap<String, LanternValue>,
    pub methods: HashMap<String, LanternMethod>,
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
    from value => value.into(),
    from value => value.into(),
});

impl_type!(String {
    LanternType::String,
    from LanternValue::String(str) => str,
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
    from LanternValue::Num(num) => num,
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
    from LanternValue::Bool(bool) => bool,
    from bool => LanternValue::Bool(bool),
});

impl_type!(() {
    LanternType::Nil,
    from LanternValue::Null => (),
    from => LanternValue::Null,
});

impl_type!(Option<LanternValue> {
    LanternType::Option(None),
    from LanternValue::Option(option) => option.map(|value| *value),
    from option => LanternValue::Option(option.map(Box::new)),
    _ = { },
    option = {
        "is_some"() -> bool {
            Ok(option.is_some())
        },
        "is_none"() -> bool {
            Ok(option.is_none())
        },
    }
});

impl<T: LanternRecord> LanternRecord for Option<T> {
    fn r#type() -> LanternType {
        LanternType::Option(Some(Box::new(T::r#type())))
    }

    fn into_value(self) -> LanternValue {
        self.map(T::into_value).into_value()
    }

    fn from_value(value: LanternValue) -> Option<Self> {
        match value {
            LanternValue::Option(Some(value)) => T::from_value(*value).map(Some),
            LanternValue::Option(None) => Some(None),
            _ => None,
        }
    }

    fn field(_name: &str) -> Option<LanternField<Self>> {
        None
    }

    fn method(name: &str) -> Option<LanternMethod> {
        <Option<LanternValue>>::method(name)
    }
}

