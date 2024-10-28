use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, FnArg, Ident, ItemFn, Pat, ReturnType, Signature, Type, TypePath};

#[proc_macro_attribute]
pub fn lantern_fun(_: TokenStream, item: TokenStream) -> TokenStream {
    let ItemFn {
        vis,
        sig: Signature {
            ident,
            inputs,
            output,
            ..
        },
        block,
        ..
    } = parse_macro_input!(item as ItemFn);

    let name = ident.to_string();

    let inner_args: Vec<Result<(Ident, TypePath), syn::Error>> = inputs.iter()
        .map(|arg| if let FnArg::Typed(typed) = arg {
            if let (Pat::Ident(ref ident), Type::Path(ty)) = (*typed.pat.clone(), *typed.ty.clone()) {
                Ok((ident.ident.clone(), ty))
            } else {
                Err(syn::Error::new(arg.span(), "only named args are allowed in lantern functions"))
            }
        } else {
            Err(syn::Error::new(arg.span(), "cannot use `self` in lantern function"))
        })
        .collect();

    let val_bindings = inner_args.iter()
        .map(|ident| match ident {
            Ok((ident, arg_type)) => {
                let name = ident.to_string();
                quote! {
                    let #ident = <#arg_type as ::lantern_lang::record::LanternRecord>::from_value(
                        scope.borrow().variable(#name).expect("value binding").value
                    )
                        .expect("value binding");
                }
            },
            Err(err) => err.to_compile_error(),
        });

    let args = inner_args.iter()
        .map(|ident| match ident {
            Ok((ident, r#type)) => {
                let name = ident.to_string();

                quote! {
                    ::lantern_lang::LanternFunctionArg {
                        name: #name.to_string(),
                        r#type: <#r#type as ::lantern_lang::record::LanternRecord>::r#type(),
                    }
                }
            },
            Err(err) => err.to_compile_error(),
        });

    let output = match output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, r#type) => quote! { #r#type },
    };

    quote! {
        #vis fn #ident () -> ::lantern_lang::LanternFunction {
            fn inner(
                scope: ::std::rc::Rc<::std::cell::RefCell<::lantern_lang::scope::Scope>>
            ) -> Result<::lantern_lang::LanternValue, ::lantern_lang::error::RuntimeError> {
                #(#val_bindings)*

                #block.map(::lantern_lang::record::LanternRecord::into_value)
            }

            let mut args = Vec::new();
            #(
                args.push(#args);
            )*

            ::lantern_lang::LanternFunction {
                name: #name.to_string(),
                args,
                ret_type: <#output as ::lantern_lang::record::LanternRecord>::r#type(),
                body: ::lantern_lang::LanternFunctionBody::Native(inner),
                scope: ::std::rc::Rc::new(::std::cell::RefCell::new(::lantern_lang::scope::Scope::Head)),
            }
        }
    }.into()
}

