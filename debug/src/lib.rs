use proc_macro::TokenStream;
use syn::{self, spanned::Spanned};
use quote::{ToTokens, quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(t) => t.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ret = generate_debug_trait(st)?;
    return Ok(ret)
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;
fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    match &d.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named,.. }),
           ..
        }) => Ok(named),
        _ => Err(syn::Error::new_spanned(
            d,
            "Must define on a Struct, not Enum".to_string(),
        )),
    }
}

fn generate_debug_trait(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let struct_name_ident = &st.ident;
    let struct_name_literal = struct_name_ident.to_string();

    let mut fmt_body_stream = proc_macro2::TokenStream::new();

    fmt_body_stream.extend(quote!(
        fmt.debug_struct(#struct_name_literal)
    ));

    for field in fields.iter() {
        let field_name_ident = field.ident.as_ref().unwrap();
        let field_name_literal = field_name_ident.to_string();

        let mut format_str = "{:?}".to_string();
        if let Some(format) = get_custom_format_of_field(field)? {
            format_str = format;
        }
        fmt_body_stream.extend(quote!(
           .field(#field_name_literal, &format_args!(#format_str, self.#field_name_ident))
        ));
    }

    fmt_body_stream.extend(quote!(
       .finish()
    ));

    let ret_stream = quote!(
        impl std::fmt::Debug for #struct_name_ident {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    );

    Ok(ret_stream)
}

fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
            ref path,
            ref lit,
            ..
        })) = attr.parse_meta() {
            if path.is_ident("debug") {
                if let syn::Lit::Str(ref s) = lit {
                    return Ok(Some(s.value()));
                }
            }
        }
    }

    Ok(None)
}