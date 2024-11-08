use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{self, parse_quote, spanned::Spanned};

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
    return Ok(ret);
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;
fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    match &d.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => Ok(named),
        _ => Err(syn::Error::new_spanned(
            d,
            "Must define on a Struct, not Enum".to_string(),
        )),
    }
}

fn generate_debug_trait_core(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
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

    Ok(fmt_body_stream)
}

fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
            ref path, ref lit, ..
        })) = attr.parse_meta()
        {
            if path.is_ident("debug") {
                if let syn::Lit::Str(ref s) = lit {
                    return Ok(Some(s.value()));
                }
            }
        }
    }

    Ok(None)
}

fn generate_debug_trait(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fmt_body_stream = generate_debug_trait_core(st)?;

    let struct_name_ident = &st.ident;

    let mut generics_param_to_modify = st.generics.clone();

    let fields = get_fields_from_derive_input(st)?;
    let mut field_type_names = Vec::new();
    let mut phantomdata_type_param_names = Vec::new();

    for field in fields.iter() {
        if let Some(field_type_name) = get_field_type_name(field)? {
            field_type_names.push(field_type_name);
        }

        if let Some(phantomdata_type_param_name) = get_phantom_data_generic_type_name(field)? {
            phantomdata_type_param_names.push(phantomdata_type_param_name);
        }
    }
    // 判定是否设置了限定条件干预，如果设定了，则不进行推断，直接使用用户给出的限定条件放到where子句中
    if let Some(hatch) = get_struct_escape_hatch(st) {
        generics_param_to_modify.make_where_clause();
        generics_param_to_modify
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(hatch.as_str()).unwrap());
    } else {
        // 下面这一行是第七关新加的，调用函数找到关联类型信息
        let associated_types_map = get_generic_associated_types(st);
        for g in generics_param_to_modify.params.iter_mut() {
            if let syn::GenericParam::Type(ref mut t) = g {
                let type_param_name = t.ident.to_string();
                if phantomdata_type_param_names.contains(&type_param_name)
                    && !field_type_names.contains(&type_param_name)
                {
                    continue;
                }

                // 下面这3行是本次新加的，如果是关联类型，就不要对泛型参数`T`本身再添加约束了,除非`T`本身也被直接使用了
                if associated_types_map.contains_key(&type_param_name)
                    && !field_type_names.contains(&type_param_name)
                {
                    continue;
                }
                t.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }

        generics_param_to_modify.make_where_clause();
        for (_, associated_types) in associated_types_map {
            for associated_type in associated_types {
                generics_param_to_modify
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote!(#associated_type: std::fmt::Debug));
            }
        }
    }

    let (impl_generics, ty_generics, where_clause) = generics_param_to_modify.split_for_impl();

    let ret_stream = quote!(

        impl #impl_generics std::fmt::Debug for #struct_name_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    );

    Ok(ret_stream)
}

fn get_phantom_data_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = &seg.arguments
                {
                    if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        path, ..
                    })) = args.first().unwrap()
                    {
                        if let Some(seg) = path.segments.last() {
                            return Ok(Some(seg.ident.to_string()));
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}

fn get_field_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(seg) = segments.last() {
            return Ok(Some(seg.ident.to_string()));
        }
    }

    Ok(None)
}

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<syn::TypePath>>,
}

fn visit_type_path(visitor: &mut TypePathVisitor, node: &syn::TypePath) {
    if node.path.segments.len() >= 2 {
        let generic_type_name = node.path.segments[0].ident.to_string();
        if visitor.generic_type_names.contains(&generic_type_name) {
            visitor
                .associated_types
                .entry(generic_type_name)
                .or_insert(Vec::new())
                .push(node.clone());
        }
    }

    // Recursively visit nested type paths
    for segment in &node.path.segments {
        if let syn::PathArguments::AngleBracketed(ref args) = segment.arguments {
            for arg in &args.args {
                if let syn::GenericArgument::Type(syn::Type::Path(type_path)) = arg {
                    visit_type_path(visitor, type_path);
                }
            }
        }
    }
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let origin_generic_param_names: Vec<String> = st
        .generics
        .params
        .iter()
        .filter_map(|f| {
            if let syn::GenericParam::Type(ref t) = f {
                return Some(t.ident.to_string());
            }
            None
        })
        .collect();

    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };

    // Replace the visit_derive_input call with a custom implementation
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = &st.data {
        for field in fields {
            if let syn::Type::Path(type_path) = &field.ty {
                visit_type_path(&mut visitor, type_path);
            }
        }
    }

    visitor.associated_types
}

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.last() {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = inert_attr.parse_meta() {
            if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(path_value))) = nested.last() {
                if path_value.path.is_ident("bound") {
                    if let syn::Lit::Str(ref lit) = path_value.lit {
                        return Some(lit.value());
                    }
                }
            }
        }
    }

    None
}
