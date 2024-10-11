#![feature(proc_macro_diagnostic)]

use proc_macro::{token_stream, TokenStream};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);

    match do_expand(&st) {
        Ok(expanded) => expanded.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = st.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.ident.span());

    let struct_ident = &st.ident; // 模板代码中不可以使用`.`来访问结构体成员，所以要在模板代码外面将标识符放到一个独立的变量中

    let fields = get_fields_from_derive_input(st)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(&fields)?;
    // 下面这一行是新加的
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    // 下面这一行是第三关新加的
    let setter_functions = generate_setter_functions(fields)?;
    // 下面这一行是第四关新加的
    let generated_builder_functions = generate_build_function(fields, struct_ident)?;

    let ret = quote! {     // ----------------------------------+
        pub struct #builder_name_ident {                   //   |
            #builder_struct_fields_def
        }                                                  //   |
        impl #struct_ident {                               //   |
            pub fn builder() -> #builder_name_ident {      //  被quote!宏包裹的是模板代码
                #builder_name_ident{
                    // 下面这一行是新加的，注意我们在这里重复展开了每一个字段
                    #(#builder_struct_factory_init_clauses),*
                }
            }                                              //   |
        }                                                  //   |
        // 下面这三行是第三关新加的
        impl #builder_name_ident {
            #setter_functions
            #generated_builder_functions
        }
    }; // ----------------------------------+

    Ok(ret)
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

fn get_fields_from_derive_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = st.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new(
            st.ident.span(),
            "Only structs with named fields are supported",
        ))
    }
}

fn generate_builder_struct_fields_def(
    fields: &StructFields,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    // 第六关，对types 变量的构建逻辑进行了调整
    let types: Vec<_> = fields
        .iter()
        .map(|f| {
            if let Some(inner_ty) = get_generic_inner_type(&f.ty, "Option") {
                quote!(std::option::Option<#inner_ty>)
            } else if get_user_specified_ident_for_vec(f).is_some() {
                let origin_ty = &f.ty;
                quote!(#origin_ty)
            } else {
                let origin_ty = &f.ty;
                quote!(std::option::Option<#origin_ty>)
            }
        })
        .collect();

    let ret = quote! {
        #(#idents: #types),*
    };

    Ok(ret)
}

fn generate_builder_struct_factory_init_clauses(
    fields: &StructFields,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: Vec<_> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            // 下面这个if分支是第七关加入的，在第六关的时候只有else分支里的代码
            if get_user_specified_ident_for_vec(f).is_some() {
                quote! {
                    #ident: std::vec::Vec::new()
                }
            } else {
                quote! {
                    #ident: std::option::Option::None
                }
            }
        })
        .collect();

    Ok(init_clauses)
}

fn generate_setter_functions(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut final_tokenstream = proc_macro2::TokenStream::new();

    // 第七关修改，这里之前用了zip串联了两个迭代器，现在需要拿到对应的原始field，所以又加了一层`enumerate()`迭代器
    // 这里写成 for idx in 0..fields.len() {let ident = &fields[idx].ident; let type_ = &fields[idx].ty;...} 这种写法或许更优雅一些
    for (idx, (ident, type_)) in idents.iter().zip(types.iter()).enumerate() {
        let mut tokenstream_piece;
        // 第六关，对tokenstream_piece 变量的构建逻辑进行了调整
        if let Some(innner_ty) = get_generic_inner_type(type_, "Option") {
            tokenstream_piece = quote! {
                fn #ident(&mut self, #ident: #innner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            };
        // 下面这个分支是第七关加入的
        } else if let Some(ref user_specified_ident) =
            get_user_specified_ident_for_vec(&fields[idx])
        {
            let inner_ty = get_generic_inner_type(type_, "Vec").ok_or(syn::Error::new(
                fields[idx].span(),
                "each field must be specified with Vec field",
            ))?;
            tokenstream_piece = quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_ty) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };
            // 如果用户指定的setter名字和原始字段的名字不一样，那么产生另一个setter，这个setter是一次性传入一个列表的
            if user_specified_ident != ident.as_ref().unwrap() {
                tokenstream_piece.extend(quote! {
                    fn #ident(&mut self, #ident: #type_) -> &mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                })
            }
        } else {
            tokenstream_piece = quote! {
                fn #ident(&mut self, #ident: #type_) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }

        final_tokenstream.extend(tokenstream_piece);
    }

    Ok(final_tokenstream)
}

fn generate_build_function(
    fields: &StructFields,
    origin_stuct_ident: &syn::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    // 下面这一行是第六关新加的，之前没用到type相关信息，就没写下面这一行
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut check_code_pieces = Vec::new();
    for idx in 0..idents.len() {
        let ident = &idents[idx];
        // 第六关修改，只对不是`Option`类型的字段生成校验逻辑
        // 第七关修改，只对不是`Option`类型且没有指定each属性的字段生成校验逻辑
        if get_generic_inner_type(&types[idx], "Option").is_none()
            && get_user_specified_ident_for_vec(&fields[idx]).is_none()
        {
            let check_code = quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field missing", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            };
            check_code_pieces.push(check_code);
        }
    }

    let mut fill_result_clauses = Vec::new();
    for idx in 0..idents.len() {
        let ident = idents[idx];
        // 第七关，这里需要判断是否有each属性。第一个分支是本关加入的。注意这里几个分支的先后判断顺序
        // 看我写在这里的代码可能没什么感觉，但如果是自己写的话，这几个分支的判断先后顺序是很重要的，否���可能生成出有问题的代码
        // 这里主要的问题是梳理清楚【是否有each属性】和【是否为Option类型】这两个条件的覆盖范围
        if get_user_specified_ident_for_vec(&fields[idx]).is_some() {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            });
        } else if get_generic_inner_type(&types[idx], "Option").is_none() {
            // 这里需要区分`Option`类型字段和非`Option`类型字段
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone().unwrap()
            });
        } else {
            fill_result_clauses.push(quote! {
                #ident: self.#ident.clone()
            });
        }
    }

    let token_stream = quote! {
        pub fn build(&mut self) -> Result<#origin_stuct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#check_code_pieces)*
            Ok(#origin_stuct_ident{
                #(#fill_result_clauses),*
            })
        }
    };
    Ok(token_stream)
}

fn get_generic_inner_type<'a>(ty: &'a syn::Type, outer_ident_name: &str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = &seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(innner_ty)) = args.first() {
                        return Some(innner_ty);
                    }
                }
            }
        }
    }

    None
}

#[proc_macro_derive(ExploreAttribute)]
pub fn attribute_explore(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    let attr = st.attrs.first().unwrap();
    let meta = attr.parse_meta(); // 解析为`sny::Meta`对象
    eprintln!("{:#?}", meta);
    proc_macro2::TokenStream::new().into()
}

fn get_user_specified_ident_for_vec(fields: &syn::Field) -> Option<syn::Ident> {
    for attr in &fields.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = attr.parse_meta()
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    None
}
