extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Fields};
use syn::punctuated::{Punctuated};
use syn::token::{Comma, Colon2};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // TODO: where is it made clear the output type of parse_macro_input?

    let di = syn::parse_macro_input!(input as syn::DeriveInput);
    let span = di.ident.span();
    let nf: syn::FieldsNamed = match di.data {
        syn::Data::Struct(syn::DataStruct {
            fields: Fields::Named(nf), ..
        }) => nf,
        _ => panic!("Can only use Builder with named fields in a struct"),
    };

    // TODO: If there is a field that is a vector, this should also be
    // treated as optional (and simply empty if nothing is given).
    
    // Original fields & names:
    let mut fields: Vec<(syn::Ident, syn::Type)> = vec![];
    // Fields which wrap Option<...> around each type:
    let mut new_fields: Vec<syn::Field> = vec![];
    // Vector fields which need single-item function; the tuple is
    // (original field, desired function name, type inside vector):
    let mut each_fields: Vec<(syn::Ident, String, syn::Type)> = vec![];
    // Fields which need a builder function:
    let mut builder_fields: Vec<(syn::Ident, syn::Type)> = vec![];

    let mut errs: Vec<syn::Error> = vec![];
    
    // Iterate over every field in the struct:
    for f in nf.named {

        fields.push((f.ident.clone().unwrap(), f.ty.clone()));
        let id = f.ident.clone().unwrap();
        let ty = f.ty.clone();

        let mut full_builder = true;
        'outer: for attr in &f.attrs {
            let nested = get_metalist_attr(attr, "builder").unwrap_or(Punctuated::new());
            for (k,v) in get_name_val_str(&nested) {
                if k.to_string() == "each" {
                    let inner = match get_inner_type(&ty, "Vec") {
                        Some(inner) => inner,
                        None => {
                            println!("Trying to generate function {} for {}, but can't get inner type", v, id.to_string());
                            continue;
                        },
                    };
                    println!("Generating function {} for {}", v, id.to_string());
                    each_fields.push((id.clone(), v.clone(), inner.clone()));
                    if id.to_string() == v {
                        println!("Suppressing normal {} function", v);
                        full_builder = false;
                    }
                    break 'outer;
                } else {
                    errs.push(syn::Error::new(id.span(), "expected `builder(each = \"...\")`"));
                }
            }
        }

        if full_builder {
            builder_fields.push((f.ident.clone().unwrap(), f.ty.clone()));
        }
        
        // Make a mutable copy and scrub out attributes:
        let mut f2 = f.clone();
        f2.attrs = vec![];
        // See if this field is already an Option.  If it is,
        // field_opt = the same field.  If not, field_opt = a field
        // with the type wrapped in Option.
        let field_opt = match get_inner_type(&ty, "Option") {
            Some(_) => f2,
            None => {
                // Build Foo of Option<Foo>:
                let mut args: Punctuated<syn::GenericArgument, Comma> = Punctuated::new();
                // Add the original type we're wrapping:
                args.push(syn::GenericArgument::Type(f.ty.clone()));
                
                // Build the <Foo> of Option<Foo>:
                let args_bkt = syn::AngleBracketedGenericArguments {
                    colon2_token: None,
                    args: args,
                    lt_token: syn::Token![<]([span]),
                    gt_token: syn::Token![>]([span]),
                };

                // Build Option<Foo>:
                let path_opt: syn::PathSegment = syn::PathSegment {
                    ident: syn::Ident::new("std::option::Option", span),
                    // TODO: This won't work for 'ident'
                    arguments: syn::PathArguments::AngleBracketed(args_bkt),
                };
                
                let mut seg: Punctuated<syn::PathSegment, Colon2> = Punctuated::new();
                seg.push(path_opt);
                
                let tp_opt: syn::TypePath = syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: seg,
                    },
                };

                // Finally, swap out type in our copy of the field:
                f2.ty = syn::Type::Path(tp_opt);
                f2
            },
        };

        new_fields.push(field_opt);
    }

    // Identifier of main type (the type we were passed):
    let main_t = di.ident.clone();
    
    // Identifier for builder type (main_t + "Builder"):
    let builder_t = syn::Ident::new(
        &(main_t.to_string() + "Builder"), span);
    
    // Iterator of code for builder functions:
    let builder_fns = builder_fields.iter().map(|(id,ty)| {
        // If original field was Option<...>, use its inner type:
        let ty = get_inner_type(ty, "Option").unwrap_or(ty);
        quote! {
            fn #id(&mut self, #id: #ty) -> &mut Self {
                self.#id = std::option::Option::Some(#id);
                self
            }
        }
    });

    let builder_each_fns = each_fields.iter().map(|(id,fn_name,ty)| {
        let fn_id = syn::Ident::new(fn_name, span);
        quote! {
            fn #fn_id(&mut self, #fn_id: #ty) -> &mut Self {
                self.#id.get_or_insert(vec![]).push(#fn_id);
                self
            }
        }
    });
    
    // Iterator of code for initializing all builder fields to None:
    let struct_init = fields.iter().map(|(id,_)| quote! { #id: std::option::Option::None, });
    
    // Iterator of code for field checks in build() function.  Fields
    // like Option<...> are optional and need not be checked.
    let field_checks = fields.iter().map(|(id,ty)| {
        let err_str = format!("Missing {} in builder", id.to_string());
        match get_inner_type(ty, "Option") {
            Some(_) => quote! {
                #id: self.#id.clone(),
            },
            None => match get_inner_type(ty, "Vec") {
                Some(_) => quote! {
                    #id: self.#id.clone().unwrap_or(vec![]),
                },
                None => quote! {
                    #id: self.#id.clone().ok_or(#err_str)?,
                    // TODO: Is .clone() the only option here?
                },
            },
        }
    });

    let errs_q = errs.iter().map(syn::Error::to_compile_error);
    
    let q = quote! {

        #(#errs_q);*

        pub struct #builder_t {
            #(#new_fields),*
        }
        
        impl #builder_t {
            #(#builder_fns)*
            #(#builder_each_fns)*
            pub fn build(&mut self) -> Result<#main_t, Box<dyn std::error::Error>> {

                let r = #main_t {
                    #(#field_checks)*
                };
                Ok(r)
            }
        }
        
        impl #main_t {
            pub fn builder() ->  #builder_t {
                #builder_t {
                    #(#struct_init)*
                }
            }
        }
    };
    println!("TOKENS: {}", q);
    TokenStream::from(q)
}

fn get_single_path(segments: &Punctuated<syn::PathSegment, Colon2>) -> Option<&syn::Ident> {
    if segments.len() != 1 {
        return None;
    }
    match segments.first() {
        Some(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        }) => Some(ident),
        _ => None,
    }
}

fn get_metalist_attr(attr: &syn::Attribute, target_path: &str) -> Option<Punctuated<syn::NestedMeta, Comma>> {
    let (segs, nested) = match attr.parse_meta() {
        Ok(syn::Meta::List(syn::MetaList {
            path: syn::Path { segments: segs, .. }, nested, ..
        })) => (segs, nested.clone()),
        _ => return None,
    };
    if segs.len() != 1 {
        return None;
    }
    let id = match segs.first() {
        Some(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        }) => ident,
        _ => return None,
    };
    if id.to_string() != target_path {
        return None;
    }

    Some(nested)
}

fn get_name_val_str(nested: &Punctuated<syn::NestedMeta, Comma>) -> Vec<(&syn::Ident, String)> {
    let mut vals: Vec<(&syn::Ident, String)> = vec![];
    for nm in nested {
        match nm {
            syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                path, lit, ..
            })) => {
                match (get_single_path(&path.segments), lit) {
                    (Some(id), syn::Lit::Str(lit)) => {
                        vals.push((id, lit.value()));
                    },
                    _ => (),
                }
            },
            _ => (),
        }
    }
    vals
}

// If the Type matches some outer type (e.g. Option), then returns its
// inner type wrapped in Option.  Otherwise, returns None.
fn get_inner_type<S: Into<String>>(ty: &syn::Type, target: S) -> Option<&syn::Type> {
    // Use a very ugly cascade of pattern matches and other checks
    // to filter out anything that is Option<...>:
    let segs = match ty {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments: s, .. },
        }) => s,
        _ => return None,
    };
    if segs.len() != 1 {
        return None
    };
    match segs.first() {
        Some(syn::PathSegment {
            ident: id,
            arguments: syn::PathArguments::AngleBracketed(
                syn::AngleBracketedGenericArguments {
                    args: ga, ..
                }),
        }) => {
            if id.to_string() != target.into() {
                return None;
            }
            if ga.len() != 1 {
                return None
            }
            match ga.first() {
                Some(syn::GenericArgument::Type(t)) => {
                    return Some(t)
                }
                _ => return None
            }
        },
        _ => return None
    };
}

