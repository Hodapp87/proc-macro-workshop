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

    // Original fields & names:
    let mut fields: Vec<(syn::Ident, syn::Type)> = vec![];
    // Fields which wrap Option<...> around each type:
    let mut new_fields: Vec<syn::Field> = vec![];
    // Vector fields which need single-item function; the tuple is
    // (original field, desired function name):
    let mut each_fields: Vec<(syn::Ident, String)> = vec![];
    // Fields which need a builder function:
    let mut builder_fields: Vec<(syn::Ident, syn::Type)> = vec![];

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
                    eprintln!("Generating function {} for {}", v, id.to_string());
                    each_fields.push((id.clone(), v.clone()));
                    if id.to_string() == v {
                        eprintln!("Suppressing normal {} function", v);
                        full_builder = false;
                    }
                    break 'outer;
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
        let field_opt = match is_option_type(&ty) {
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
                    ident: syn::Ident::new("Option", span),
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
        let ty = is_option_type(ty).unwrap_or(ty);
        quote! {
            fn #id(&mut self, #id: #ty) -> &mut Self {
                self.#id = Some(#id);
                self
            }
        }
    });

    let builder_each_fns = each_fields.iter().map(|(id,fn_name)| {
        let fn_id = syn::Ident::new(fn_name, span);
        quote! {
            // TODO:
            // Still need to know type inside of vector.
            fn #fn_id(&mut self, #fn_id: _) -> &mut Self {
                self.#id.push(#fn_id);
                self
            }
        }
    });
    
    // Iterator of code for initializing all builder fields to None:
    let struct_init = fields.iter().map(|(id,_)| quote! { #id: None, });
    
    // Iterator of code for field checks in build() function.  Fields
    // like Option<...> are optional and need not be checked.
    let field_checks = fields.iter().map(|(id,ty)| {
        let err_str = format!("Missing {} in builder", id.to_string());
        match is_option_type(ty) {
            Some(_) => quote! {
                #id: self.#id.clone(),
            },
            None => quote! {
                #id: self.#id.clone().ok_or(#err_str)?,
                // TODO: Is .clone() the only option here?
            }
        }
    });
    
    let q = quote! {

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
    eprintln!("TOKENS: {}", q);
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

// If the given Type is an Option, then returns its inner type wrapped
// in Option.  Otherwise, returns None.
fn is_option_type(ty: &syn::Type) -> Option<&syn::Type> {
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
            if id.to_string() != "Option" {
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

