extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{Fields};
use syn::punctuated::{Punctuated};
use syn::token::{Comma, Colon2};

/*
#[proc_macro_attribute]
pub fn builder(attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
*/

// TODO: Still stuck at:
// "error[E0658]: The attribute `builder` is currently unknown to the
// compiler and may have meaning added to it in the future"
//
// The macro *output*, not the input, is the problem.  I need to scrub
// the attribs out of the fields.

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

    // Iterate over every field in the struct:
    for f in nf.named {

        fields.push((f.ident.clone().unwrap(), f.ty.clone()));
        let id = f.ident.clone().unwrap();
        let ty = f.ty.clone();
        let t = quote! {
            fn #id(&mut self, #id #ty) -> &mut self {
                self.#id = Some(#id);
                self
            }
        };
        eprintln!("tokens for t: {}", t);

        // See if this field is already an Option.  If it is,
        // field_opt = the same field.  If not, field_opt = a field
        // with the type wrapped in Option.
        let field_opt = match is_option_type(&ty) {
            Some(_) => f.clone(),
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

                // Finally, build a field (by copying the old one and
                // swapping out the type):
                let mut f_opt: syn::Field = f.clone();
                f_opt.ty = syn::Type::Path(tp_opt);
                f_opt
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
    let builder_fns = fields.iter().map(|(id,ty)| {
        // If original field was Option<...>, use its inner type:
        let ty = is_option_type(ty).unwrap_or(ty);
        quote! {
            fn #id(&mut self, #id: #ty) -> &mut Self {
                self.#id = Some(#id);
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
                eprintln!("Rejected ID: {}", id.to_string());
                return None;
            }
            eprintln!("Using ID: {}", id.to_string());
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

