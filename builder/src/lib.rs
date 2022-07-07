use proc_macro::TokenStream;
use quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Result};
#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
type StrcutFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields_from_derive_input(input: &DeriveInput) -> Result<&StrcutFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new(
            input.span(),
            "This macro only works on structs with named fields",
        ))
    }
}

fn generate_build_struct_fields(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;

    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();
    let res = quote::quote!(
        #(#idents: Option<#types>),*
    );
    Ok(res)
}

fn generate_execuable_fn(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let res = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = &f.ty;
            quote::quote!(
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            )
        })
        .collect();
    Ok(res)
}

fn generate_build_struct_factory_init_clasuses(
    st: &DeriveInput,
) -> Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;

    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let res = quote::quote!(
        #(#idents: None),*
    );
    Ok(res)
}

fn generate_build_fn(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let struct_ident = &st.ident;
    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();

    let mut assert_block = Vec::new();
    for field in fields {
        let ident = &field.ident;

        assert_block.push(quote::quote!(
            if self.#ident.is_none() {
                return std::result::Result::Err( "Missing required field".into());
            }
        ));
    }
    let ret_block = quote::quote!(
        Ok(#struct_ident {
            #(#idents: self.#idents.clone().unwrap()),*
        })
    );

    let res = quote::quote!(
        fn build(&self) -> std::result::Result<#struct_ident, Box<dyn std::error::Error>> {
            #(#assert_block)*
            #ret_block
        }
    );
    Ok(res)
}
fn do_expand(st: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_name = st.ident.to_string();
    let builder_name_iteral = format!("{}Builder", struct_name);
    let builder_name_ident = syn::Ident::new(&builder_name_iteral, st.span());
    let struct_ident = &st.ident;

    let fields = generate_build_struct_fields(&st)?;
    let init_clauses = generate_build_struct_factory_init_clasuses(&st)?;
    let execuable_fn = generate_execuable_fn(&st)?;
    let build_fn = generate_build_fn(&st)?;
    let res = quote::quote!(
        #[derive(Debug)]
        pub struct #builder_name_ident {
            #fields
        }
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #init_clauses
                }
            }
        }
        impl #builder_name_ident {
            #execuable_fn
            #build_fn
        }
    );
    Ok(res)
}
