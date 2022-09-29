use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use syn::Error;
use syn_rsx::{Node as RsxNode, NodeName, NodeType};

pub fn expand_vtree(input: RsxNode, root: bool) -> Result<TokenStream, Error> {
    let found_crate = proc_macro_crate::crate_name("grandeur_gdnative")
        .expect("grandeur_gdnative is present in `Cargo.toml`");

    let found_crate = match found_crate {
        proc_macro_crate::FoundCrate::Itself => quote! { crate },
        proc_macro_crate::FoundCrate::Name(name) => {
            let ident = Ident::new(&name, Span::call_site());
            quote! { #ident }
        }
    };

    expand_node(&found_crate, input, root)
}

fn expand_node(
    found_crate: &TokenStream,
    input: RsxNode,
    root: bool,
) -> Result<TokenStream, Error> {
    match input.node_type {
        NodeType::Element => expand_element(found_crate, input, root),
        NodeType::Fragment => expand_fragment(found_crate, input),
        NodeType::Block => Ok(input.value_as_block().unwrap().into_token_stream()),
        NodeType::Attribute | NodeType::Comment | NodeType::Doctype | NodeType::Text => {
            Err(Error::new(error_span(&input), "invalid top level element"))
        }
    }
}

fn expand_fragment(found_crate: &TokenStream, input: RsxNode) -> Result<TokenStream, Error> {
    if let Some(attr) = input.attributes.get(0) {
        return Err(Error::new(
            error_span(attr),
            "attributes are not allowed on fragments",
        ));
    }

    let children = input
        .children
        .into_iter()
        .map(|node| expand_node(found_crate, node, false))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(quote! {
        {
            let mut __nodes = Vec::new();
            #(
                #[allow(unused_braces)]
                __nodes.extend(::core::iter::IntoIterator::into_iter(#children));
            )*
            __nodes
        }
    })
}

fn expand_element(
    found_crate: &TokenStream,
    input: RsxNode,
    root: bool,
) -> Result<TokenStream, Error> {
    enum PathKind {
        NativePath,
        VarIdent,
    }

    let input_name_span = input.name_span().unwrap();
    let node_data = match input.name.unwrap() {
        NodeName::Block(expr) => expr.into_token_stream(),
        NodeName::Path(path) => {
            let path = path.path;

            if let Some(colon) = path.leading_colon {
                return Err(Error::new_spanned(colon, "unexpected leading colons"));
            }

            let path_kind = if path.segments.is_empty() {
                return Err(Error::new(Span::call_site(), "unexpected empty path"));
            } else if path.segments.len() == 1 {
                let seg = &path.segments[0];
                // Magic: determine whether it's an identifier or a type name by case
                let ident_str = seg.ident.to_string();
                if ident_str
                    .chars()
                    .next()
                    .map_or(true, |c| c.is_ascii_uppercase())
                {
                    PathKind::NativePath
                } else {
                    PathKind::VarIdent
                }
            } else {
                PathKind::NativePath
            };

            match path_kind {
                PathKind::NativePath => quote! {
                    #found_crate::vnode::Native::new::<#path>()
                },
                PathKind::VarIdent => quote! {
                    #path.clone()
                },
            }
        }
        NodeName::Dash(_) | NodeName::Colon(_) => {
            return Err(Error::new(input_name_span, "unsupported element name"));
        }
    };

    let chain = input
        .attributes
        .into_iter()
        .map(|node| {
            enum AttrKind {
                Key,
                PreferAsIs,
                Prop(Ident),
                Connect(Ident),
            }

            if node.node_type != NodeType::Attribute {
                return Err(Error::new(
                    error_span(&node),
                    "block attributes are not supported",
                ));
            }

            let attr_name_span = node.name_span().unwrap();
            let attr_kind = match node.name.unwrap() {
                NodeName::Path(path) => {
                    if let Some(ident) = path.path.get_ident() {
                        AttrKind::Prop(ident.clone())
                    } else {
                        return Err(Error::new_spanned(path, "use single colons for namespaces"));
                    }
                }
                NodeName::Colon(list) => {
                    if list.len() == 1 {
                        AttrKind::Prop(list[0].clone())
                    } else if list.len() == 2 {
                        let namespace = &list[0];
                        if namespace == "x" {
                            let name = &list[1];
                            if name == "key" {
                                AttrKind::Key
                            } else if name == "prefer_as_is" {
                                AttrKind::PreferAsIs
                            } else {
                                return Err(Error::new(name.span(), "unknown special attribute"));
                            }
                        } else if namespace == "prop" {
                            AttrKind::Prop(list[1].clone())
                        } else if namespace == "connect" || namespace == "on" {
                            AttrKind::Connect(list[1].clone())
                        } else {
                            return Err(Error::new(namespace.span(), "unknown namespace"));
                        }
                    } else {
                        return Err(Error::new_spanned(list, "path too long"));
                    }
                }
                _ => {
                    return Err(Error::new(attr_name_span, "invalid attribute name"));
                }
            };

            let value =
                node.value
                    .map(|e| e.into_token_stream())
                    .unwrap_or_else(|| match &attr_kind {
                        AttrKind::Key => quote! { key },
                        AttrKind::PreferAsIs => quote! { true },
                        AttrKind::Connect(ident) | AttrKind::Prop(ident) => quote! { #ident },
                    });

            let output = match attr_kind {
                AttrKind::Key => quote! { .key(#value) },
                AttrKind::PreferAsIs => quote! { .prefer_as_is(#value) },
                AttrKind::Prop(ident) => {
                    let name = ident.to_string();
                    quote! { .prop(#name, #value) }
                }
                AttrKind::Connect(ident) => {
                    let name = ident.to_string();
                    quote! { .connect(#name, #value) }
                }
            };

            Ok(output)
        })
        .collect::<Result<Vec<_>, _>>()?;

    let children = input
        .children
        .into_iter()
        .map(|node| expand_node(found_crate, node, false))
        .collect::<Result<Vec<_>, _>>()?;

    let node = quote! {
        {
            #found_crate::grandeur::Node::with_children(
                #found_crate::vnode::VNode::new(#node_data) #( #chain )*,
                {
                    let mut __nodes = Vec::new();
                    #(
                        #[allow(unused_braces)]
                        __nodes.extend(::core::iter::IntoIterator::into_iter(#children));
                    )*
                    __nodes
                }
            )
        }
    };

    if root {
        Ok(node)
    } else {
        Ok(quote! { ::core::iter::once(#node) })
    }
}

fn error_span(node: &RsxNode) -> Span {
    node.name_span().unwrap_or_else(Span::call_site)
}
