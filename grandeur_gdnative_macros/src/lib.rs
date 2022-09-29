use proc_macro::TokenStream;

mod rsx;

/// Parses a RSX tree into a single `grandeur_gdnative` node. There must be
/// exactly one top-level element, which may be a fragment.
///
/// The macro interprets tag names depending on its case. `CamelCase` identifiers
/// are assumed to be names of Godot API classes, while `snake_case` identifiers
/// are assumed to be names of bindings (variables) of `VNodeKind` types.
///
/// The macro accepts attributes in the following namespaces:
///
/// - `prop` or un-namespaced, for property names. The binding with the same name
///   as the property name is used as the value, if no value is provided.
/// - `x`, for `grandeur_gdnative` internal semantics.
///     - `x:key` for equality comparison in conciliation. The `key` binding is used
///       if no value is provided.
///     - `x:prefer_as_is` for keeping the existing scene tree whenever possible.
///       The flag is set to `true` if no value is provided.
/// - `connect` or `on`, for signal connections. The binding with the same name
///   as the signal name is used as the value (handler), if no value is provided.
///
/// See [`vfrag`] for a macro that always produces a fragment, given any number
/// of top-level nodes.
#[proc_macro]
pub fn vtree(input: TokenStream) -> TokenStream {
    let mut rsx_nodes = match syn_rsx::parse(input) {
        Ok(nodes) => nodes,
        Err(e) => return e.into_compile_error().into(),
    };

    if rsx_nodes.len() != 1 {
        return quote::quote! {
            ::core::compile_error!("input must contain exactly one node (which may be a fragment)");
        }
        .into();
    }

    match rsx::expand_vtree(rsx_nodes.remove(0), true) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

/// Parses RSX into a `grandeur_gdnative` fragment. There may be any number of
/// top-level elements. The output will always be a fragment.
///
/// The macro interprets tag names depending on its case. `CamelCase` identifiers
/// are assumed to be names of Godot API classes, while `snake_case` identifiers
/// are assumed to be names of bindings (variables) of `VNodeKind` types.
///
/// The macro accepts attributes in the following namespaces:
///
/// - `prop` or un-namespaced, for property names. The binding with the same name
///   as the property name is used as the value, if no value is provided.
/// - `x`, for `grandeur_gdnative` internal semantics.
///     - `x:key` for equality comparison in conciliation. The `key` binding is used
///       if no value is provided.
///     - `x:prefer_as_is` for keeping the existing scene tree whenever possible.
///       The flag is set to `true` if no value is provided.
/// - `connect` or `on`, for signal connections. The binding with the same name
///   as the signal name is used as the value (handler), if no value is provided.
#[proc_macro]
pub fn vfrag(input: TokenStream) -> TokenStream {
    let rsx_nodes = match syn_rsx::parse(input) {
        Ok(nodes) => nodes,
        Err(e) => return e.into_compile_error().into(),
    };

    let trees = rsx_nodes
        .into_iter()
        .map(|n| rsx::expand_vtree(n, false))
        .collect::<Result<Vec<_>, _>>();

    let trees = match trees {
        Ok(trees) => trees,
        Err(err) => return err.into_compile_error().into(),
    };

    quote::quote! {
        {
            let mut __nodes = Vec::new();
            #(
                #[allow(unused_braces)]
                __nodes.extend(::core::iter::IntoIterator::into_iter(#trees));
            )*
            __nodes
        }
    }
    .into()
}
