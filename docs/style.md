# Style

## File Structure

Dealing with name conflicts and dependency cycles in OCaml is a pain, especially in larger projects. To circumvent this, we do not use nested folders, and instead prefix each file with nested namespaces.

For example, `src/abs_x86/liveness.ml` becomes `ae_abs_x86_liveness`

When importing modules, be sure to rename the module to the unprefixed version. 

```ocaml
module Liveness = Ae_abs_x86_liveness
```

We also have `_std.ml` files which reexport all the modules in it's namespace. This is very similar to `mod.rs` or `index.js`.

For example, here are the contents of `ae_abs_x86_std.ml`

```ocaml
module Lower_flat_x86 = Ae_abs_x86_lower_flat_x86
module Liveness = Ae_abs_x86_liveness
module Regalloc = Ae_abs_x86_regalloc
```

When inside the namespace, do *not* import the `_std.ml` file, because it is intended to be used by consumers of this namespace.