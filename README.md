# aether4

See the [documentation](./docs)

TODO:

- [] fix the existing graph coloring register allocator
- [] remove all record types from the ir interface, it should be fully abstract
- add invariant functions for the basic blocks and other stuff
- unfold types in the elaborator, so that the lowering process works properly
- pointer equivalence
- fix the issue with evaluating stuff twice for stuff like `+= 1`, lvalues