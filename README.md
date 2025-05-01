# aether4

See the [documentation](./docs)

TODO:

- [] add more tests
- [] fix the existing graph coloring register allocator
- [] remove all record types from the ir interface, it should be fully abstract
- rewrite the process stuff to use spawn/cmd instead of eio
- remove the changed stuff in the driver
- add invariant functions for the basic blocks and other stuff
- add Alloc as a nullary operation
- use Nullary, Unary, Binary, Ternary representation in all Irs
    - remove the adding extern on prim ops and just add a primops data type instead