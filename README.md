# aether4

See the [documentation](./docs)

TODO:
- [ ] first fix the destruct ssa to account for locations
- [ ] add the base pointer back to the frame layout, so pushing and popping registers is easier
- [ ] fix the mem to mem move issue in ae_abs_x86_lower_flat_x86.ml
- [ ] fix the pre spilling code
- [ ] fix the existing graph coloring register allocator
    - Preallocate all the temps the stand for machine registers before, and assign them in the allocation
    - Pass the offset to different functions, just add to the offset to get the desired mach reg
    - get rid of the assign colors phase, every color directly matches to the machine register. The colors that go over are the ones that need to be spilled.
    - maybe modularize the existing graph register allocator