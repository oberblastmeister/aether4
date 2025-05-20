const std = @import("std");

pub fn build_lib(b: *std.Build, name: []const u8, root: []const u8, target: anytype, optimize: anytype) void {
    const lib = b.addStaticLibrary(.{
        .name = name,
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path(root),
        .target = target,
        .optimize = optimize,
    });
    lib.pie = true;
    lib.addAssemblyFile(b.path("src/asm_bits.s"));

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    b.installArtifact(lib);
}

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const c0_runtime_lib = b.addStaticLibrary(.{
        .name = "c0_runtime",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    c0_runtime_lib.pie = true;
    c0_runtime_lib.addAssemblyFile(b.path("src/asm_bits.s"));

    b.installArtifact(c0_runtime_lib);

    const c0_test_utils_lib = b.addStaticLibrary(.{
        .name = "c0_test_utils",
        .root_source_file = b.path("test_utils/lib.zig"),
        .target = target,
        .optimize = optimize,
    });
    c0_test_utils_lib.pie = true;

    b.installArtifact(c0_test_utils_lib);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_tests.pie = true;
    main_tests.addAssemblyFile(b.path("src/asm_bits.s"));

    b.installArtifact(main_tests);
    const run_main_tests = b.addRunArtifact(main_tests);

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build test`
    // This will evaluate the `test` step rather than the default, which is "install".
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
}
