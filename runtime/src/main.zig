const std = @import("std");

extern fn _c0_main() i64;

export fn _runtime_c0_panic() void {
    @panic("Panic!");
}

export fn main() void {
    const res = _c0_main();
    std.io.getStdOut().writer().print("{}\n", .{res}) catch unreachable;
    std.process.exit(0);
}
