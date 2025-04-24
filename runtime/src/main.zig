const std = @import("std");

extern fn c0_main_export() i64;

export fn c0_panic() void {
    @panic("Panic!");
}

export fn main() void {
    const res = c0_main_export();
    std.io.getStdOut().writer().print("{}\n", .{res}) catch unreachable;
    std.process.exit(0);
}
