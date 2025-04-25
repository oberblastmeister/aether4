const std = @import("std");

extern fn c0_main_export() i64;

export fn c0_panic() void {
    @panic("Panic!");
}

export fn c0_runtime_assert(cond: bool, start_line: u64, start_col: u64, end_line: u64, end_col: u64) void {
    if (!cond) {
        std.debug.panic("Assertion failed at [{},{}]-[{},{}]", .{ start_line, start_col, end_line, end_col });
    }
}

export fn main() void {
    const res = c0_main_export();
    std.io.getStdOut().writer().print("{}\n", .{res}) catch unreachable;
    std.process.exit(0);
}
