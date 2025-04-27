const std = @import("std");

extern fn c0_main_export() void;

export fn c0_panic() void {
    @panic("Panic!");
}

export fn c0_runtime_assert(cond: bool, start_line: u64, start_col: u64, end_line: u64, end_col: u64) void {
    if (!cond) {
        std.debug.panic("Assertion failed at [{},{}]-[{},{}]", .{ start_line, start_col, end_line, end_col });
    }
}

export fn c0_runtime_print_int(i: i64) void {
    std.debug.print("{}\n", .{i});
}

export fn c0_runtime_print_bool(b: bool) void {
    std.debug.print("{}\n", .{b});
}

export fn main() void {
    c0_main_export();
    std.process.exit(0);
}
