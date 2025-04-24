const std = @import("std");

export fn c0_print_char(c: u8) void {
    std.debug.print("{c}", .{c});
}

export fn c0_print_char2(c: u8) void {
    std.debug.print("{c}", .{c});
}
