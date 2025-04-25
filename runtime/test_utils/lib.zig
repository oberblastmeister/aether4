const std = @import("std");

export fn c0_print_char(c: u64) void {
    std.debug.print("{c}\n", .{@as(u8, @truncate(c))});
}

export fn c0_print_char2(c1: u64, c2: u64) void {
    std.debug.print("{c} {c}\n", .{ @as(u8, @truncate(c1)), @as(u8, @truncate(c2)) });
}

export fn c0_print_char6(c1: u64, c2: u64, c3: u64, c4: u64, c5: u64, c6: u64) void {
    std.debug.print("{c} {c} {c} {c} {c} {c}\n", .{ @as(u8, @truncate(c1)), @as(u8, @truncate(c2)), @as(u8, @truncate(c3)), @as(u8, @truncate(c4)), @as(u8, @truncate(c5)), @as(u8, @truncate(c6)) });
}

export fn c0_print_char7(c1: u64, c2: u64, c3: u64, c4: u64, c5: u64, c6: u64, c7: u64) void {
    std.debug.print("{c} {c} {c} {c} {c} {c} {c}\n", .{ @as(u8, @truncate(c1)), @as(u8, @truncate(c2)), @as(u8, @truncate(c3)), @as(u8, @truncate(c4)), @as(u8, @truncate(c5)), @as(u8, @truncate(c6)), @as(u8, @truncate(c7)) });
}

export fn c0_print_char14(c1: u64, c2: u64, c3: u64, c4: u64, c5: u64, c6: u64, c7: u64, c8: u64, c9: u64, c10: u64, c11: u64, c12: u64, c13: u64, c14: u64) void {
    std.debug.print("{c} {c} {c} {c} {c} {c} {c} {c} {c} {c} {c} {c} {c} {c}\n", .{ @as(u8, @truncate(c1)), @as(u8, @truncate(c2)), @as(u8, @truncate(c3)), @as(u8, @truncate(c4)), @as(u8, @truncate(c5)), @as(u8, @truncate(c6)), @as(u8, @truncate(c7)), @as(u8, @truncate(c8)), @as(u8, @truncate(c9)), @as(u8, @truncate(c10)), @as(u8, @truncate(c11)), @as(u8, @truncate(c12)), @as(u8, @truncate(c13)), @as(u8, @truncate(c14)) });
}

export fn c0_print_float(x: u64) void {
    std.debug.print("{d}\n", .{@as(f64, @bitCast(x))});
}

export fn c0_fdiv(x: u64, y: u64) u64 {
    return @bitCast(@as(f64, @bitCast(x)) / @as(f64, @bitCast(y)));
}

export fn c0_to_float(x: u64) u64 {
    const f: f64 = @bitCast(x);
    return @bitCast(f);
}

export fn c0_print_hex(n: u64) void {
    std.debug.print("{x}\n", .{n});
}
