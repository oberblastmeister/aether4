const std = @import("std");

const Context = extern struct {
    hp_lim: *anyopaque,
    sp_lim: *anyopaque,
};

// first argument is CX pointer.
// second argument is HP pointer.
extern fn c0_main_export(*Context, *anyopaque) void;

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

export fn c0_runtime_alloc_fail() void {
    std.debug.panic("Failed to allocate memory", .{});
}

export fn c0_runtime_deref_fail() void {
    std.debug.panic("Failed to dereference pointer", .{});
}

export fn lir_runtime_unreachable() void {
    std.debug.panic("Executed unreachable instruction", .{});
}

fn run() !void {
    var act = std.os.linux.Sigaction{
        .handler = .{ .handler = std.os.linux.SIG.DFL },
        .mask = std.os.linux.empty_sigset,
        .flags = 0,
    };
    // override the default zig sigfpe handler
    _ = std.os.linux.sigaction(std.os.linux.SIG.FPE, &act, null);
    const allocator = std.heap.page_allocator;
    const HP = try allocator.alloc(u8, 1024 * 1024);
    @memset(HP, 0);
    defer allocator.free(HP);
    var context = Context{ .hp_lim = HP[HP.len..].ptr, .sp_lim = undefined };
    c0_main_export(&context, HP.ptr);
}

pub fn main() !void {
    try run();
}
