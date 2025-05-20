const std = @import("std");
const Scheduler = @import("Scheduler.zig");
const Context = @import("context.zig").Context;

comptime {
    std.testing.refAllDeclsRecursive(Scheduler);
}

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

export fn c0_runtime_print_char(c: u64) void {
    std.debug.print("{c}", .{@as(u8, @truncate(c))});
}

export fn c0_runtime_print_bool(b: bool) void {
    std.debug.print("{}\n", .{b});
}

export fn c0_runtime_alloc_fail() void {
    std.debug.panic("Failed to allocate memory", .{});
}

export fn c0_runtime_null_pointer_panic() void {
    std.debug.panic("Unexpected null pointer", .{});
}

export fn lir_runtime_unreachable() void {
    std.debug.panic("Executed unreachable instruction", .{});
}

export fn c0_runtime_out_of_bounds_panic() void {
    std.debug.panic("Index out of bounds", .{});
}

const Task = struct {
    code: *anyopaque,
    len: usize,
};

extern fn c0_runtime_call(func: *anyopaque, cx: *Context, hp: [*]u8, self: *anyopaque) [*]u8;
extern fn _c0_main() callconv(.Naked) void;

var global_hp: [*]u8 = undefined;
var global_cx: Context = undefined;
var global_scheduler: *Scheduler = undefined;

export fn c0_runtime_par(task1: *Task, task2: *Task, hp: [*]u8) [*]u8 {
    Scheduler.set_worker_hp(hp);
    global_scheduler.par(@ptrCast(task1), @ptrCast(task2));
    return Scheduler.get_worker_hp();
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
    global_hp = HP.ptr;
    global_cx = Context{ .hp_lim = HP[HP.len..].ptr, .sp_lim = undefined };

    const scheduler = Scheduler.init(12, std.heap.smp_allocator) catch unreachable;
    global_scheduler = scheduler;
    defer scheduler.deinit();
    const closure = std.heap.smp_allocator.create(*anyopaque) catch unreachable;
    closure.* = @constCast(@ptrCast(&_c0_main));
    const job = Scheduler.Job.init(closure);
    scheduler.spawn(job);
    while (!job.done.load(.seq_cst)) {}
}

pub fn main() !void {
    try run();
}
