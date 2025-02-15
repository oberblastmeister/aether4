const std = @import("std");

extern fn _c0_main() u64;

export fn main() void {
    const res = _c0_main();
    std.debug.print("{}\n", .{res});
    std.process.exit(0);
}
