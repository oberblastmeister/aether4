const std = @import("std");
const atomic = std.atomic;

const Age = packed struct {
    tag: u32 = 0,
    top: u32 = 0,
};

pub fn Deque(comptime T: type) type {
    return struct {
        const Self = @This();

        const SIZE: usize = 2000;

        bot: atomic.Value(usize) = atomic.Value(usize).init(0),
        age: atomic.Value(Age) = atomic.Value(Age).init(.{}),
        items: [SIZE]atomic.Value(*T) = undefined,

        pub fn push_bottom(self: *Self, node: *T) bool {
            var local_bot = self.bot.load(.seq_cst);
            self.items[local_bot].store(node, .seq_cst);
            local_bot = local_bot + 1;
            if (local_bot == SIZE) {
                std.debug.panic("Scheduler queue overflow\n", .{});
            }
            self.bot.store(local_bot, .seq_cst);
            return (local_bot == 1);
        }

        pub fn pop_top(self: *Self) struct { ?*T, bool } {
            const old_age = self.age.load(.seq_cst);
            const local_bot = self.bot.load(.seq_cst);
            if (local_bot <= old_age.top) {
                return .{ null, true };
            }
            const node = self.items[old_age.top].load(.seq_cst);
            var new_age = old_age;
            new_age.top = new_age.top + 1;
            if (self.age.cmpxchgStrong(old_age, new_age, .seq_cst, .seq_cst)) |_| {} else {
                return .{ node, (local_bot == old_age.top + 1) };
            }
            return .{ null, (local_bot == old_age.top + 1) };
        }

        pub fn pop_bottom(self: *Self) ?*T {
            var local_bot = self.bot.load(.seq_cst);
            if (local_bot == 0) {
                return null;
            }
            local_bot = local_bot - 1;
            self.bot.store(local_bot, .seq_cst);
            const node = self.items[local_bot].load(.seq_cst);
            const old_age = self.age.load(.seq_cst);
            if (local_bot > old_age.top) {
                return node;
            }
            self.bot.store(0, .seq_cst);
            const new_age = Age{ .tag = old_age.tag + 1, .top = 0 };
            if (local_bot == old_age.top) {
                if (self.age.cmpxchgStrong(old_age, new_age, .seq_cst, .seq_cst)) |_| {} else {
                    return node;
                }
            }
            self.age.store(new_age, .seq_cst);
            return null;
        }
    };
}
