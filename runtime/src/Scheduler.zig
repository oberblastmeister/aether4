const std = @import("std");
const atomic = std.atomic;
const Thread = std.Thread;
const ArrayList = std.ArrayList;
const Futex = Thread.Futex;
const deque = @import("deque.zig");
const Context = @import("context.zig").Context;
const Allocator = std.mem.Allocator;
const smp_allocator = std.heap.smp_allocator;

extern fn c0_runtime_call(func: *anyopaque, cx: *Context, hp: [*]u8, self: *anyopaque) void;

const WorkerInfo = struct {
    id: u32,
    cx: Context,
    hp: [*]u8,
};

threadlocal var worker_info: WorkerInfo = undefined;

pub const Job = struct {
    closure: **anyopaque,
    done: atomic.Value(bool) = atomic.Value(bool).init(false),

    const Self = @This();

    pub fn init(closure: **anyopaque) *Job {
        const job = smp_allocator.create(Self) catch unreachable;
        job.* = .{ .closure = closure };
        return job;
    }

    // pub fn deinit(self: *Job) void {
    //     smp_allocator.destroy(self);
    // }
};

const Deque = deque.Deque(Job);

num_threads: u32,
num_finished_workers: atomic.Value(u32) = .{ .raw = 0 },
is_finished: atomic.Value(bool) = atomic.Value(bool).init(false),
spawned_threads: ArrayList(Thread),
wake_up_counter: atomic.Value(u32) = .{ .raw = 0 },
num_awake_workers: atomic.Value(u32) = .{ .raw = 0 },
deques: ArrayList(Deque),

const Scheduler = @This();

fn get_own_job(self: *Scheduler) ?*Job {
    const id = worker_info.id;
    const de = &self.deques.items[id];
    return Deque.pop_bottom(de);
}

fn steal_job(self: *Scheduler) ?*Job {
    for (0..self.num_threads) |id| {
        if (self.deques.items[id].pop_top().@"0") |job| {
            return job;
        }
    }
    return null;
}

fn get_job(self: *Scheduler) ?*Job {
    if (self.get_own_job()) |job| {
        return job;
    }
    return self.steal_job();
}

pub fn spawn(self: *Scheduler, job: *Job) void {
    const id = worker_info.id;
    _ = self.deques.items[id].push_bottom(job);
}

fn run_closure(closure: **anyopaque) void {
    const func = closure.*;
    c0_runtime_call(func, &worker_info.cx, worker_info.hp, @ptrCast(closure));
}

fn run_job(job: *Job) void {
    const func = job.closure.*;
    c0_runtime_call(func, &worker_info.cx, worker_info.hp, @ptrCast(job.closure));
    job.done.store(true, .seq_cst);
}

fn worker(self: *Scheduler, worker_id: u32, hp: []u8) !void {
    const context = Context{ .hp_lim = hp[hp.len..].ptr, .sp_lim = undefined };
    worker_info = .{ .id = worker_id, .cx = context, .hp = hp.ptr };
    while (!self.finished()) {
        if (self.get_job()) |job| {
            run_job(job);
        }
    }
}

pub fn finished(self: *Scheduler) bool {
    return self.is_finished.load(.seq_cst);
}

pub fn wait_for_work(self: *Scheduler) void {
    _ = self.num_awake_workers.fetchSub(1, .seq_cst);
    Futex.wait(&self.wake_up_counter, self.wake_up_counter.load(.seq_cst));
    _ = self.num_awake_workers.fetchAdd(1, .seq_cst);
}

pub fn par(self: *Scheduler, left: **anyopaque, right: **anyopaque) void {
    var right_job = Job{ .closure = right };
    self.spawn(&right_job);
    run_closure(left);
    while (!right_job.done.load(.seq_cst)) {
        if (self.get_job()) |job| {
            run_job(job);
        }
    }
}

pub fn init(num_threads: u32, allocator: Allocator) !*Scheduler {
    const scheduler = allocator.create(Scheduler) catch unreachable;
    scheduler.* = Scheduler{
        .num_threads = num_threads,
        .spawned_threads = ArrayList(Thread).init(allocator),
        .deques = ArrayList(Deque).init(allocator),
    };
    for (0..num_threads) |_| {
        scheduler.deques.append(.{}) catch unreachable;
    }
    for (0..num_threads) |worker_id| {
        const HP = try std.heap.page_allocator.alloc(u8, 1024 * 1024);
        @memset(HP, 0);
        const thread = Thread.spawn(.{}, worker, .{ scheduler, @as(u32, @intCast(worker_id)), HP }) catch unreachable;
        scheduler.spawned_threads.append(thread) catch unreachable;
    }
    return scheduler;
}

pub fn deinit(self: *Scheduler) void {
    self.is_finished.store(true, .seq_cst);
    for (self.spawned_threads.items) |spawned_thread| {
        spawned_thread.join();
    }
    self.spawned_threads.deinit();
    self.deques.deinit();
}

test "smoke" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    var scheduler = try Scheduler.init(24, allocator);
    defer scheduler.deinit();
}
