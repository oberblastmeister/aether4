const std = @import("std");

pub const Context = extern struct {
    hp_lim: *anyopaque,
    sp_lim: *anyopaque,
};
