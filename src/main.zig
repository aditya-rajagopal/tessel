const std = @import("std");
const repl = @import("repl/repl.zig");

pub fn main() !void {
    try repl.start();
}
