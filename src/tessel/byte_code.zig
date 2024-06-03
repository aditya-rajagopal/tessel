pub const ByteCode = @This();
instructions: []const u8,
constants: std.MultiArrayList(ObjectPool.InternalObject).Slice,

pub fn deinit(self: *ByteCode, allocator: Allocator) void {
    allocator.free(self.instructions);
    self.constants.deinit(allocator);
}

const ObjectPool = @import("object.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
