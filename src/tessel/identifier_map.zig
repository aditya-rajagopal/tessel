pub const IdentifierMap = @This();

map: std.StringHashMapUnmanaged(IdentifierIndex),
current_index: u32,

pub const IdentifierIndex = u32;

pub fn init() IdentifierMap {
    return IdentifierMap{
        .map = .{},
        .current_index = 0,
    };
}

pub fn deinit(self: *IdentifierMap, allocator: Allocator) void {
    var kit = self.map.keyIterator();
    while (kit.next()) |value_ptr| {
        allocator.free(value_ptr.*);
    }
    self.map.deinit(allocator);
}

pub fn create(self: *IdentifierMap, allocator: Allocator, key: []const u8) !IdentifierIndex {
    if (self.map.contains(key)) {
        return self.map.get(key) orelse unreachable;
    } else {
        var local_key = std.ArrayList(u8).init(allocator);
        try local_key.appendSlice(key);
        try self.map.put(allocator, try local_key.toOwnedSlice(), self.current_index);
        self.current_index += 1;
        return self.current_index - 1;
    }
}

pub fn print_env_hashmap_stderr(self: *IdentifierMap) void {
    var it = self.map.iterator();
    std.debug.print("\n", .{});
    std.debug.print("Environment Hash map: \n", .{});
    while (it.next()) |value_ptr| {
        std.debug.print(
            "\tKey: \"{s}\"\t Value: {any}\n",
            .{ value_ptr.key_ptr.*, value_ptr.value_ptr.* },
        );
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;
