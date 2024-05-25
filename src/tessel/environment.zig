pub const Environment = @This();

memory: std.StringHashMapUnmanaged(StorageType),

pub const Error = error{ VariableAlreadyInitialised, NonExistantVariable, ConstVariableModification } || Allocator.Error;

pub fn Create(allocator: Allocator) !*Environment {
    const env = try allocator.create(Environment);
    env.memory = .{};
    return env;
}

pub fn deinit(self: *Environment, allocator: Allocator) void {
    var it = self.memory.valueIterator();
    while (it.next()) |value_ptr| {
        value_ptr.value.deinit(allocator);
    }
    var kit = self.memory.keyIterator();
    while (kit.next()) |value_ptr| {
        allocator.free(value_ptr.*);
    }
    self.memory.deinit(allocator);
    allocator.destroy(self);
}

pub fn get_object(self: *const Environment, key: []const u8, allocator: Allocator) !Object {
    const output = self.memory.get(key) orelse return .null;
    const out = try output.value.copy(allocator);
    return out;
}

pub fn get_ident_tag(self: *const Environment, key: []const u8) ?StorageType.Tag {
    const output = self.memory.get(key) orelse return null;
    return output.tag;
}

pub fn create_variable(
    self: *Environment,
    allocator: Allocator,
    key: []const u8,
    value: Object,
    tag: StorageType.Tag,
) Error!void {
    // We create copies of the key and value so that if the source of the key or value
    // are uninitialized the hashmap is not affected. This does mean that we need to clean these up on evn destruction.
    if (self.memory.contains(key)) {
        return Error.VariableAlreadyInitialised;
    }

    const local_value = try value.copy(allocator);
    var local_key = std.ArrayList(u8).init(allocator);
    try local_key.appendSlice(key);

    const storage = StorageType{ .tag = tag, .value = local_value };
    try self.memory.put(allocator, try local_key.toOwnedSlice(), storage);
    local_key.deinit();
}

pub fn update_variable(self: *Environment, allocator: Allocator, key: []const u8, value: Object) Error!void {
    var value_ptr = self.memory.getPtr(key) orelse return Error.NonExistantVariable;
    if (value_ptr.tag == .constant) {
        return Error.ConstVariableModification;
    }
    value_ptr.value.deinit(allocator);
    const local_value = try value.copy(allocator);
    value_ptr.value = local_value;
}

pub fn print_env_hashmap_stderr(self: *Environment) void {
    var it = self.memory.iterator();
    std.debug.print("\tEnvironment Hash map: \n", .{});
    while (it.next()) |value_ptr| {
        std.debug.print(
            "\tKey: \"{s}\"\t\t Modifiable: {s}\t\t Value: {any}\n",
            .{ value_ptr.key_ptr.*, @tagName(value_ptr.value_ptr.tag), value_ptr.value_ptr.value },
        );
    }
}

pub const StorageType = struct {
    tag: Tag,
    value: Object,

    pub const Tag = enum {
        constant,
        variable,
    };
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const Object = @import("object.zig").Object;
