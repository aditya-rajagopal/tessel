pub const Environment = @This();

memory: std.StringHashMapUnmanaged(StorageType),
parent_env: ?*Environment,
child_envs: std.ArrayListUnmanaged(*Environment),
depth: u32 = 0,

pub const Error = error{
    VariableAlreadyInitialised,
    NonExistantVariable,
    ConstVariableModification,
    ExceedingMaxDepth,
} || Allocator.Error;

pub const MaxEnvDepth = 500;

pub fn Create(allocator: Allocator) !*Environment {
    const env = try allocator.create(Environment);
    env.memory = .{};
    env.parent_env = null;
    env.child_envs = .{};
    env.depth = 0;
    return env;
}

pub fn CreateEnclosed(allocator: Allocator, parent: *Environment) !*Environment {
    if (parent.depth == MaxEnvDepth) {
        return Error.ConstVariableModification;
    }
    const env = try allocator.create(Environment);
    env.memory = .{};
    env.parent_env = parent;
    env.child_envs = .{};
    env.depth = parent.depth + 1;
    parent.add_child(allocator, env);
    return env;
}

pub fn add_child(self: *Environment, allocator: Allocator, child: *Environment) !void {
    try self.child_envs.append(allocator, child);
}

pub fn deinit(self: *Environment, allocator: Allocator) void {
    var kit = self.memory.keyIterator();
    while (kit.next()) |value_ptr| {
        allocator.free(value_ptr.*);
    }
    for (self.child_envs.items) |i| {
        i.deinit(allocator);
    }
    self.child_envs.deinit(allocator);

    self.memory.deinit(allocator);
    allocator.destroy(self);
}

pub fn get_object(self: *const Environment, key: []const u8) ObjectIndex {
    const output = self.memory.get(key);
    if (output) |o| {
        return o.value;
    } else {
        const env_to_check = self.parent_env orelse return null_object;
        return env_to_check.get_object(key);
    }
}

pub fn get_ident_tag(self: *const Environment, key: []const u8) ?StorageType.Tag {
    const output = self.memory.get(key) orelse return StorageType.Tag.does_not_exist;
    return output.tag;
}

pub fn create_variable(
    self: *Environment,
    allocator: Allocator,
    key: []const u8,
    value: ObjectIndex,
    tag: StorageType.Tag,
) Error!void {
    // We create copies of the key so that if the source of the key
    // is uninitialized the hashmap is not affected. This does mean that we need to clean these up on evn destruction.
    if (self.memory.contains(key)) {
        return Error.VariableAlreadyInitialised;
    }

    var local_key = std.ArrayList(u8).init(allocator);
    try local_key.appendSlice(key);

    const storage = StorageType{ .tag = tag, .value = value };
    try self.memory.put(allocator, try local_key.toOwnedSlice(), storage);
    local_key.deinit();
}

pub fn update_variable(self: *Environment, key: []const u8, value: ObjectIndex) Error!void {
    var value_ptr = self.memory.getPtr(key) orelse return Error.NonExistantVariable;
    if (value_ptr.tag == .constant) {
        return Error.ConstVariableModification;
    }
    value_ptr.value = value;
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
    value: ObjectIndex,

    pub const Tag = enum {
        constant,
        variable,
        does_not_exist,
    };
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;
