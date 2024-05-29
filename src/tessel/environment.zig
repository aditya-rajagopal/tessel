pub const Environment = @This();

memory: std.AutoHashMapUnmanaged(IdentifierIndex, StorageType),
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
    return env;
}

pub fn add_child(self: *Environment, allocator: Allocator, child: *Environment) !void {
    try self.child_envs.append(allocator, child);
}

pub fn deinit(self: *Environment, allocator: Allocator, object_pool: *ObjectPool) void {
    for (self.child_envs.items) |i| {
        i.deinit(allocator, object_pool);
    }
    self.child_envs.deinit(allocator);

    var kit = self.memory.valueIterator();
    while (kit.next()) |value_ptr| {
        object_pool.free(allocator, value_ptr.value);
    }

    self.memory.deinit(allocator);
    allocator.destroy(self);
}

pub fn get_object(self: *const Environment, key: IdentifierIndex) ObjectIndex {
    const output = self.memory.get(key);
    if (output) |o| {
        return o.value;
    } else {
        const env_to_check = self.parent_env orelse return null_object;
        return env_to_check.get_object(key);
    }
}

pub fn get_object_ptr(self: *const Environment, key: IdentifierIndex) Error!*StorageType {
    const output = self.memory.getPtr(key);
    if (output) |o| {
        return o;
    } else {
        const env_to_check = self.parent_env orelse return Error.NonExistantVariable;
        return env_to_check.get_object_ptr(key);
    }
}

pub fn create_variable(
    self: *Environment,
    allocator: Allocator,
    key: IdentifierIndex,
    value: ObjectIndex,
    tag: StorageType.Tag,
) Error!void {
    if (self.memory.contains(key)) {
        return Error.VariableAlreadyInitialised;
    }

    const storage = StorageType{ .tag = tag, .value = value };
    try self.memory.put(allocator, key, storage);
}

pub fn update_variable(self: *Environment, key: IdentifierIndex, value: ObjectIndex) Error!ObjectIndex {
    var value_ptr = try self.get_object_ptr(key);
    if (value_ptr.tag == .constant) {
        return Error.ConstVariableModification;
    }
    const old_value = value_ptr.value;
    value_ptr.value = value;
    return old_value;
}

pub fn print_env_hashmap_stderr(self: *Environment) void {
    var it = self.memory.iterator();
    std.debug.print("\n", .{});
    std.debug.print("Environment Hash map: \n", .{});
    while (it.next()) |value_ptr| {
        std.debug.print(
            "\tKey: \"{d}\"\t\t Modifiable: {s}\t\t Value: {any}\n",
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

test "environment" {
    const allocator = testing.allocator;
    var env = try Environment.Create(allocator);
    defer env.deinit(allocator);
    const enclosedEnv = try Environment.CreateEnclosed(allocator, env);
    try env.add_child(allocator, enclosedEnv);
    try env.create_variable(allocator, 0, 1, .constant);
    try enclosedEnv.create_variable(allocator, 1, 2, .variable);

    var value = env.get_object(1);
    try testing.expectEqual(0, value);
    value = env.get_object(0);
    try testing.expectEqual(1, value);
    value = enclosedEnv.get_object(1);
    try testing.expectEqual(2, value);
    value = enclosedEnv.get_object(0);
    try testing.expectEqual(1, value);
    var err = enclosedEnv.update_variable(0, 3);
    try testing.expectError(Error.ConstVariableModification, err);
    err = enclosedEnv.update_variable(2, 3);
    try testing.expectError(Error.NonExistantVariable, err);
    const err2 = env.create_variable(allocator, 0, 3, .constant);
    try testing.expectError(Error.VariableAlreadyInitialised, err2);
    try enclosedEnv.create_variable(allocator, 0, 3, .constant);
    value = enclosedEnv.get_object(0);
    try testing.expectEqual(3, value);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const ObjectPool = @import("object.zig");
const null_object = @import("object.zig").null_object;
const IdentifierIndex = @import("identifier_map.zig").IdentifierIndex;
