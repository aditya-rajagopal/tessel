pub const EnvironmentPool = @This();

environment_pool: std.MultiArrayList(EnvironmentInstance),
free_list: std.ArrayListUnmanaged(EnvironmentIndex),

pub const EnvironmentIndex = u32;

pub const global_env: EnvironmentIndex = 0;

const reserved_capacity = 1;

pub const EnvPoolError = error{AccessingFreeEnv};

pub const Error = EnvPoolError || Allocator.Error || Environment.Error;

pub fn init(allocator: Allocator) !EnvironmentPool {
    return initCapacity(allocator, 0);
}

pub fn initCapacity(allocator: Allocator, capacity: u32) !EnvironmentPool {
    const internal_capacity = capacity + reserved_capacity;
    var pool = EnvironmentPool{
        .environment_pool = .{},
        .free_list = .{},
    };
    try pool.environment_pool.ensureUnusedCapacity(allocator, internal_capacity);
    try pool.free_list.ensureUnusedCapacity(allocator, internal_capacity);
    for (0..internal_capacity) |i| {
        try pool.free_list.append(allocator, @as(EnvironmentIndex, @intCast(internal_capacity - 1 - i)));
        try pool.environment_pool.append(
            allocator,
            EnvironmentInstance{
                .tag = .free,
                .data = null,
                .child_nodes = .{},
            },
        );
    }

    // 0 will always be a null node and will be referenced when .null is needed
    const global = pool.free_list.popOrNull() orelse unreachable;
    pool.environment_pool.items(.tag)[global] = .global;
    pool.environment_pool.items(.data)[global] = try Environment.Create(allocator);
    return pool;
}

pub fn deinit(self: *EnvironmentPool, allocator: Allocator, object_pool: *ObjectPool) void {
    for (0..self.environment_pool.len) |i| {
        if (self.environment_pool.get(i).data) |env| {
            env.deinit(allocator, object_pool);
        }
        self.environment_pool.items(.child_nodes)[i].deinit(allocator);
    }
    self.environment_pool.deinit(allocator);
    self.free_list.deinit(allocator);
}

pub fn add_child(
    self: *EnvironmentPool,
    env_loc: EnvironmentIndex,
    allocator: Allocator,
    child: EnvironmentIndex,
) (EnvPoolError || Allocator.Error)!void {
    std.debug.assert(env_loc < self.environment_pool.len);
    const env_tag = self.environment_pool.items(.tag)[env_loc];
    if (env_tag == .free) {
        return Error.AccessingFreeEnv;
    }
    try self.environment_pool.items(.child_nodes)[env_loc].append(allocator, child);
}

pub fn create_env(self: *EnvironmentPool, allocator: Allocator, parent_loc: EnvironmentIndex) !EnvironmentIndex {
    std.debug.assert(parent_loc < self.environment_pool.len);

    const parent_env = self.environment_pool.items(.data)[parent_loc].?;
    var location: EnvironmentIndex = 0;
    if (self.free_list.items.len == 0) {
        const env = try Environment.CreateEnclosed(allocator, parent_env);
        location = @as(u32, @intCast(self.environment_pool.len));
        try self.environment_pool.append(allocator, .{
            .tag = .inuse,
            .data = env,
            .child_nodes = .{},
        });
    } else {
        location = self.free_list.popOrNull() orelse unreachable;
        self.environment_pool.items(.tag)[location] = .inuse;
        self.environment_pool.items(.child_nodes)[location] = .{};
        if (self.environment_pool.items(.data)[location]) |env| {
            env.parent_env = parent_env;
            env.child_envs = .{};
            env.depth = parent_env.depth + 1;
        } else {
            const env = try Environment.CreateEnclosed(allocator, parent_env);
            self.environment_pool.items(.data)[location] = env;
        }
    }
    try self.free_list.ensureTotalCapacity(allocator, self.environment_pool.len);
    return location;
}

pub fn free_env(self: *EnvironmentPool, allocator: Allocator, env_loc: EnvironmentIndex, object_pool: *ObjectPool) void {
    std.debug.assert(env_loc < self.environment_pool.len);
    if (env_loc == global_env) {
        return;
    }

    const env_tag = self.environment_pool.items(.tag)[env_loc];
    if (env_tag == .free) {
        return;
    }

    self.environment_pool.items(.tag)[env_loc] = .free;
    const env: *Environment = self.environment_pool.items(.data)[env_loc].?;
    env.parent_env = null;
    env.depth = 0;

    var kit = env.memory.valueIterator();
    while (kit.next()) |value_ptr| {
        object_pool.free(allocator, value_ptr.value);
    }
    env.memory.clearRetainingCapacity();

    for (self.environment_pool.items(.child_nodes)[env_loc].items) |i| {
        self.free_env(allocator, i, object_pool);
    }
    self.environment_pool.items(.child_nodes)[env_loc].clearRetainingCapacity();
    self.free_list.appendAssumeCapacity(env_loc);
}

pub fn get_object(self: *EnvironmentPool, env_loc: EnvironmentIndex, hash: SymbolIndex) EnvPoolError!ObjectIndex {
    std.debug.assert(env_loc < self.environment_pool.len);
    const env_tag = self.environment_pool.items(.tag)[env_loc];
    if (env_tag == .free) {
        return Error.AccessingFreeEnv;
    }
    const env: *Environment = self.environment_pool.items(.data)[env_loc].?;
    return env.get_object(hash);
}

pub fn create_variable(
    self: *EnvironmentPool,
    env_loc: EnvironmentIndex,
    allocator: Allocator,
    key: SymbolIndex,
    value: ObjectIndex,
    tag: Environment.StorageType.Tag,
) Error!void {
    std.debug.assert(env_loc < self.environment_pool.len);
    const env_tag = self.environment_pool.items(.tag)[env_loc];
    if (env_tag == .free) {
        return Error.AccessingFreeEnv;
    }
    const env: *Environment = self.environment_pool.items(.data)[env_loc].?;
    return env.create_variable(allocator, key, value, tag);
}

pub fn update_variable(
    self: *EnvironmentPool,
    env_loc: EnvironmentIndex,
    key: SymbolIndex,
    value: ObjectIndex,
) Error!ObjectIndex {
    std.debug.assert(env_loc < self.environment_pool.len);
    const env_tag = self.environment_pool.items(.tag)[env_loc];
    if (env_tag == .free) {
        return Error.AccessingFreeEnv;
    }
    const env: *Environment = self.environment_pool.items(.data)[env_loc].?;
    return env.update_variable(key, value);
}

pub fn print_to_stderr(self: *EnvironmentPool) void {
    std.debug.print("Free List: {any}\n", .{self.free_list});
    std.debug.print("Environment pool: \n", .{});
    for (0..self.environment_pool.len) |i| {
        const env = self.environment_pool.get(i);
        std.debug.print(
            "\tPosition: {d}\t tag:{s} \t Children: {any}\n",
            .{
                i,
                @tagName(env.tag),
                env.child_nodes.items,
            },
        );
        std.debug.print("\t\t", .{});
        if (env.data) |e| {
            e.print_env_hashmap_stderr();
            std.debug.print("\n", .{});
        } else {
            std.debug.print("Uninitialized Environment\n", .{});
        }
    }
}

pub const EnvironmentInstance = struct {
    tag: EnvironmentType,
    data: ?*Environment,
    child_nodes: std.ArrayListUnmanaged(EnvironmentIndex),
};

pub const EnvironmentType = enum {
    global,
    inuse,
    free,
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const ObjectPool = @import("object.zig");
const Environment = @import("environment.zig");
const null_object = @import("object.zig").null_object;
const SymbolIndex = @import("symbol_table.zig").SymbolIndex;
