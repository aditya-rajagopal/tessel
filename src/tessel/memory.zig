pub const Memory = @This();

memory: std.MultiArrayList(MemoryObject),
free_list: std.ArrayListUnmanaged(MemoryAddress),
stack_frames: std.ArrayListUnmanaged(StackFrame),
stack_ptr: u32,
unused_frames: std.ArrayListUnmanaged(FrameIndex),
allocator: Allocator,
reserved_memory: u32,

pub const MemoryError = error{ StackOverflow, PoppingEmptyStack };
pub const Error = MemoryError || Allocator.Error;

pub const StackSize = 4096;

pub const null_object: MemoryAddress = StackSize + 0;
pub const true_object: MemoryAddress = StackSize + 1;
pub const false_object: MemoryAddress = StackSize + 2;
pub const break_object: MemoryAddress = StackSize + 3;
pub const continue_object: MemoryAddress = StackSize + 4;

pub const reserved_objects: MemoryAddress = 5;

pub const BuiltInFn = *const fn (
    *Memory,
    *const Allocator,
    [*]const MemoryAddress,
    u32,
) callconv(.C) MemoryAddress;

pub fn init(allocator: Allocator) Allocator.Error!Memory {
    return initCapacity(allocator, 0);
}

pub fn initCapacity(allocator: Allocator, capacity: u32) Allocator.Error!Memory {
    const internal_capacity = capacity + reserved_objects;
    var pool = Memory{
        .memory = .{},
        .free_list = .{},
        .stack_frames = .{},
        .unused_frames = .{},
        .stack_ptr = 0,
        .allocator = allocator,
        .reserved_memory = reserved_objects,
    };
    try pool.free_list.ensureUnusedCapacity(allocator, internal_capacity);
    try pool.memory.ensureUnusedCapacity(allocator, StackSize + internal_capacity);

    // Creating the stack
    try pool.memory.resize(allocator, StackSize);
    @memset(pool.memory.items(.tag), .stack);
    @memset(pool.memory.items(.dtype), .null);
    @memset(pool.memory.items(.refs), 0);
    @memset(pool.memory.items(.data), MemoryObject.ObjectData{ .integer = 0 });

    for (0..internal_capacity) |i| {
        try pool.free_list.append(allocator, @as(MemoryAddress, @intCast(StackSize + internal_capacity - 1 - i)));
        try pool.memory.append(
            allocator,
            MemoryObject{
                .tag = .heap,
                .dtype = .null,
                .data = .{ .integer = 0 },
                .refs = 0,
            },
        );
    }

    std.debug.print("FreeList: {d}\n", .{pool.free_list.items});
    // 0 will always be a null node and will be referenced when .null is needed
    const null_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.memory.items(.tag)[null_loc] = .reserved;

    // 1 will be the true node and again will be referenced
    const true_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.memory.items(.tag)[true_loc] = .reserved;
    pool.memory.items(.dtype)[true_loc] = .boolean;
    pool.memory.items(.data)[true_loc].boolean = true;

    // 2 will be the false node and again will be referenced
    const false_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.memory.items(.tag)[false_loc] = .reserved;
    pool.memory.items(.dtype)[false_loc] = .boolean;
    pool.memory.items(.data)[false_loc].boolean = false;

    const break_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.memory.items(.tag)[break_loc] = .reserved;
    pool.memory.items(.dtype)[break_loc] = .break_statement;

    const continue_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.memory.items(.tag)[continue_loc] = .reserved;
    pool.memory.items(.dtype)[continue_loc] = .continue_statement;
    return pool;
}

pub fn deinit(self: *Memory) void {
    self.memory.deinit(self.allocator);
    self.stack_frames.deinit(self.allocator);
    self.unused_frames.deinit(self.allocator);
    self.free_list.deinit(self.allocator);
}

pub fn alloc(self: *Memory, dtype: Types, data: *const anyopaque) !MemoryAddress {
    if (dtype == .null) {
        return null_object;
    }

    if (dtype == .builtin) {
        const value: *const BuiltInFn = @ptrCast(@alignCast(data));
        try self.memory.append(
            self.allocator,
            MemoryObject{
                .dtype = dtype,
                .data = .{ .builtin = value.* },
                .refs = 0,
            },
        );
        self.reserved_memory += 1;
        return @as(u32, @intCast(self.memory.len - 1));
    }

    if (dtype == .break_statement) {
        return break_object;
    }
    if (dtype == .continue_statement) {
        return continue_object;
    }

    if (dtype == .boolean) {
        const value: *const bool = @ptrCast(@alignCast(data));
        if (value.*) {
            return true_object;
        } else {
            return false_object;
        }
    }

    var location: MemoryAddress = 0;
    if (self.free_list.items.len == 0) {
        location = @as(MemoryAddress, @intCast(try self.memory.addOne(self.allocator)));
    } else {
        location = self.free_list.popOrNull() orelse unreachable;
    }

    var memory_data = self.memory.items(.data);
    var memory_dtype = self.memory.items(.dtype);
    switch (dtype) {
        .integer => {
            const value: *const i64 = @ptrCast(@alignCast(data));
            memory_dtype[location] = .integer;
            memory_data[location].integer = value.*;
        },
        .return_expression => {
            const value: *const MemoryAddress = @ptrCast(@alignCast(data));
            memory_dtype[location] = .return_expression;
            memory_data[location].return_value = value.*;
        },
        .function_expression => {
            const value: *const MemoryObject.FunctionExpression = @ptrCast(@alignCast(data));
            memory_dtype[location] = .function_expression;
            memory_data[location].function.instruction_ptr = value.instruction_ptr;
            memory_data[location].function.env = value.env;
        },
        .runtime_error => {
            const value: *const MemoryObject.StringType = @ptrCast(@alignCast(data));
            memory_dtype[location] = .runtime_error;
            memory_data[location].runtime_error.ptr = value.ptr;
            memory_data[location].runtime_error.len = value.len;
        },
        .string => {
            const value: *const MemoryObject.StringType = @ptrCast(@alignCast(data));
            memory_dtype[location] = .string;
            memory_data[location].string_type = try self.allocator.create(std.ArrayListUnmanaged(u8));
            memory_data[location].string_type.* = .{};
            try memory_data[location].string_type.appendSlice(self.allocator, value.ptr[0..value.len]);
        },
        .array => {
            const value: *const MemoryObject.ArrayType = @ptrCast(@alignCast(data));
            memory_dtype[location] = .array;
            memory_data[location].array = try self.allocator.create(
                std.ArrayListUnmanaged(MemoryAddress),
            );
            memory_data[location].array.* = .{};
            try memory_data[location].array.appendSlice(self.allocator, value.data);
        },
        .hash_map => {
            const value: *const u32 = @ptrCast(@alignCast(data));
            memory_dtype[location] = .hash_map;
            memory_data[location].hash_map.map = try self.allocator.create(
                std.AutoHashMapUnmanaged(
                    MemoryObject.HashKey,
                    MemoryAddress,
                ),
            );
            memory_data[location].hash_map.keys = try self.allocator.create(
                std.ArrayListUnmanaged(
                    MemoryAddress,
                ),
            );
            memory_data[location].hash_map.map.* = .{};
            memory_data[location].hash_map.keys.* = .{};
            try memory_data[location].hash_map.map.ensureUnusedCapacity(self.allocator, value.*);
            try memory_data[location].hash_map.keys.ensureUnusedCapacity(self.allocator, value.*);
        },
        .builtin => unreachable,
        .null => unreachable,
        .boolean => unreachable,
        .break_statement => unreachable,
        .continue_statement => unreachable,
    }
    self.memory.items(.refs)[location] = 0;
    self.memory.items(.tag)[location] = .heap;
    try self.free_list.ensureTotalCapacity(self.allocator, self.memory.len);
    return location;
}

pub fn free(self: *Memory, ptr: MemoryAddress) void {
    std.debug.assert(ptr <= self.memory.len);
    std.debug.assert(ptr >= StackSize);
    if (ptr - StackSize < self.reserved_memory) {
        return;
    }
    if (self.memory.items(.refs)[ptr] == 0) {
        self.free_possible_memory(ptr);
        self.free_list.appendAssumeCapacity(ptr);
    } else {
        self.memory.items(.refs)[ptr] -= 1;
    }
}

fn free_possible_memory(self: *Memory, ptr: MemoryAddress) void {
    const tag = self.memory.items(.dtype)[ptr];
    var memory_data: []MemoryObject.ObjectData = self.memory.items(.data);
    switch (tag) {
        .integer => {},
        .return_expression => {},
        .function_expression => {},
        .runtime_error => {
            self.allocator.free(
                memory_data[ptr].runtime_error.ptr[0..memory_data[ptr].runtime_error.len],
            );
            memory_data[ptr].runtime_error.len = 0;
        },
        .string => {
            const str_ptr = memory_data[ptr].string_type;
            str_ptr.deinit(self.allocator);
            self.allocator.destroy(str_ptr);
        },
        .hash_map => {
            const map = memory_data[ptr].hash_map.map;
            var value_iter = map.valueIterator();
            while (value_iter.next()) |value| {
                self.free(value.*);
            }
            map.deinit(self.allocator);
            const keys: *std.ArrayListUnmanaged(MemoryAddress) = memory_data[ptr].hash_map.keys;
            for (keys.items) |i| {
                self.free(i);
            }
            keys.deinit(self.allocator);
            self.allocator.destroy(map);
            self.allocator.destroy(keys);
        },
        .array => {
            const arr_ptr = memory_data[ptr].array;
            for (arr_ptr.items) |pos| {
                self.free(pos);
            }
            arr_ptr.deinit(self.allocator);
            self.allocator.destroy(arr_ptr);
        },
        .null, .boolean, .break_statement, .continue_statement, .builtin => unreachable,
    }
    self.memory.items(.dtype)[ptr] = .null;
    self.memory.items(.data)[ptr].integer = 0;
}

pub fn get_dtype(self: *Memory, ptr: MemoryAddress) Types {
    std.debug.assert(ptr <= self.memory.len);
    return self.memory.items(.dtype)[ptr];
}

pub fn get_tag(self: *Memory, ptr: MemoryAddress) MemoryObject.Tag {
    std.debug.assert(ptr <= self.memory.len);
    return self.memory.items(.tag)[ptr];
}

pub fn get_refs(self: *Memory, ptr: MemoryAddress) u32 {
    std.debug.assert(ptr <= self.memory.len);
    return self.memory.items(.refs)[ptr];
}

pub fn get_data(self: *Memory, ptr: MemoryAddress) MemoryObject.ObjectData {
    std.debug.assert(ptr <= self.memory.len);
    return self.memory.items(.data)[ptr];
}

pub fn get(self: *Memory, ptr: MemoryAddress) MemoryObject {
    std.debug.assert(ptr <= self.memory.len);
    return self.memory.get(ptr);
}

pub fn stack_push(self: *Memory, element: MemoryObject) MemoryError!void {
    if (self.stack_ptr >= StackSize) {
        return Error.StackOverflow;
    }
    self.memory.set(self.stack_ptr, element);
    self.stack_ptr += 1;
}

pub fn stack_pop(self: *Memory) MemoryError!MemoryObject {
    if (self.stack_ptr == 0) {
        return Error.PoppingEmptyStack;
    }
    self.stack_ptr -= 1;
    const value = self.memory.get(self.stack_ptr);
    self.memory.items(.dtype)[self.stack_ptr] = .null;
    return value;
}

pub const MemoryAddress = u32;

pub const FrameIndex = u32;

pub const MemoryObject = struct {
    tag: Tag = .stack,
    dtype: Types,
    data: ObjectData,
    refs: u32,

    pub const ObjectData = extern union {
        // Passed by value
        integer: i64,
        boolean: bool,
        return_value: MemoryAddress,
        function: FunctionExpression,
        // Passed By reference,
        runtime_error: StringType,
        builtin: BuiltInFn,
        string_type: *std.ArrayListUnmanaged(u8),
        array: *std.ArrayListUnmanaged(MemoryAddress),
        hash_map: HashData,
    };

    pub const HashData = extern struct {
        map: *std.AutoHashMapUnmanaged(HashKey, MemoryAddress),
        keys: *std.ArrayListUnmanaged(MemoryAddress),
    };

    pub const HashKey = struct {
        type: HashTag,
        hash: u64,
        pub const HashTag = enum {
            pos_integer_key,
            neg_integer_key,
            bool_key,
            string_key,
        };

        pub fn create(key: MemoryObject) HashKey {
            switch (key.type) {
                .integer => {
                    if (key.data.integer >= 0) {
                        return .{
                            .type = .pos_integer_key,
                            .hash = @as(u64, @intCast(key.data.integer)),
                        };
                    } else {
                        return .{
                            .type = .neg_integer_key,
                            .hash = @as(u64, @intCast(-key.data.integer)),
                        };
                    }
                },
                .boolean => {
                    if (key.data.boolean) {
                        return .{
                            .type = .bool_key,
                            .hash = @as(u64, @intCast(1)),
                        };
                    } else {
                        return .{
                            .type = .bool_key,
                            .hash = @as(u64, @intCast(0)),
                        };
                    }
                },
                .string => {
                    const slice = key.data.string_type.ptr[0..key.data.string_type.len];
                    const hash = std.hash.Wyhash.hash(0, slice);
                    return .{
                        .type = .string_key,
                        .hash = hash,
                    };
                },
                else => unreachable,
            }
        }
    };

    const FunctionExpression = extern struct {
        env: StackFrame,
        instruction_ptr: u32,
    };

    pub const ArrayType = struct {
        data: []const MemoryAddress,
    };

    pub const StringType = extern struct {
        ptr: [*]u8,
        len: u32,
    };

    pub const Tag = enum {
        reserved,
        builtin,
        heap,
        stack,
    };
};

pub const Types = enum(u8) {
    integer,
    boolean,
    string,
    array,
    hash_map,
    return_expression,
    break_statement,
    continue_statement,
    function_expression,
    runtime_error,
    builtin,
    null,
};

pub const StackFrame = extern struct {
    stack_start_ptr: u32,
    parent: FrameIndex,
};

test "Memory init" {
    var timer = try std.time.Timer.start();
    var memory = try Memory.init(testing.allocator);
    const end = timer.read();
    defer memory.deinit();
    std.debug.print("Time to init: {s}\n", .{std.fmt.fmtDuration(end)});
    std.debug.print("\n", .{});
    try testing.expect(memory.memory.items(.tag)[StackSize - 1] == .stack);
    try testing.expect(memory.memory.items(.dtype)[StackSize] == .null);
    try testing.expect(memory.memory.items(.dtype)[StackSize + 1] == .boolean);
    try testing.expect(memory.memory.items(.dtype)[StackSize + 2] == .boolean);
    try testing.expect(memory.memory.items(.dtype)[StackSize + 3] == .break_statement);
    try testing.expect(memory.memory.items(.dtype)[StackSize + 4] == .continue_statement);
    try memory.stack_push(MemoryObject{
        .dtype = .integer,
        .data = .{ .integer = 15 },
        .refs = 0,
    });
    try testing.expect(memory.stack_ptr == 1);
    try testing.expect(memory.memory.items(.dtype)[0] == .integer);
    try testing.expect(memory.memory.items(.data)[0].integer == 15);
    const value = try memory.stack_pop();

    try testing.expect(value.dtype == .integer);
    try testing.expect(value.data.integer == 15);
    try testing.expect(memory.memory.items(.dtype)[0] == .null);

    const int_data: i64 = 10;
    const int_ptr = try memory.alloc(.integer, @ptrCast(&int_data));
    try testing.expect(memory.stack_ptr == 0);
    const int_ptr_data = memory.memory.get(int_ptr);
    try testing.expect(int_ptr_data.tag == .heap);
    try testing.expect(int_ptr_data.dtype == .integer);
    try testing.expect(int_ptr_data.data.integer == int_data);
    try testing.expect(int_ptr_data.refs == 0);

    const bool_data: bool = true;
    const bool_ptr = try memory.alloc(.boolean, @ptrCast(&bool_data));
    var bool_ptr_data = memory.memory.get(bool_ptr);
    try testing.expect(bool_ptr == true_object);
    try testing.expect(bool_ptr_data.tag == .reserved);
    try testing.expect(bool_ptr_data.dtype == .boolean);
    try testing.expect(bool_ptr_data.data.boolean == bool_data);
    try testing.expect(bool_ptr_data.refs == 0);

    const string_data = try std.fmt.allocPrint(testing.allocator, "This is a test string: {d}", .{int_data});
    defer testing.allocator.free(string_data);
    const string_ptr = try memory.alloc(.string, @ptrCast(&string_data));
    const string_ptr_data = memory.memory.get(string_ptr);

    try testing.expect(string_ptr_data.tag == .heap);
    try testing.expect(string_ptr_data.dtype == .string);
    try testing.expect(string_ptr_data.refs == 0);
    memory.free(string_ptr);
    memory.free(bool_ptr);
    bool_ptr_data = memory.memory.get(bool_ptr);
    try testing.expect(bool_ptr == true_object);
    try testing.expect(bool_ptr_data.tag == .reserved);
    try testing.expect(bool_ptr_data.dtype == .boolean);
    try testing.expect(bool_ptr_data.data.boolean == bool_data);
    try testing.expect(bool_ptr_data.refs == 0);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
