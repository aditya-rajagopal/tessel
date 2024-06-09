pub const Memory = @This();

memory: std.MultiArrayList(MemoryObject),
free_list: std.ArrayListUnmanaged(MemoryAddress),
stack_ptr: u32,
reserved_memory: u32,

stack_frames: std.ArrayListUnmanaged(StackFrame),
unused_frames: std.ArrayListUnmanaged(FrameIndex),

instructions: std.ArrayList(u8),
ins_ptr: u32,
constants: std.ArrayListUnmanaged(MemoryAddress),
globals: []MemoryAddress,

allocator: Allocator,

pub const ConstantID = u32;
pub const GlobalID = u32;

pub const MemoryError = error{ StackOverflow, PoppingEmptyStack };
pub const Error = MemoryError || Allocator.Error;

pub const stack_limit = 8;
pub const globals_limit = 4096;
pub const instruction_init_size = 4096;

pub const null_object: MemoryAddress = stack_limit + 0;
pub const true_object: MemoryAddress = stack_limit + 1;
pub const false_object: MemoryAddress = stack_limit + 2;
pub const break_object: MemoryAddress = stack_limit + 3;
pub const continue_object: MemoryAddress = stack_limit + 4;

pub const reserved_objects: MemoryAddress = stack_limit + 5;

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
    const internal_capacity = capacity + reserved_objects - stack_limit;
    var pool = Memory{
        .memory = .{},
        .free_list = .{},
        .stack_frames = .{},
        .unused_frames = .{},
        .stack_ptr = 0,
        .allocator = allocator,
        .instructions = try std.ArrayList(u8).initCapacity(allocator, instruction_init_size),
        .ins_ptr = 0,
        .constants = .{},
        .globals = try allocator.alloc(MemoryAddress, globals_limit),
        .reserved_memory = reserved_objects,
    };
    try pool.free_list.ensureUnusedCapacity(allocator, internal_capacity);
    try pool.memory.ensureUnusedCapacity(allocator, stack_limit + internal_capacity);

    // Creating the stack
    try pool.memory.resize(allocator, stack_limit + internal_capacity);
    var memory_slice = pool.memory.slice();
    const tag_slice = memory_slice.items(.tag);
    const dtype_slice = memory_slice.items(.dtype);
    const data_slice = memory_slice.items(.data);
    const ref_slice = memory_slice.items(.refs);

    @memset(tag_slice, .stack);
    @memset(memory_slice.items(.dtype), .null);
    // @memset(memory_slice.items(.refs), 0);
    // @memset(memory_slice.items(.data), MemoryObject.ObjectData{ .integer = 0 });
    @memset(pool.globals, stack_limit);

    for (0..internal_capacity) |i| {
        const loc = @as(MemoryAddress, @intCast(stack_limit + internal_capacity - 1 - i));
        pool.free_list.appendAssumeCapacity(loc);
        tag_slice[loc] = .heap;
    }

    // 0 will always be a null node and will be referenced when .null is needed
    const null_loc = pool.free_list.popOrNull() orelse unreachable;
    tag_slice[null_loc] = .reserved;
    ref_slice[null_loc] = 0;

    // 1 will be the true node and again will be referenced
    const true_loc = pool.free_list.popOrNull() orelse unreachable;
    tag_slice[true_loc] = .reserved;
    dtype_slice[true_loc] = .boolean;
    data_slice[true_loc].boolean = true;
    ref_slice[true_loc] = 0;

    // 2 will be the false node and again will be referenced
    const false_loc = pool.free_list.popOrNull() orelse unreachable;
    tag_slice[false_loc] = .reserved;
    dtype_slice[false_loc] = .boolean;
    data_slice[false_loc].boolean = false;
    ref_slice[false_loc] = 0;

    const break_loc = pool.free_list.popOrNull() orelse unreachable;
    tag_slice[break_loc] = .reserved;
    dtype_slice[break_loc] = .break_statement;
    ref_slice[break_loc] = 0;

    const continue_loc = pool.free_list.popOrNull() orelse unreachable;
    tag_slice[continue_loc] = .reserved;
    dtype_slice[continue_loc] = .continue_statement;
    ref_slice[continue_loc] = 0;
    return pool;
}

pub fn deinit(self: *Memory) void {
    self.stack_frames.deinit(self.allocator);
    self.unused_frames.deinit(self.allocator);
    self.free_list.deinit(self.allocator);
    self.allocator.free(self.globals);
    self.instructions.deinit();
    self.constants.deinit(self.allocator);

    for (0..self.memory.len) |i| {
        const memory_slice = self.memory.slice();
        const tag = memory_slice.items(.tag)[i];
        if ((tag == .constant or tag == .heap) and i < stack_limit) {
            continue;
        }
        self.destroy(@as(MemoryAddress, @intCast(i)));
    }
    self.memory.deinit(self.allocator);
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
    try self.create(location, dtype, data);

    try self.free_list.ensureTotalCapacity(self.allocator, self.memory.len - stack_limit + 1);
    return location;
}

pub fn create(self: *Memory, location: MemoryAddress, dtype: Types, data: *const anyopaque) !void {
    var memory_data: []MemoryObject.ObjectData = self.memory.items(.data);
    var memory_dtype: []Types = self.memory.items(.dtype);
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
            // memory_data[location].function.instruction_ptr = value.instruction_ptr;
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
            memory_data[location].string_type.* = std.ArrayListUnmanaged(u8).fromOwnedSlice(value.ptr[0..value.len]);
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
}

pub fn dupe(self: *Memory, addr: MemoryAddress) !MemoryAddress {
    const data = self.get(addr);

    switch (data.dtype) {
        .string => {
            var output = try data.data.string_type.clone(self.allocator);
            const obj = try self.alloc(.string, @ptrCast(&output.toOwnedSlice(self.allocator)));
            return obj;
        },
        inline else => return addr,
    }
}

pub fn free(self: *Memory, ptr: MemoryAddress) void {
    std.debug.assert(ptr <= self.memory.len);
    // std.debug.assert(ptr >= stack_limit);
    if (ptr < self.reserved_memory and ptr >= stack_limit) {
        return;
    }
    const memory_slice = self.memory.slice();
    const refs = memory_slice.items(.refs);
    const tag = memory_slice.items(.tag)[ptr];
    if (tag == .constant) {
        return;
    }

    if (refs[ptr] == 0) {
        self.destroy(ptr);
        if (tag != .stack) {
            self.free_list.appendAssumeCapacity(ptr);
        }
    } else {
        refs[ptr] -= 1;
    }
}

fn destroy(self: *Memory, ptr: MemoryAddress) void {
    const memory_slice = self.memory.slice();
    const tag: Types = memory_slice.items(.dtype)[ptr];
    var memory_data: []MemoryObject.ObjectData = memory_slice.items(.data);
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
        .null, .boolean, .break_statement, .continue_statement, .builtin => return,
    }
    memory_slice.items(.dtype)[ptr] = .null;
    memory_slice.items(.data)[ptr].integer = 0;
}

pub fn register_constant(self: *Memory, dtype: Types, data: *const anyopaque) !ConstantID {
    const ptr = try self.alloc(dtype, data);
    self.memory.items(.tag)[ptr] = .constant;
    try self.constants.append(self.allocator, ptr);
    return @as(ConstantID, @intCast(self.constants.items.len - 1));
}

pub fn get_constant(self: *Memory, id: ConstantID) MemoryObject {
    std.debug.assert(id < self.constants.items.len);
    return self.memory.get(self.constants.items[id]);
}

// Give ownership of top of stack to global id
pub fn set_global(self: *Memory, id: GlobalID) Error!void {
    // TODO: WHen you want to set a global constant variable just point to the constant
    std.debug.assert(id < globals_limit);

    var ptr = self.globals[id];
    self.stack_ptr -= 1;

    const data: MemoryObject = self.memory.get(self.stack_ptr);
    var memory_slice = self.memory.slice();

    if (ptr > reserved_objects) {
        // if (memory_slice.items(.tag)[ptr] != .constant) {
        self.free(ptr);
        // }
    }

    switch (data.dtype) {
        .boolean => {
            self.globals[id] = if (data.data.boolean) true_object else false_object;
            return;
        },
        .null => {
            self.globals[id] = null_object;
            return;
        },
        .continue_statement => {
            self.globals[id] = continue_object;
            return;
        },
        .break_statement => {
            self.globals[id] = break_object;
            return;
        },
        .builtin => unreachable,
        else => {},
    }

    if (data.tag == .constant) {
        ptr = try self.dupe(self.stack_ptr);
    }

    if (ptr < self.reserved_memory) {
        const dummy: i64 = @intCast(id);
        ptr = try self.alloc(.integer, @ptrCast(&dummy));
        self.globals[id] = ptr;
        memory_slice = self.memory.slice();
        memory_slice.set(ptr, data);
    } else {
        self.globals[id] = ptr;
    }

    memory_slice = self.memory.slice();
    memory_slice.items(.data)[self.stack_ptr].integer = 0;
    memory_slice.items(.dtype)[self.stack_ptr] = .null;
}

pub fn get_global(self: *Memory, id: GlobalID) !void {
    std.debug.assert(id < globals_limit);
    const ptr = self.globals[id];
    std.debug.assert(ptr != stack_limit);
    self.increase_ref(ptr);
    try self.stack_push(self.get(ptr));
}

pub fn shrink_constants(self: *Memory, new_len: usize) void {
    std.debug.assert(new_len <= self.constants.items.len);
    for (new_len..self.constants.items.len) |i| {
        self.free(@as(MemoryAddress, @intCast(i)));
    }
    self.constants.shrinkRetainingCapacity(new_len);
}

pub fn increase_ref(self: *Memory, ptr: MemoryAddress) void {
    std.debug.assert(ptr < self.memory.len);
    std.debug.assert(ptr >= reserved_objects);
    self.memory.items(.refs)[ptr] += 1;
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
    if (self.stack_ptr >= stack_limit) {
        return Error.StackOverflow;
    }

    var memory_slice = self.memory.slice();
    if (memory_slice.items(.dtype)[self.stack_ptr] != .null) {
        self.free(self.stack_ptr);
    }

    memory_slice.set(self.stack_ptr, element);
    memory_slice.items(.tag)[self.stack_ptr] = element.tag;
    self.stack_ptr += 1;
}

pub fn stack_push_create(self: *Memory, dtype: Types, data: *const anyopaque) Error!void {
    if (self.stack_ptr >= stack_limit) {
        return Error.StackOverflow;
    }
    var memory_slice = self.memory.slice();
    const stack_ptr_dtype = memory_slice.items(.dtype)[self.stack_ptr];
    const stack_ptr_tag = memory_slice.items(.tag)[self.stack_ptr];
    if (stack_ptr_tag != .heap) {
        switch (stack_ptr_dtype) {
            .string, .array, .hash_map => self.free(self.stack_ptr),
            else => {},
        }
    }

    try self.create(self.stack_ptr, dtype, data);
    memory_slice.items(.tag)[self.stack_ptr] = .stack;
    self.stack_ptr += 1;
}

pub fn stack_pop(self: *Memory) MemoryError!MemoryObject {
    if (self.stack_ptr == 0) {
        return Error.PoppingEmptyStack;
    }
    self.stack_ptr -= 1;
    return self.memory.get(self.stack_ptr);
}

pub fn stack_top(self: *Memory) ?u32 {
    if (self.stack_ptr == 0) {
        return null;
    }
    return self.stack_ptr;
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
        // instruction_ptr: u32,
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
        constant,
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

pub fn ObjectToString(obj: MemoryObject, buffer: []u8) ![]const u8 {
    switch (obj.dtype) {
        .integer => return std.fmt.bufPrint(buffer, "{d}", .{obj.data.integer}),
        .boolean => if (obj.data.boolean) {
            return std.fmt.bufPrint(buffer, "true", .{});
        } else {
            return std.fmt.bufPrint(buffer, "false", .{});
        },
        .string => {
            const data = obj.data.string_type;
            return std.fmt.bufPrint(buffer, "{s}", .{data.items});
        },
        .runtime_error => {
            const data = obj.data.runtime_error;
            return std.fmt.bufPrint(buffer, "{s}", .{data.ptr[0..data.len]});
        },
        .null => return std.fmt.bufPrint(buffer, "null", .{}),
        .function_expression => return std.fmt.bufPrint(buffer, "Function", .{}),
        .builtin => unreachable,
        .return_expression => unreachable,
        .break_statement => return std.fmt.bufPrint(buffer, "break", .{}),
        .continue_statement => return std.fmt.bufPrint(buffer, "continue", .{}),
        else => return std.fmt.bufPrint(buffer, "null", .{}),
    }
}

pub const StackFrame = extern struct {
    frame_len: u32,
    frame: [*]MemoryObject,
    // parent: FrameIndex,
};

test "Memory init" {
    // var timer = try std.time.Timer.start();
    var memory = try Memory.initCapacity(testing.allocator, 2048);
    defer memory.deinit();
    // const end = timer.read();
    // std.debug.print("Time to test: {s}\n", .{std.fmt.fmtDuration(end)});
    // end = timer.read();
    // try testing.expectEqual(memory.memory.items(.tag)[stack_limit - 1], .stack);
    try testing.expectEqual(memory.memory.items(.dtype)[stack_limit], .null);
    try testing.expectEqual(memory.memory.items(.dtype)[stack_limit + 1], .boolean);
    try testing.expectEqual(memory.memory.items(.dtype)[stack_limit + 2], .boolean);
    try testing.expectEqual(memory.memory.items(.dtype)[stack_limit + 3], .break_statement);
    try testing.expectEqual(memory.memory.items(.dtype)[stack_limit + 4], .continue_statement);
    try memory.stack_push(MemoryObject{
        .dtype = .integer,
        .data = .{ .integer = 15 },
        .refs = 0,
    });
    // end = timer.read() - end;
    // std.debug.print("Time to stackpush: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    try testing.expectEqual(memory.stack_ptr, 1);
    try testing.expectEqual(memory.memory.items(.dtype)[0], .integer);
    try testing.expectEqual(memory.memory.items(.data)[0].integer, 15);

    const value = try memory.stack_pop();
    try testing.expectEqual(value.dtype, .integer);
    try testing.expectEqual(value.data.integer, 15);

    const int_data: i64 = 10;
    // end = timer.read();

    const int_ptr = try memory.alloc(.integer, @ptrCast(&int_data));
    // end = timer.read() - end;
    try testing.expectEqual(memory.stack_ptr, 0);
    const int_ptr_data = memory.memory.get(int_ptr);
    try testing.expectEqual(int_ptr_data.tag, .heap);
    try testing.expectEqual(int_ptr_data.dtype, .integer);
    try testing.expectEqual(int_ptr_data.data.integer, int_data);
    try testing.expectEqual(int_ptr_data.refs, 0);
    // std.debug.print("Time to create int: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();

    const bool_data: bool = true;
    const bool_ptr = try memory.alloc(.boolean, @ptrCast(&bool_data));
    var bool_ptr_data = memory.memory.get(bool_ptr);
    try testing.expectEqual(bool_ptr, true_object);
    try testing.expectEqual(bool_ptr_data.tag, .reserved);
    try testing.expectEqual(bool_ptr_data.dtype, .boolean);
    try testing.expectEqual(bool_ptr_data.data.boolean, bool_data);
    try testing.expectEqual(bool_ptr_data.refs, 0);

    // end = timer.read() - end;
    // std.debug.print("Time to create bool: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    const string_data = try std.fmt.allocPrint(testing.allocator, "This is a test string: {d}", .{int_data});
    const string_ptr = try memory.alloc(.string, @ptrCast(&string_data));
    // end = timer.read() - end;
    // std.debug.print("Time to create string: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    const string_ptr_data = memory.memory.get(string_ptr);

    try testing.expectEqual(string_ptr_data.tag, .heap);
    try testing.expectEqual(string_ptr_data.dtype, .string);
    try testing.expectEqual(string_ptr_data.refs, 0);

    // freeing reserved object should not do anything
    memory.free(bool_ptr);
    bool_ptr_data = memory.memory.get(bool_ptr);
    // end = timer.read() - end;
    // std.debug.print("Time to free bool: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    try testing.expectEqual(bool_ptr, true_object);
    try testing.expectEqual(bool_ptr_data.tag, .reserved);
    try testing.expectEqual(bool_ptr_data.dtype, .boolean);
    try testing.expectEqual(bool_ptr_data.data.boolean, bool_data);
    try testing.expectEqual(bool_ptr_data.refs, 0);

    // Push a string onto the stack and create a new object
    const string_data2 = try std.fmt.allocPrint(testing.allocator, "This is a test string2: {d}", .{int_data});
    try memory.stack_push_create(.string, @ptrCast(&string_data2));
    // end = timer.read() - end;
    // std.debug.print("Time to push on stack: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    // The new stack variable should have string type and the data should be euqal
    try testing.expectEqual(memory.memory.items(.dtype)[0], .string);
    try testing.expectEqualSlices(u8, string_data2, memory.memory.items(.data)[0].string_type.items);

    const arraylist_ptr = memory.memory.items(.data)[0].string_type;

    // pop the top of the stack and register that as a global variable. Invalidates the stack data
    // But does nto copy the memory
    try memory.set_global(0);
    // end = timer.read() - end;
    // std.debug.print("Time to set global: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();

    // the 0th position in the stack should now be null and 0 value
    try testing.expectEqual(memory.memory.items(.dtype)[0], .null);
    try testing.expectEqual(memory.memory.items(.data)[0].integer, 0);

    // The global[0] should point to the first object after reserved object;
    try testing.expectEqual(memory.globals[0], memory.reserved_memory + 2);
    try testing.expectEqual(memory.memory.items(.dtype)[memory.globals[0]], .string);

    // The pointer of the array list should be the same as the one on the stack before
    try testing.expectEqual(memory.memory.items(.data)[memory.globals[0]].string_type, arraylist_ptr);
    try testing.expectEqualSlices(u8, string_data2, memory.memory.items(.data)[memory.globals[0]].string_type.items);

    // Push another string onto the stack
    const string_data3 = try std.fmt.allocPrint(testing.allocator, "This is a test string: {d}", .{int_data});
    try memory.stack_push_create(.string, @ptrCast(&string_data3));
    // end = timer.read() - end;
    // std.debug.print("Time to push on stack and create: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    try testing.expectEqual(memory.memory.items(.dtype)[0], .string);
    try testing.expectEqualSlices(u8, string_data, memory.memory.items(.data)[0].string_type.items);
    // This is a different array list
    try testing.expect(arraylist_ptr != memory.memory.items(.data)[0].string_type);

    const arraylist_ptr2 = memory.memory.items(.data)[0].string_type;

    // Pop this string off the stack
    const string_stack_obj = try memory.stack_pop();
    // end = timer.read() - end;
    // std.debug.print("Time to pop off stack: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    // The old top of the stack is still the same as pop does not nullify data
    try testing.expectEqual(memory.memory.items(.dtype)[0], .string);
    try testing.expectEqualSlices(u8, string_data, memory.memory.items(.data)[0].string_type.items);

    // The retrived string should be the same as the top of the stack
    try testing.expectEqual(string_stack_obj.dtype, .string);
    try testing.expectEqualSlices(u8, string_data, string_stack_obj.data.string_type.items);
    // This should be the same as the new object
    try testing.expectEqual(arraylist_ptr2, string_stack_obj.data.string_type);

    // Push a new integer object onto the stack
    // This should free the string memory unless we increased the reference by storing it somewhere else.

    try memory.stack_push(int_ptr_data);
    // end = timer.read() - end;
    // std.debug.print("Time to push on stack with strin overwrite: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // end = timer.read();
    try testing.expectEqual(memory.memory.items(.dtype)[0], .integer);
    try testing.expectEqual(10, memory.memory.items(.data)[0].integer);
    // end = timer.read();
    // std.debug.print("Time to test: {s}\n", .{std.fmt.fmtDuration(end)});
    // std.debug.print("\n", .{});
    // std.debug.print("\n", .{});
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
