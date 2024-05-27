pub const Error = error{ NonStringifibaleObject, InactiveField } || Allocator.Error;

pub const ObjectPool = @This();

object_pool: std.MultiArrayList(InternalObject),
free_list: std.ArrayListUnmanaged(ObjectIndex),

pub const ObjectIndex = u32;

pub const null_object: ObjectIndex = 0;
pub const true_object: ObjectIndex = 1;
pub const false_object: ObjectIndex = 2;

pub fn init(allocator: Allocator) !ObjectPool {
    return initCapacity(allocator, 0);
}

pub fn initCapacity(allocator: Allocator, capacity: u32) !ObjectPool {
    const internal_capacity = capacity + 3;
    var pool = ObjectPool{
        .object_pool = .{},
        .free_list = .{},
    };
    try pool.object_pool.ensureUnusedCapacity(allocator, internal_capacity);
    try pool.free_list.ensureUnusedCapacity(allocator, internal_capacity);
    for (0..internal_capacity) |i| {
        try pool.free_list.append(allocator, @as(ObjectIndex, @intCast(internal_capacity - 1 - i)));
        try pool.object_pool.append(
            allocator,
            InternalObject{
                .tag = .null,
                .data = .{ .integer = @as(i64, @intCast(0)) },
                .refs = 0,
            },
        );
    }

    // 0 will always be a null node and will be referenced when .null is needed
    _ = pool.free_list.popOrNull() orelse unreachable;
    // 1 will be the true node and again will be referenced
    const true_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.object_pool.items(.tag)[true_loc] = .boolean;
    pool.object_pool.items(.data)[true_loc].boolean = true;
    // 2 will be the false node and again will be referenced
    const false_loc = pool.free_list.popOrNull() orelse unreachable;
    pool.object_pool.items(.tag)[false_loc] = .boolean;
    pool.object_pool.items(.data)[false_loc].boolean = false;
    return pool;
}

pub fn deinit(self: *ObjectPool, allocator: Allocator) void {
    for (0..self.object_pool.len) |i| {
        _ = self.free_possible_memory(allocator, @as(u32, @intCast(i)));
    }
    self.object_pool.deinit(allocator);
    self.free_list.deinit(allocator);
}

pub fn create(self: *ObjectPool, allocator: Allocator, tag: ObjectTypes, data: *const anyopaque) !ObjectIndex {
    if (tag == .null) {
        return null_object;
    }

    if (tag == .boolean) {
        const value: *const bool = @ptrCast(@alignCast(data));
        if (value.*) {
            return true_object;
        } else {
            return false_object;
        }
    }

    var location: ObjectIndex = 0;
    if (self.free_list.items.len == 0) {
        location = @as(ObjectIndex, @intCast(try self.object_pool.addOne(allocator)));
    } else {
        location = self.free_list.popOrNull() orelse unreachable;
    }

    switch (tag) {
        .integer => {
            const value: *const i64 = @ptrCast(@alignCast(data));
            self.object_pool.items(.tag)[location] = .integer;
            self.object_pool.items(.data)[location].integer = value.*;
        },
        .return_expression => {
            const value: *const ObjectIndex = @ptrCast(@alignCast(data));
            self.object_pool.items(.tag)[location] = .return_expression;
            self.object_pool.items(.data)[location].return_value = value.*;
        },
        .function_expression => {
            const value: *const InternalObject.FunctionData = @ptrCast(@alignCast(data));
            self.object_pool.items(.tag)[location] = .function_expression;
            // const block = try allocator.alloc(Ast.Node.NodeIndex, value.block.len);
            // const parameter = try allocator.alloc(Ast.Node.NodeIndex, value.parameters.len);
            self.object_pool.items(.data)[location].function.block_ptr = value.block.ptr;
            self.object_pool.items(.data)[location].function.block_len = @as(u32, @intCast(value.block.len));
            self.object_pool.items(.data)[location].function.parameters_ptr = value.parameters.ptr;
            self.object_pool.items(.data)[location].function.parameters_len = @as(u32, @intCast(value.parameters.len));
            self.object_pool.items(.data)[location].function.env = value.env;
            // @memcpy(self.object_pool.items(.data)[location].function.block_ptr, value.block);
            // @memcpy(self.object_pool.items(.data)[location].function.parameters_ptr, value.parameters);
        },
        .runtime_error => {
            const value: *const InternalObject.StringType = @ptrCast(@alignCast(data));
            self.object_pool.items(.tag)[location] = .runtime_error;
            // const string_ptr = try allocator.alloc(u8, value.len);
            // self.object_pool.items(.data)[location].string_type.ptr = string_ptr.ptr;
            self.object_pool.items(.data)[location].string_type.ptr = value.ptr;
            self.object_pool.items(.data)[location].string_type.len = value.len;
            // @memcpy(self.object_pool.items(.data)[location].string_type.ptr, value.ptr[0..value.len]);
        },
        .string => {
            const value: *const InternalObject.StringType = @ptrCast(@alignCast(data));
            self.object_pool.items(.tag)[location] = .string;
            // const string_ptr = try allocator.alloc(u8, value.len);
            // self.object_pool.items(.data)[location].string_type.ptr = string_ptr.ptr;
            self.object_pool.items(.data)[location].string_type.ptr = value.ptr;
            self.object_pool.items(.data)[location].string_type.len = value.len;
            // @memcpy(self.object_pool.items(.data)[location].string_type.ptr, value.ptr[0..value.len]);
        },
        .null => unreachable,
        .boolean => unreachable,
    }
    self.object_pool.items(.refs)[location] = 0;
    try self.free_list.ensureTotalCapacity(allocator, self.object_pool.len);
    return location;
}

pub fn free(self: *ObjectPool, allocator: Allocator, position: ObjectIndex) void {
    std.debug.assert(position <= self.object_pool.len);
    if (position < 3) {
        return;
    }
    if (self.object_pool.items(.refs)[position] == 0) {
        _ = self.free_possible_memory(allocator, position);
        self.object_pool.items(.tag)[position] = .null;
        self.object_pool.items(.data)[position].integer = 0;
        self.free_list.appendAssumeCapacity(position);
    } else {
        self.object_pool.items(.refs)[position] -= 1;
    }
}

fn free_possible_memory(self: *ObjectPool, allocator: Allocator, position: ObjectIndex) bool {
    const tag = self.object_pool.items(.tag)[position];
    switch (tag) {
        .integer => {},
        .return_expression => {},
        .function_expression => {
            allocator.free(
                self.object_pool.items(.data)[position].function.block_ptr[0..self.object_pool.items(.data)[position].function.block_len],
            );
            allocator.free(
                self.object_pool.items(.data)[position].function.parameters_ptr[0..self.object_pool.items(.data)[position].function.parameters_len],
            );
            self.object_pool.items(.data)[position].function.block_len = 0;
            self.object_pool.items(.data)[position].function.parameters_len = 0;
        },
        .runtime_error => {
            allocator.free(
                self.object_pool.items(.data)[position].string_type.ptr[0..self.object_pool.items(.data)[position].string_type.len],
            );
            self.object_pool.items(.data)[position].string_type.len = 0;
        },
        .string => {
            allocator.free(
                self.object_pool.items(.data)[position].string_type.ptr[0..self.object_pool.items(.data)[position].string_type.len],
            );
            self.object_pool.items(.data)[position].string_type.len = 0;
        },
        .null => return false,
        .boolean => return false,
    }
    return true;
}

pub fn get(self: *ObjectPool, position: ObjectIndex) InternalObject {
    std.debug.assert(position <= self.object_pool.len);
    return self.object_pool.get(position);
}

pub fn get_tag(self: *ObjectPool, position: ObjectIndex) ObjectTypes {
    std.debug.assert(position <= self.object_pool.len);
    return self.object_pool.items(.tag)[position];
}

pub fn get_data(self: *ObjectPool, position: ObjectIndex) InternalObject.ObjectData {
    std.debug.assert(position <= self.object_pool.len);
    return self.object_pool.items(.data)[position];
}

pub fn increase_ref(self: *ObjectPool, position: ObjectIndex) void {
    std.debug.assert(position <= self.object_pool.len);
    if (position < 3) {
        return;
    }

    self.object_pool.items(.refs)[position] += 1;
}

pub fn ToString(self: *ObjectPool, buffer: []u8, position: ObjectIndex) ![]const u8 {
    switch (self.get_tag(position)) {
        .integer => return std.fmt.bufPrint(buffer, "{d}", .{self.get_data(position).integer}),
        .boolean => if (position == 1) {
            return std.fmt.bufPrint(buffer, "true", .{});
        } else {
            return std.fmt.bufPrint(buffer, "false", .{});
        },
        .string => {
            const data = self.get_data(position).string_type;
            return std.fmt.bufPrint(buffer, "{s}", .{data.ptr[0..data.len]});
        },
        .runtime_error => {
            const data = self.get_data(position).runtime_error;
            return std.fmt.bufPrint(buffer, "{s}", .{data.ptr[0..data.len]});
        },
        .null => return std.fmt.bufPrint(buffer, "null", .{}),
        .function_expression => return std.fmt.bufPrint(buffer, "Function", .{}),
        .return_expression => unreachable,
    }
}

pub fn get_tag_string(self: *ObjectPool, position: ObjectIndex) []const u8 {
    std.debug.assert(position <= self.object_pool.len);
    const tag = self.object_pool.items(.tag)[position];
    return switch (tag) {
        .integer => "INTEGER",
        .boolean => "BOOLEAN",
        .string => "STRING",
        .return_expression => "RETURN EXPRESSION",
        .function_expression => "FUNCTION EXPRESSION",
        .runtime_error => "RUNTIME ERROR",
        .null => "NULL",
    };
}
pub const InternalObject = struct {
    tag: ObjectTypes,
    data: ObjectData,
    refs: u32,

    pub const ObjectData = extern union {
        integer: i64,
        boolean: bool,
        return_value: ObjectIndex,
        function: FunctionExpression,
        runtime_error: StringType,
        string_type: StringType,
    };

    const FunctionExpression = extern struct {
        env: *Environment,
        block_ptr: [*]Ast.Node.NodeIndex,
        block_len: u32,
        parameters_ptr: [*]Ast.Node.NodeIndex,
        parameters_len: u32,
    };

    pub const FunctionData = struct {
        block: []Ast.Node.NodeIndex,
        parameters: []Ast.Node.NodeIndex,
        env: *Environment,
    };

    pub const StringType = extern struct {
        ptr: [*]u8,
        len: u32,
    };
};

pub const ObjectTypes = enum {
    integer,
    boolean,
    string,
    return_expression,
    function_expression,
    runtime_error,
    null,
};

test "test_object" {
    const allocator = testing.allocator;
    var pool = try ObjectPool.init(allocator);
    defer pool.deinit(allocator);

    try testing.expectEqual(0, pool.free_list.items.len);
    try testing.expectEqual(.null, pool.get_tag(0));
    try testing.expectEqual(false, pool.get_data(0).boolean);
    try testing.expectEqual(.boolean, pool.get_tag(1));
    try testing.expectEqual(true, pool.get_data(1).boolean);
    try testing.expectEqual(.boolean, pool.get_tag(2));
    try testing.expectEqual(false, pool.get_data(2).boolean);

    const integer: i64 = 32;
    const position = try pool.create(allocator, .integer, @ptrCast(&integer));
    try testing.expectEqual(3, position);
    try testing.expectEqual(.integer, pool.get_tag(position));
    try testing.expectEqual(32, pool.get_data(position).integer);
    try testing.expectEqual(0, pool.free_list.items.len);

    const boolean: bool = true;
    const bool_pos = try pool.create(allocator, .boolean, @ptrCast(&boolean));
    try testing.expectEqual(1, bool_pos);
    try testing.expectEqual(.boolean, pool.get_tag(bool_pos));
    try testing.expectEqual(true, pool.get_data(bool_pos).boolean);
    try testing.expectEqual(0, pool.free_list.items.len);

    const buf = try std.fmt.allocPrint(allocator, "This is a sample string {d}\n", .{integer});
    const str_pos = try pool.create(allocator, .string, @ptrCast(&buf));
    try testing.expectEqual(4, str_pos);
    try testing.expectEqual(.string, pool.get_tag(str_pos));
    try testing.expectEqual(27, pool.get_data(str_pos).string_type.len);
    try testing.expectEqual(0, pool.free_list.items.len);
    const str = pool.get(str_pos).data.string_type;
    try testing.expectEqualSlices(u8, buf, str.ptr[0..str.len]);
    try testing.expectEqual(5, pool.object_pool.len);
    pool.free(allocator, str_pos);
    try testing.expectEqual(1, pool.free_list.items.len);
    try testing.expectEqualSlices(u32, &[_]u32{4}, pool.free_list.items);
    try testing.expectEqual(.null, pool.get_tag(str_pos));

    const err_buf = try std.fmt.allocPrint(allocator, "This is a sample Error {d}\n", .{integer});
    const err_pos = try pool.create(allocator, .runtime_error, @ptrCast(&err_buf));
    try testing.expectEqual(4, err_pos);
    try testing.expectEqual(.runtime_error, pool.get_tag(err_pos));
    try testing.expectEqual(26, pool.get_data(err_pos).runtime_error.len);
    try testing.expectEqual(0, pool.free_list.items.len);
    const err = pool.get(err_pos).data.runtime_error;
    try testing.expectEqualSlices(u8, err_buf, err.ptr[0..err.len]);
    try testing.expectEqual(5, pool.object_pool.len);
    pool.free(allocator, err_pos);
    try testing.expectEqual(1, pool.free_list.items.len);
    try testing.expectEqualSlices(u32, &[_]u32{4}, pool.free_list.items);
    try testing.expectEqual(.null, pool.get_tag(err_pos));

    const return_expr = try pool.create(allocator, .return_expression, @ptrCast(&position));
    try testing.expectEqual(4, return_expr);
    try testing.expectEqual(.return_expression, pool.get_tag(return_expr));
    try testing.expectEqual(3, pool.get_data(return_expr).return_value);
    try testing.expectEqual(0, pool.free_list.items.len);
    try testing.expectEqual(5, pool.object_pool.len);
    pool.free(allocator, return_expr);
    try testing.expectEqual(1, pool.free_list.items.len);
    try testing.expectEqualSlices(u32, &[_]u32{4}, pool.free_list.items);
    try testing.expectEqual(.null, pool.get_tag(return_expr));

    var blocks = std.ArrayList(u32).init(allocator);
    try blocks.appendSlice(&[_]u32{ 1, 2, 3, 5 });
    var params = std.ArrayList(u32).init(allocator);
    try params.appendSlice(&[_]u32{ 1, 2 });
    const env = try Environment.Create(allocator);
    defer env.deinit(allocator);
    const func_data = InternalObject.FunctionData{
        .block = try blocks.toOwnedSlice(),
        .parameters = try params.toOwnedSlice(),
        .env = env,
    };
    const func_loc = try pool.create(allocator, .function_expression, @ptrCast(&func_data));

    try testing.expectEqual(4, func_loc);
    try testing.expectEqual(.function_expression, pool.get_tag(return_expr));
    const fdata = pool.get_data(func_loc).function;
    try testing.expectEqual(4, fdata.block_len);
    try testing.expectEqual(2, fdata.parameters_len);
    try testing.expectEqualSlices(u32, func_data.block, fdata.block_ptr[0..fdata.block_len]);
    try testing.expectEqualSlices(u32, func_data.parameters, fdata.parameters_ptr[0..fdata.parameters_len]);
    try testing.expectEqual(0, pool.free_list.items.len);
    try testing.expectEqual(5, pool.object_pool.len);
    pool.free(allocator, func_loc);
    try testing.expectEqual(1, pool.free_list.items.len);
    try testing.expectEqualSlices(u32, &[_]u32{4}, pool.free_list.items);
    try testing.expectEqual(.null, pool.get_tag(func_loc));
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const token = @import("token.zig");
const Ast = @import("ast.zig");
const Parser = @import("parser.zig");
const Environment = @import("environment.zig");
