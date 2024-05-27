pub const Error = error{ NonStringifibaleObject, InactiveField } || Allocator.Error;

pub const ObjectPool = @This();

object_pool: std.MultiArrayList(InternalObject),
free_list: std.ArrayListUnmanaged(ObjectIndex),

pub const ObjectIndex = u32;

pub const null_object: ObjectIndex = 0;
pub const true_object: ObjectIndex = 1;
pub const false_object: ObjectIndex = 2;

pub fn Create(allocator: Allocator) !ObjectPool {
    return CreateCapacity(allocator, 0);
}

pub fn CreateCapacity(allocator: Allocator, capacity: u32) !ObjectPool {
    const internal_capacity = capacity + 3;
    var pool = ObjectPool{
        .object_pool = .{},
        .free_list = .{},
    };
    try pool.object_pool.ensureUnusedCapacity(allocator, internal_capacity);
    try pool.free_list.ensureUnusedCapacity(allocator, internal_capacity);
    for (0..internal_capacity) |i| {
        try pool.free_list.append(allocator, @as(ObjectIndex, @intCast(internal_capacity - 1 - i)));
        try pool.object_pool.append(allocator, InternalObject{ .tag = .null, .data = undefined });
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

pub fn create_object(self: *ObjectPool, tag: ObjectTypes, data: *const anyopaque) !ObjectIndex {
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

    if (self.free_list.items.len == 0) {} else {
        const free_loc = self.free_list.popOrNull() orelse unreachable;
        // var obj: InternalObject.ObjectData = undefined;
        switch (tag) {
            .integer => {
                const value: *const i64 = @ptrCast(@alignCast(data));
                self.object_pool.items(.tag)[free_loc] = .integer;
                self.object_pool.items(.data)[free_loc].integer = value.*;
            },
            .string => {},
            .return_expression => {},
            .function_expression => {},
            .runtime_error => {},
            .null => unreachable,
            .boolean => unreachable,
        }
        return free_loc;
    }
    return 0;
}

pub fn get(self: *ObjectPool, position: ObjectIndex) InternalObject {
    return self.object_pool.get(position);
}

pub const InternalObject = struct {
    tag: ObjectTypes,
    data: ObjectData,

    pub const ObjectData = extern union {
        integer: i64,
        boolean: bool,
        return_value: ObjectIndex,
        function_expression: FunctionExpression,
        runtime_error: StringType,
        string_type: StringType,
    };

    pub const FunctionExpression = extern struct {
        block_statements: [*]Ast.Node.NodeIndex,
        block_len: u32,
        parameters: [*]Ast.Node.NodeIndex,
        parameter_len: u32,
        env: *Environment,
    };

    pub const StringType = extern struct {
        string: [*]const u8,
        length: u32,
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

pub const ObjectStructures = struct {
    pub const IntegerType = struct {
        value: i64,
    };

    pub const BooleanType = struct {
        value: bool,
    };

    pub const ReturnType = struct {
        value: Object,
    };

    pub const FunctionExpressionType = struct {
        value: FunctionValueType,
    };

    pub const RuntimeErrorType = struct {
        value: []const u8,
    };

    pub const StringType = struct {
        value: []u8,
    };

    pub const FunctionValueType = struct {
        block_statements: []Ast.Node.NodeIndex,
        parameters: []Ast.Node.NodeIndex,
        env: *Environment,
    };

    pub const StringStorage = struct {
        string: []u8,
    };

    // pub const ReturnTypeUnion = union(enum) {
    //     int: i64,
    //     bool: bool,
    //     err: RuntimeErrorType,
    //     func: FunctionExpressionType,
    // };
};

pub const Object = union(ObjectTypes) {
    integer: *ObjectStructures.IntegerType,
    boolean: *ObjectStructures.BooleanType,
    string: *ObjectStructures.StringType,
    return_expression: *ObjectStructures.ReturnType,
    function_expression: *ObjectStructures.FunctionExpressionType,
    runtime_error: *ObjectStructures.RuntimeErrorType,
    null,

    const Tag = @typeInfo(Object).Union.tag_type.?;

    fn fieldType(comptime kind: Tag) type {
        return std.meta.fields(Object)[@intFromEnum(kind)].type;
    }

    pub fn get(self: Object, comptime kind: Tag) Error!fieldType(kind) {
        std.debug.print("", .{});
        switch (self) {
            kind => |v| return v,
            else => return Error.InactiveField,
        }
    }

    pub fn getEnumTagAsString(self: Object) []const u8 {
        return switch (self) {
            .integer => "INTEGER",
            .boolean => "BOOLEAN",
            .string => "STRING",
            .return_expression => "RETURN EXPRESSION",
            .function_expression => "FUNCTION EXPRESSION",
            .runtime_error => "RUNTIME ERROR",
            .null => "NULL",
        };
    }

    pub fn getEnumTag(self: Object) ObjectTypes {
        return switch (self) {
            .integer => .integer,
            .boolean => .boolean,
            .string => .string,
            .return_expression => .return_expression,
            .function_expression => .function_expression,
            .runtime_error => .runtime_error,
            .null => .null,
        };
    }

    pub fn copy(self: Object, allocator: Allocator) Allocator.Error!Object {
        switch (self) {
            .integer => |i| {
                const obj = try allocator.create(ObjectStructures.IntegerType);
                obj.value = i.value;
                return Object{ .integer = obj };
            },
            .boolean => |b| {
                const obj = try allocator.create(ObjectStructures.BooleanType);
                obj.value = b.value;
                return Object{ .boolean = obj };
            },
            .string => |s| {
                var obj = try allocator.create(ObjectStructures.StringType);
                obj.value = try allocator.alloc(u8, s.value.len);
                @memcpy(obj.value, s.value);
                return Object{ .string = obj };
            },
            .return_expression => |r| {
                const obj = try allocator.create(ObjectStructures.ReturnType);
                obj.value = r.value;
                return Object{ .return_expression = obj };
            },
            .function_expression => |f| {
                var obj = try allocator.create(ObjectStructures.FunctionExpressionType);
                obj.value = f.value;
                obj.value.parameters = try allocator.alloc(u32, f.value.parameters.len);
                obj.value.block_statements = try allocator.alloc(u32, f.value.block_statements.len);
                @memcpy(obj.value.parameters, f.value.parameters);
                @memcpy(obj.value.block_statements, f.value.block_statements);
                obj.value.env = f.value.env;
                return Object{ .function_expression = obj };
            },
            .runtime_error => |err| {
                const obj = try allocator.create(ObjectStructures.RuntimeErrorType);
                obj.value = err.value;
                return Object{ .runtime_error = obj };
            },
            .null => {
                return .null;
            },
        }
    }

    pub fn Create(tag: ObjectTypes, allocator: Allocator, data: *const anyopaque) Error!Object {
        switch (tag) {
            .integer => {
                const obj = try allocator.create(ObjectStructures.IntegerType);
                const value: *const i64 = @ptrCast(@alignCast(data));
                obj.value = value.*;
                return Object{ .integer = obj };
            },
            .boolean => {
                const obj = try allocator.create(ObjectStructures.BooleanType);
                const value: *const bool = @ptrCast(@alignCast(data));
                obj.value = value.*;
                return Object{ .boolean = obj };
            },
            .string => {
                const value: *const ObjectStructures.StringStorage = @ptrCast(@alignCast(data));
                var obj = try allocator.create(ObjectStructures.StringType);
                // obj.value = try allocator.alloc(u8, value.string.len);
                // @memcpy(obj.value, value.string);
                obj.value = value.string;
                return Object{ .string = obj };
            },
            .return_expression => {
                const obj = try allocator.create(ObjectStructures.ReturnType);
                const value: *const Object = @ptrCast(@alignCast(data));
                obj.value = value.*;
                return Object{ .return_expression = obj };
            },
            .function_expression => {
                var obj = try allocator.create(ObjectStructures.FunctionExpressionType);
                const value: *const ObjectStructures.FunctionValueType = @ptrCast(@alignCast(data));
                obj.value.parameters = value.parameters;
                obj.value.block_statements = value.block_statements;
                // obj.value.parameters = try allocator.alloc(u32, value.parameters.len);
                // obj.value.block_statements = try allocator.alloc(u32, value.block_statements.len);
                // @memcpy(obj.value.parameters, value.parameters);
                // @memcpy(obj.value.block_statements, value.block_statements);
                // allocator.free(value.parameters);
                // allocator.free(value.block_statements);
                obj.value.env = value.env;
                return Object{ .function_expression = obj };
            },
            .runtime_error => {
                const value: *const ObjectStructures.StringStorage = @ptrCast(@alignCast(data));
                const obj = try allocator.create(ObjectStructures.RuntimeErrorType);
                obj.value = value.string;
                return Object{ .runtime_error = obj };
            },
            .null => {
                return .null;
            },
        }
    }

    pub fn deinit(self: *const Object, allocator: Allocator) void {
        switch (self.*) {
            .integer => |i| allocator.destroy(i),
            .boolean => |b| allocator.destroy(b),
            .string => |s| {
                allocator.free(s.value);
                allocator.destroy(s);
            },
            .return_expression => |r| allocator.destroy(r),
            .function_expression => |f| {
                allocator.free(f.value.block_statements);
                allocator.free(f.value.parameters);
                allocator.destroy(f);
            },
            .runtime_error => |r| {
                allocator.free(r.value);
                allocator.destroy(r);
            },
            .null => return,
        }
    }

    pub fn ToString(self: Object, buffer: []u8) ![]const u8 {
        switch (self) {
            .integer => |i| return std.fmt.bufPrint(buffer, "{d}", .{i.value}),
            .boolean => |b| if (b.value) {
                return std.fmt.bufPrint(buffer, "true", .{});
            } else {
                return std.fmt.bufPrint(buffer, "false", .{});
            },
            .string => |s| return s.value,
            .runtime_error => |e| return e.value,
            .null => return std.fmt.bufPrint(buffer, "null", .{}),
            .function_expression => return std.fmt.bufPrint(buffer, "Function", .{}),
            inline else => return Error.NonStringifibaleObject,
        }
    }
};

test "evaluator_object_test_obj_get" {
    const int_val: i64 = 0;
    var buffer: [1024]u8 = undefined;
    var int_obj = try Object.Create(.integer, testing.allocator, @ptrCast(&int_val));
    defer int_obj.deinit(testing.allocator);
    const val = try int_obj.get(.integer);
    try testing.expectError(error.InactiveField, int_obj.get(.boolean));
    val.value = 41;

    switch (int_obj) {
        .integer => |i| i.value += 1,
        inline else => unreachable,
    }
    try testing.expect(int_obj.integer.value == 42);

    const int_str = try int_obj.ToString(&buffer);

    try testing.expectEqualSlices(u8, "42", int_str);
}

test "evaluator_object_test" {
    const env = try Environment.Create(testing.allocator);
    defer env.deinit(testing.allocator);
    const int_val: i64 = 41;
    var buffer: [1024]u8 = undefined;
    var int_obj = try Object.Create(.integer, testing.allocator, @ptrCast(&int_val));
    defer int_obj.deinit(testing.allocator);
    // int_obj.integer.value = 41;

    switch (int_obj) {
        .integer => |i| i.value += 1,
        inline else => unreachable,
    }
    try testing.expect(int_obj.integer.value == 42);

    const int_str = try int_obj.ToString(&buffer);

    try testing.expectEqualSlices(u8, "42", int_str);

    const bool_value = false;
    var bool_obj = try Object.Create(.boolean, testing.allocator, @ptrCast(&bool_value));
    defer bool_obj.deinit(testing.allocator);
    bool_obj.boolean.value = false;

    switch (bool_obj) {
        .boolean => |i| i.value = true,
        inline else => unreachable,
    }
    try testing.expect(bool_obj.boolean.value);

    const bool_string = try bool_obj.ToString(&buffer);
    try testing.expectEqualSlices(u8, "true", bool_string);

    // const func_data: ObjectStructures.FunctionValueType = .{
    //     .block_statements = .{
    //         .start = 1,
    //         .end = 2,
    //     },
    //     .parameters = .{
    //         .start = 1,
    //         .end = 2,
    //     },
    //     .env = env,
    // };
    // var func_obj = try Object.Create(.function_expression, testing.allocator, @ptrCast(&func_data));
    // defer func_obj.deinit(testing.allocator);
    //
    // try testing.expectEqualDeep(
    //     Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
    //     func_obj.function_expression.value.parameters,
    // );
    // try testing.expectEqualDeep(
    //     Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
    //     func_obj.function_expression.value.block_statements,
    // );
    // var function_parameters: Ast.Node.ExtraDataRange = undefined;
    // var function_block: Ast.Node.ExtraDataRange = undefined;
    // switch (func_obj) {
    //     .function_expression => |f| {
    //         function_parameters = f.value.parameters;
    //         function_block = f.value.block_statements;
    //         try f.value.env.create_variable(testing.allocator, "var", int_obj, .constant);
    //     },
    //     inline else => unreachable,
    // }
    // try testing.expectEqualDeep(function_parameters, func_obj.function_expression.value.parameters);
    // try testing.expectEqualDeep(function_block, func_obj.function_expression.value.block_statements);
    // const obj = try func_obj.function_expression.value.env.get_object("var", testing.allocator);
    // defer obj.deinit(testing.allocator);
    // try testing.expect(obj.integer.value == 42);
    //
    // // const return_data = ObjectStructures.ReturnTypeUnion{ .func = func_obj.function_expression.* };
    // const return_obj = try Object.Create(.return_expression, testing.allocator, @ptrCast(&func_obj));
    // defer return_obj.deinit(testing.allocator);
    //
    // try testing.expectEqualDeep(
    //     Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
    //     return_obj.return_expression.value.function_expression.value.parameters,
    // );
    // try testing.expectEqualDeep(
    //     Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
    //     return_obj.return_expression.value.function_expression.value.block_statements,
    // );

    const err_msg = try std.fmt.allocPrint(testing.allocator, "This is an error: {d}", .{42});
    var err_obj = try Object.Create(.runtime_error, testing.allocator, @ptrCast(&err_msg));
    defer err_obj.deinit(testing.allocator);

    try testing.expectEqualSlices(u8, "This is an error: 42", err_obj.runtime_error.value);

    const null_obj: Object = .null;
    defer null_obj.deinit(testing.allocator);
    var identified = false;
    switch (null_obj) {
        .null => identified = true,
        inline else => unreachable,
    }
    try testing.expect(identified);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const token = @import("token.zig");
const Ast = @import("ast.zig");
const Parser = @import("parser.zig");
const Environment = @import("environment.zig");
