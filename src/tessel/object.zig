pub const Error = error{ NonStringifibaleObject, InactiveField } || Allocator.Error;

pub const ObjectTypes = enum {
    integer,
    boolean,
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
        value: ReturnTypeUnion,
    };

    pub const FunctionExpressionType = struct {
        value: FunctionValueType,
    };

    pub const RuntimeErrorType = struct {
        value: []const u8,
    };

    pub const FunctionValueType = struct {
        block_statements: Ast.Node.ExtraDataRange,
        parameters: Ast.Node.ExtraDataRange,
    };

    pub const ReturnTypeUnion = union(enum) {
        int: i64,
        bool: bool,
        err: RuntimeErrorType,
        func: FunctionExpressionType,
    };
};

pub const Object = union(ObjectTypes) {
    integer: *ObjectStructures.IntegerType,
    boolean: *ObjectStructures.BooleanType,
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
            .return_expression => {
                const obj = try allocator.create(ObjectStructures.ReturnType);
                const value: *const ObjectStructures.ReturnTypeUnion = @ptrCast(@alignCast(data));
                obj.value = value.*;
                return Object{ .return_expression = obj };
            },
            .function_expression => {
                const obj = try allocator.create(ObjectStructures.FunctionExpressionType);
                const value: *const ObjectStructures.FunctionValueType = @ptrCast(@alignCast(data));
                obj.value = value.*;
                return Object{ .function_expression = obj };
            },
            .runtime_error => {
                const obj = try allocator.create(ObjectStructures.RuntimeErrorType);
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
            .return_expression => |r| allocator.destroy(r),
            .function_expression => |f| allocator.destroy(f),
            .runtime_error => |r| allocator.destroy(r),
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
            .null => return std.fmt.bufPrint(buffer, "null", .{}),
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

    const func_data: ObjectStructures.FunctionValueType = .{
        .block_statements = .{
            .start = 1,
            .end = 2,
        },
        .parameters = .{
            .start = 1,
            .end = 2,
        },
    };
    var func_obj = try Object.Create(.function_expression, testing.allocator, @ptrCast(&func_data));
    defer func_obj.deinit(testing.allocator);

    try testing.expectEqualDeep(
        Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
        func_obj.function_expression.value.parameters,
    );
    try testing.expectEqualDeep(
        Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
        func_obj.function_expression.value.block_statements,
    );
    var function_parameters: Ast.Node.ExtraDataRange = undefined;
    var function_block: Ast.Node.ExtraDataRange = undefined;
    switch (func_obj) {
        .function_expression => |f| {
            function_parameters = f.value.parameters;
            function_block = f.value.block_statements;
        },
        inline else => unreachable,
    }
    try testing.expectEqualDeep(function_parameters, func_obj.function_expression.value.parameters);
    try testing.expectEqualDeep(function_block, func_obj.function_expression.value.block_statements);

    const return_data = ObjectStructures.ReturnTypeUnion{ .func = func_obj.function_expression.* };
    const return_obj = try Object.Create(.return_expression, testing.allocator, @ptrCast(&return_data));
    defer return_obj.deinit(testing.allocator);

    try testing.expectEqualDeep(
        Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
        return_obj.return_expression.value.func.value.parameters,
    );
    try testing.expectEqualDeep(
        Ast.Node.ExtraDataRange{ .start = 1, .end = 2 },
        return_obj.return_expression.value.func.value.block_statements,
    );

    var err_msg: []const u8 = undefined;
    var err_obj = try Object.Create(.runtime_error, testing.allocator, "");
    err_obj.runtime_error.value = "This is an error";
    defer err_obj.deinit(testing.allocator);

    switch (err_obj) {
        .runtime_error => |e| {
            err_msg = e.value;
        },
        inline else => unreachable,
    }
    try testing.expectEqualSlices(u8, err_msg, err_obj.runtime_error.value);

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
