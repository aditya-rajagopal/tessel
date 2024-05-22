pub const Evaluator = @This();

pub const Error = error{ ReferencingNodeZero, NonStringifibaleObject } || Allocator.Error;

fn eval(ast: *Ast, node: Ast.Node.NodeIndex, allocator: Allocator) !Object {
    if (node >= ast.nodes.len) {
        return .null;
    }

    if (node == 0) {
        return Error.ReferencingNodeZero;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .INTEGER_LITERAL => {
            const obj = try Object.Create(.integer, allocator, @ptrCast(&ast.integer_literals[ast_node.node_data.lhs]));
            return obj;
        },
        .BOOLEAN_LITERAL => {
            const value = ast_node.node_data.lhs == 1;
            const obj = try Object.Create(.boolean, allocator, @ptrCast(&value));
            return obj;
        },
        .EXPRESSION_STATEMENT => {
            return eval(ast, ast_node.node_data.lhs, allocator);
        },
        else => return .null,
    }
}

pub fn evaluate_program(ast: *Ast, allocator: Allocator) Error!Object {
    return eval(ast, 1, allocator);
}

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

    pub fn ToString(self: Object, allocator: Allocator) Error![]const u8 {
        switch (self) {
            .integer => |i| return std.fmt.allocPrint(allocator, "{d}", .{i.value}),
            .boolean => |b| if (b.value) {
                return "true";
            } else {
                return "false";
            },
            inline else => return Error.NonStringifibaleObject,
        }
    }
};

pub fn convert_ast_to_string(ast: *Ast, root_node: usize, list: *std.ArrayList(u8)) !void {
    if (root_node >= ast.nodes.len) {
        return;
    }

    if (root_node == 0) {
        return Error.ReferencingNodeZero;
    }

    const node = ast.nodes.get(root_node);
    switch (node.tag) {
        .MULTIPLY, //
        .ADDITION,
        .SUBTRACTION,
        .DIVIDE,
        .LESS_THAN,
        .GREATER_THAN,
        => {
            try list.append('(');
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.append(' ');
            try list.appendSlice(Ast.Node.Tag.get_operator_string(node.tag));
            try list.append(' ');
            try convert_ast_to_string(ast, node.node_data.rhs, list);
            try list.append(')');
        },

        .GREATER_THAN_EQUAL,
        .LESS_THAN_EQUAL,
        .DOUBLE_EQUAL,
        .NOT_EQUAL,
        => {
            try list.append('(');
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.append(' ');
            const op_str = Ast.Node.Tag.get_operator_string(node.tag);
            std.debug.assert(op_str.len == 2);
            try list.appendSlice(op_str);
            try list.append(' ');
            try convert_ast_to_string(ast, node.node_data.rhs, list);
            try list.append(')');
        },
        .INTEGER_LITERAL, .IDENTIFIER, .BOOLEAN_LITERAL => {
            try list.appendSlice(get_token_literal(ast, node.main_token));
        },
        .NEGATION, .BOOL_NOT => {
            try list.append('(');
            try list.appendSlice(Ast.Node.Tag.get_operator_string(node.tag));
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.append(')');
        },
        .FUNCTION_CALL => {
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.appendSlice("(");
            if (node.node_data.rhs != 0) {
                const start = ast.extra_data[node.node_data.rhs];
                const end = ast.extra_data[node.node_data.rhs + 1];
                for (start..end) |i| {
                    try convert_ast_to_string(ast, ast.extra_data[i], list);
                    try list.appendSlice(", ");
                }
                _ = list.pop();
                _ = list.pop();
            }
            try list.appendSlice(")");
        },
        .VAR_STATEMENT => {
            try list.appendSlice(get_token_literal(ast, node.main_token));
            try list.appendSlice(" ");
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.appendSlice(" ");
            try list.appendSlice("=");
            try list.appendSlice(" ");
            try convert_ast_to_string(ast, node.node_data.rhs, list);
            try list.appendSlice(";");
            try convert_ast_to_string(ast, node.node_data.rhs + 1, list);
        },
        .RETURN_STATEMENT => {
            try list.appendSlice(get_token_literal(ast, node.main_token));
            try list.appendSlice(" ");
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.appendSlice(";");
        },
        .EXPRESSION_STATEMENT => {
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.appendSlice(";");
            try convert_ast_to_string(ast, node.node_data.lhs + 1, list);
        },
        .ASSIGNMENT_STATEMENT => {
            try list.appendSlice("(");
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.appendSlice("=");
            try convert_ast_to_string(ast, node.node_data.lhs + 1, list);
            try list.appendSlice(")");
        },
        else => {},
    }
}

fn get_token_literal(ast: *Ast, tok_loc: Ast.TokenArrayIndex) []const u8 {
    const tok = ast.tokens.get(tok_loc);
    return ast.source_buffer[tok.start..tok.end];
}

test "evaluator_object_test" {
    const int_val: i64 = 41;
    var int_obj = try Object.Create(.integer, testing.allocator, @ptrCast(&int_val));
    defer testing.allocator.destroy(int_obj.integer);
    // int_obj.integer.value = 41;

    switch (int_obj) {
        .integer => |i| i.value += 1,
        inline else => unreachable,
    }
    try testing.expect(int_obj.integer.value == 42);

    const int_str = try int_obj.ToString(testing.allocator);
    defer testing.allocator.free(int_str);

    try testing.expectEqualSlices(u8, "42", int_str);

    const bool_value = false;
    var bool_obj = try Object.Create(.boolean, testing.allocator, @ptrCast(&bool_value));
    defer testing.allocator.destroy(bool_obj.boolean);
    bool_obj.boolean.value = false;

    switch (bool_obj) {
        .boolean => |i| i.value = true,
        inline else => unreachable,
    }
    try testing.expect(bool_obj.boolean.value);

    const bool_string = try bool_obj.ToString(testing.allocator);
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
    const func_obj = try Object.Create(.function_expression, testing.allocator, @ptrCast(&func_data));
    defer testing.allocator.destroy(func_obj.function_expression);

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

    const return_data = ObjectStructures.ReturnTypeUnion{ .bool = true };
    const return_obj = try Object.Create(.return_expression, testing.allocator, @ptrCast(&return_data));
    defer testing.allocator.destroy(return_obj.return_expression);
    return_obj.return_expression.value.bool = false;
    try testing.expect(!return_obj.return_expression.value.bool);
    switch (return_obj) {
        .return_expression => |r| r.value.bool = true,
        inline else => unreachable,
    }
    try testing.expect(return_obj.return_expression.value.bool);

    try testing.expect(bool_obj.boolean.value);

    var err_msg: []const u8 = undefined;
    var err_obj = try Object.Create(.runtime_error, testing.allocator, "");
    // err_obj.runtime_error.message = try std.fmt.allocPrint(testing.allocator, "{s}\n", .{"This is an error"});
    err_obj.runtime_error.value = "This is an error";
    // defer testing.allocator.free(err_obj.runtime_error.message);
    defer testing.allocator.destroy(err_obj.runtime_error);

    switch (err_obj) {
        .runtime_error => |e| {
            err_msg = e.value;
        },
        inline else => unreachable,
    }
    try testing.expectEqualSlices(u8, err_msg, err_obj.runtime_error.value);

    const null_obj: Object = .null;
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
