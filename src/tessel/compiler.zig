pub const Compiler = @This();

instructions: std.ArrayList(u8),
constants: ObjectPool,
gpa: Allocator,

pub const CompilerErrors = error{ IllegalAstNodeReference, ReferencingRootNode };
const Error = CompilerErrors || Allocator.Error;

pub fn init(allocator: Allocator) !Compiler {
    return .{
        .instructions = std.ArrayList(u8).init(allocator),
        .constants = try ObjectPool.init(allocator),
        .gpa = allocator,
    };
}

pub fn deinit(self: *Compiler, allocator: Allocator) void {
    self.constants.deinit(allocator);
    self.instructions.deinit();
}

pub fn compile(self: *Compiler, ast: *const Ast, start: usize) !void {
    const root_node = ast.nodes.get(0);
    const statements = ast.extra_data[root_node.node_data.lhs..root_node.node_data.rhs];

    try self.compile_program_statements(ast, statements[start..]);
}

fn compile_program_statements(self: *Compiler, ast: *const Ast, statements: []u32) !void {
    for (statements, 0..) |s, i| {
        try self.compile_statement(ast, s, statements.len - 1 - i == 0);
    }
}

fn compile_statement(self: *Compiler, ast: *const Ast, node: Ast.Node.NodeIndex, is_last: bool) !void {
    if (node >= ast.nodes.len) {
        return Error.IllegalAstNodeReference;
    }

    if (node == 0) {
        return Error.ReferencingRootNode;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .EXPRESSION_STATEMENT => {
            if (is_last) {
                return self.eval_expression(ast, ast_node.node_data.lhs);
            } else {
                return;
            }
        },
        else => unreachable,
    }
}

fn eval_expression(self: *Compiler, ast: *const Ast, node: Ast.Node.NodeIndex) !void {
    if (node >= ast.nodes.len) {
        return Error.IllegalAstNodeReference;
    }

    if (node == 0) {
        return Error.ReferencingRootNode;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .INTEGER_LITERAL => {
            const obj = try self.constants.create(
                self.gpa,
                .integer,
                @ptrCast(&ast.integer_literals[ast_node.node_data.lhs]),
            );
            try self.emit(.load_const, &[_]u32{obj});
        },
        .BOOLEAN_LITERAL => {
            if (ast_node.node_data.lhs == 1) {
                try self.emit(.ltrue, &[_]u32{});
            } else {
                try self.emit(.lfalse, &[_]u32{});
            }
        },
        .GREATER_THAN,
        .GREATER_THAN_EQUAL,
        .ADDITION,
        .SUBTRACTION,
        .MULTIPLY,
        .DIVIDE,
        => {
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.eval_expression(ast, ast_node.node_data.rhs);
            try self.emit_infix_op(ast_node.tag);
        },
        .LESS_THAN,
        .LESS_THAN_EQUAL,
        => {
            try self.eval_expression(ast, ast_node.node_data.rhs);
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.emit_infix_op(ast_node.tag);
        },
        .NOT_EQUAL => {
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.eval_expression(ast, ast_node.node_data.rhs);
            try self.emit(.neq, &[_]u32{});
        },
        .DOUBLE_EQUAL => {
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.eval_expression(ast, ast_node.node_data.rhs);
            try self.emit(.eq, &[_]u32{});
        },
        .BOOL_NOT => {
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.emit(.not, &[_]u32{});
        },
        .NEGATION => {
            try self.eval_expression(ast, ast_node.node_data.lhs);
            try self.emit(.neg, &[_]u32{});
        },
        else => unreachable,
    }
}

fn emit_infix_op(self: *Compiler, tag: Ast.Node.Tag) !void {
    switch (tag) {
        .GREATER_THAN, .LESS_THAN => {
            try self.emit(.gt, &[_]u32{});
        },
        .LESS_THAN_EQUAL, .GREATER_THAN_EQUAL => {
            try self.emit(.gt, &[_]u32{});
        },
        .ADDITION => {
            try self.emit(.add, &[_]u32{});
        },
        .SUBTRACTION => {
            try self.emit(.sub, &[_]u32{});
        },
        .MULTIPLY => {
            try self.emit(.mul, &[_]u32{});
        },
        .DIVIDE => {
            try self.emit(.div, &[_]u32{});
        },
        else => unreachable,
    }
}

fn emit(self: *Compiler, op: Code.Opcode, operands: []const ObjectIndex) !void {
    try Code.make(&self.instructions, op, operands);
}

pub fn get_byte_code(self: *Compiler) !ByteCode {
    return .{
        .instructions = try self.instructions.toOwnedSlice(),
        .constants = self.constants.object_pool.toOwnedSlice(),
    };
}

const CompilerTest = struct {
    source: [:0]const u8,
    expected_instructions: []const u8,
    expected_constant_tags: []const ObjectPool.ObjectTypes,
    expected_data: []const ObjectPool.InternalObject.ObjectData,
};

test "test_arithmatic_compile" {
    const tests = [_]CompilerTest{
        .{
            .source = "1 + 2; 1 + 2",
            .expected_instructions = &[_]u8{ 0, 5, 0, 0, 6, 0, 1 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },
        .{
            .source = "true",
            .expected_instructions = &[_]u8{11},
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{},
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{},
        },
        .{
            .source = "false",
            .expected_instructions = &[_]u8{12},
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{},
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{},
        },
        .{
            .source = "1 - 2",
            .expected_instructions = &[_]u8{ 0, 5, 0, 0, 6, 0, 2 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },
        .{
            .source = "true == true",
            .expected_instructions = &[_]u8{ 11, 11, 9 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{},
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{},
        },
        .{
            .source = "1 * 2",
            .expected_instructions = &[_]u8{ 0, 5, 0, 0, 6, 0, 3 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },
        .{
            .source = "1 < 2",
            .expected_instructions = &[_]u8{ 0, 5, 0, 0, 6, 0, 7 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{
                .{ .integer = 2 },
                .{ .integer = 1 },
            },
        },
        .{
            .source = "!!-5",
            .expected_instructions = &[_]u8{ 0, 5, 0, 5, 8, 8 },
            .expected_constant_tags = &[_]ObjectPool.ObjectTypes{
                .integer,
            },
            .expected_data = &[_]ObjectPool.InternalObject.ObjectData{
                .{ .integer = 5 },
            },
        },
    };

    try test_compiler(&tests);
}

fn test_compiler(tests: []const CompilerTest) !void {
    for (tests) |t| {
        var identifier_map = IdentifierMap.init();
        defer identifier_map.deinit(testing.allocator);
        var compiler = try Compiler.init(testing.allocator);
        defer compiler.deinit(testing.allocator);
        var ast = try Parser.parse_program(t.source, testing.allocator, &identifier_map);
        defer ast.deinit(testing.allocator);
        try compiler.compile(&ast, 0);
        var byte_code: ByteCode = try compiler.get_byte_code();
        try testing.expectEqual(t.expected_instructions.len, byte_code.instructions.len);
        try testing.expectEqual(t.expected_data.len, byte_code.constants.len - ObjectPool.reserved_objects);
        try testing.expectEqual(
            t.expected_constant_tags.len,
            byte_code.constants.len - ObjectPool.reserved_objects,
        );

        try testing.expectEqualSlices(u8, t.expected_instructions, byte_code.instructions);
        try testing.expectEqualSlices(
            ObjectPool.ObjectTypes,
            t.expected_constant_tags,
            byte_code.constants.items(.tag)[ObjectPool.reserved_objects..],
        );
        for (ObjectPool.reserved_objects..byte_code.constants.len) |i| {
            switch (byte_code.constants.items(.tag)[i]) {
                .integer => try testing.expectEqual(
                    t.expected_data[i - ObjectPool.reserved_objects].integer,
                    byte_code.constants.items(.data)[i].integer,
                ),
                else => unreachable,
            }
        }
        testing.allocator.free(byte_code.instructions);
        byte_code.constants.deinit(testing.allocator);
    }
}

const ObjectPool = @import("object.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
const Ast = @import("ast.zig");
const Code = @import("code.zig");
const Parser = @import("parser.zig");
const IdentifierMap = @import("identifier_map.zig");
const ObjectTypes = ObjectPool.ObjectTypes;
const ObjectIndex = ObjectPool.ObjectIndex;
const null_object = ObjectPool.null_object;
const true_object = ObjectPool.true_object;
const false_object = ObjectPool.false_object;
const break_object = ObjectPool.break_object;
const continue_object = ObjectPool.continue_object;
const ByteCode = @import("byte_code.zig");
