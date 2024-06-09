pub const Compiler = @This();

memory: *Memory,
allocator: Allocator,
symbol_table: *SymbolTable,

pub const CompilerErrors = error{ IllegalAstNodeReference, ReferencingRootNode, AssigningToConst };
const Error = CompilerErrors || Allocator.Error;

pub fn create(allocator: Allocator, map: *SymbolTable, memory: *Memory) !Compiler {
    const comp = Compiler{
        .memory = memory,
        .symbol_table = map,
        .allocator = allocator,
    };

    // inline for (std.meta.fields(Builtins)) |f| {
    //     const position = try comp.memory.alloc(.builtin, @ptrCast(&@field(Builtins.default, f.name)));
    //     const hash = try map.define(allocator, f.name, .constant, .global);
    //     _ = position;
    //     _ = hash;
    //     // try eval.environment_pool.create_variable(env, allocator, hash, position, .constant);
    // }
    return comp;
}

// pub fn get_byte_code(self: *Compiler) !ByteCode {
//     return ByteCode{
//         .instructions = self.memory.instructions.items,
//         .constants = self.memory.constants.slice(),
//     };
// }

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

fn compile_block_statements(self: *Compiler, ast: *const Ast, statements: []u32) !void {
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
                const start = self.memory.instructions.items.len;
                const obj_start = self.memory.constants.items.len;
                try self.eval_expression(ast, ast_node.node_data.lhs);
                const end = self.memory.instructions.items.len;
                var i = start;
                var is_modifying = false;
                while (i < end) {
                    const op: Code.Opcode = @enumFromInt(self.memory.instructions.items[i]);
                    if (op == .set_global) {
                        is_modifying = true;
                        break;
                    }
                    const value = Code.Definitions[@as(usize, @intCast(@intFromEnum(op)))];
                    i += 1;
                    for (value) |v| {
                        i += v;
                    }
                }
                if (!is_modifying) {
                    self.memory.instructions.shrinkRetainingCapacity(start);
                    self.memory.shrink_constants(obj_start);
                    return;
                }
                try self.emit(.pop, &[_]u32{});
            }
        },
        .VAR_STATEMENT => {
            try self.eval_expression(ast, ast_node.node_data.rhs);
            const ident = ast.nodes.get(ast_node.node_data.lhs);
            try self.emit(.set_global, &[_]u32{ident.node_data.lhs});
        },
        .ASSIGNMENT_STATEMENT => {
            try self.eval_expression(ast, ast_node.node_data.rhs);
            const ident = ast.nodes.get(ast_node.node_data.lhs);
            if (ident.node_data.rhs == 0) {
                return Error.AssigningToConst;
            }
            try self.emit(.set_global, &[_]u32{ident.node_data.lhs});
        },
        .WHILE_LOOP => {
            // While loop
            // main_token = while
            // lhs = condition expression
            // rhs = block to execute if lhs is true and then loop back to lhs condition
            try self.eval_while_loop(ast, ast_node);
        },
        else => unreachable,
    }
}

fn eval_while_loop(self: *Compiler, ast: *const Ast, node: Ast.Node) Error!void {
    const eval_start: u32 = @intCast(self.memory.instructions.items.len);
    try self.eval_expression(ast, node.node_data.lhs);
    const jn_pos = self.memory.instructions.items.len;
    try self.emit(.jn, &[_]u32{9090});

    const block_node_tag = ast.nodes.items(.tag)[node.node_data.rhs];
    std.debug.assert(block_node_tag == .BLOCK);
    const block_node_data = ast.nodes.items(.node_data)[node.node_data.rhs];
    const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
    try self.compile_block_statements(ast, statements);

    // const jmp_pos = self.memory.instructions.items.len;
    try self.emit(.jmp, &[_]u32{eval_start});

    self.overwrite_jmp_pos(jn_pos, self.memory.instructions.items.len);
}

fn eval_expression(self: *Compiler, ast: *const Ast, node: Ast.Node.NodeIndex) Error!void {
    if (node >= ast.nodes.len) {
        return Error.IllegalAstNodeReference;
    }

    if (node == 0) {
        return Error.ReferencingRootNode;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .INTEGER_LITERAL => {
            const obj = try self.memory.register_constant(
                .integer,
                @ptrCast(&ast.integer_literals[ast_node.node_data.lhs]),
            );
            try self.emit(.load_const, &[_]u32{obj});
        },
        .IDENTIFIER => {
            try self.emit(.get_global, &[_]u32{ast_node.node_data.lhs});
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
        .NAKED_IF, .IF_ELSE => {
            try self.emit_if_expression(ast, ast_node);
        },
        else => unreachable,
    }
}

fn emit_if_expression(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    // Emit condition evaluation
    try self.eval_expression(ast, ast_node.node_data.lhs);

    const jn_pos = self.memory.instructions.items.len;
    try self.emit(.jn, &[_]u32{9090});

    switch (ast_node.tag) {
        .NAKED_IF => {
            const block_node_tag = ast.nodes.items(.tag)[ast_node.node_data.rhs];
            std.debug.assert(block_node_tag == .BLOCK);
            const block_node_data = ast.nodes.items(.node_data)[ast_node.node_data.rhs];
            const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            try self.compile_block_statements(ast, statements);

            const jmp_pos = self.memory.instructions.items.len;
            try self.emit(.jmp, &[_]u32{9090});

            self.overwrite_jmp_pos(jn_pos, self.memory.instructions.items.len);

            try self.emit(.lnull, &[_]u32{});

            self.overwrite_jmp_pos(jmp_pos, self.memory.instructions.items.len);
        },
        .IF_ELSE => {
            const blocks = ast.extra_data[ast_node.node_data.rhs .. ast_node.node_data.rhs + 2];
            var block_node_data: Ast.Node.NodeData = undefined;
            var block_node_tag = ast.nodes.items(.tag)[blocks[0]];
            std.debug.assert(block_node_tag == .BLOCK);
            block_node_data = ast.nodes.items(.node_data)[blocks[0]];
            const statements_if = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            try self.compile_block_statements(ast, statements_if);

            const jmp_pos = self.memory.instructions.items.len;
            try self.emit(.jmp, &[_]u32{9090});

            self.overwrite_jmp_pos(jn_pos, self.memory.instructions.items.len);

            block_node_tag = ast.nodes.items(.tag)[blocks[1]];
            std.debug.assert(block_node_tag == .BLOCK);
            block_node_data = ast.nodes.items(.node_data)[blocks[1]];
            const statements_else = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            try self.compile_block_statements(ast, statements_else);

            self.overwrite_jmp_pos(jmp_pos, self.memory.instructions.items.len);
        },
        inline else => unreachable,
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

fn overwrite_jmp_pos(self: *Compiler, jmp_loc: usize, value: usize) void {
    const slice = std.mem.asBytes(&@as(u32, @intCast(value)));
    for (0..4) |i| {
        self.memory.instructions.items[jmp_loc + 1 + i] = slice[i];
    }
}

fn emit(self: *Compiler, op: Code.Opcode, operands: []const u32) !void {
    try Code.make(&self.memory.instructions, op, operands);
}

const CompilerTest = struct {
    source: [:0]const u8,
    expected_instructions: []const u8,
    expected_constant_tags: []const MemoryTypes,
    expected_data: []const Memory.MemoryObject.ObjectData,
};

test "test_arithmatic_compile" {
    const tests = [_]CompilerTest{
        .{
            .source = "1 + 2; 1 + 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 1 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },

        .{
            .source = "true",
            .expected_instructions = &[_]u8{11},
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
        },
        .{
            .source = "false",
            .expected_instructions = &[_]u8{12},
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
        },
        .{
            .source = "1 - 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 2 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },
        .{
            .source = "true == true",
            .expected_instructions = &[_]u8{ 11, 11, 9 },
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
        },
        .{
            .source = "1 * 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 3 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
        },
        .{
            .source = "1 < 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 7 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 2 },
                .{ .integer = 1 },
            },
        },
        .{
            .source = "!!-5",
            .expected_instructions = &[_]u8{ 0, 0, 0, 5, 8, 8 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 5 },
            },
        },
        .{
            .source = "if (1 < 2) { 10; } else { 50; 60;}",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 7, 14, 20, 0, 0, 0, 0, 2, 0, 13, 23, 0, 0, 0, 0, 3, 0 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 2 },
                .{ .integer = 1 },
                .{ .integer = 10 },
                .{ .integer = 60 },
            },
        },
        .{
            .source = "if (true) { 10; }",
            .expected_instructions = &[_]u8{ 11, 14, 14, 0, 0, 0, 0, 0, 0, 13, 15, 0, 0, 0, 15 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 10 },
            },
        },
        .{
            .source = "var a = 10; a = 5",
            .expected_instructions = &[_]u8{ 0, 0, 0, 16, 0, 0, 0, 1, 0, 16, 0, 0 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 10 },
                .{ .integer = 5 },
            },
        },
        .{
            .source = "var i = 0; while (i < 5) { i = i + 1}",
            .expected_instructions = &[_]u8{ 0, 0, 0, 16, 0, 0, 0, 1, 0, 17, 0, 0, 7, 14, 33, 0, 0, 0, 17, 0, 0, 0, 2, 0, 1, 16, 0, 0, 13, 6, 0, 0, 0 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 0 },
                .{ .integer = 5 },
                .{ .integer = 1 },
            },
        },
    };

    try test_compiler(&tests);
}

fn test_compiler(tests: []const CompilerTest) !void {
    for (tests) |t| {
        // std.debug.print("{s}\n", .{t.source});
        var symbol_table = SymbolTable.init();
        defer symbol_table.deinit(testing.allocator);
        var memory = try Memory.initCapacity(testing.allocator, 2048);
        defer memory.deinit();
        var compiler = try Compiler.create(testing.allocator, &symbol_table, &memory);
        var ast = try Parser.parse_program(t.source, testing.allocator, &symbol_table);
        defer ast.deinit(testing.allocator);
        try compiler.compile(&ast, 0);
        // var byte_code: ByteCode = try compiler.get_byte_code();
        try testing.expectEqual(t.expected_instructions.len, memory.instructions.items.len);
        try testing.expectEqual(t.expected_data.len, memory.constants.items.len);
        try testing.expectEqual(
            t.expected_constant_tags.len,
            memory.constants.items.len,
        );

        try testing.expectEqualSlices(u8, t.expected_instructions, memory.instructions.items);
        for (0..memory.constants.items.len) |i| {
            const data = memory.get_constant(@as(Memory.ConstantID, @intCast(i)));
            try testing.expectEqual(t.expected_constant_tags[i], data.dtype);
            switch (data.dtype) {
                .integer => try testing.expectEqual(
                    t.expected_data[i].integer,
                    data.data.integer,
                ),
                else => unreachable,
            }
        }
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
const Ast = @import("ast.zig");
const Code = @import("code.zig");
const Parser = @import("parser.zig");
const SymbolTable = @import("symbol_table.zig");
const MemoryTypes = Memory.Types;
const MemoryAddress = Memory.MemoryAddress;
const null_object = Memory.null_object;
const true_object = Memory.true_object;
const false_object = Memory.false_object;
const break_object = Memory.break_object;
const continue_object = Memory.continue_object;
// const ByteCode = @import("byte_code.zig");
const Builtins = @import("builtins.zig");
const Memory = @import("memory.zig");
