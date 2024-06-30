pub const Compiler = @This();

memory: *Memory,
allocator: Allocator,
scratch_pad: std.ArrayList(u8),
emit_to_scratch: bool,
scratch_ptrs: std.ArrayList(usize),
last_statement: Ast.Node.Tag,

pub const CompilerErrors = error{
    IllegalAstNodeReference,
    ReferencingRootNode,
    AssigningToConst,
    EmptyHashLiteral,
};
const Error = CompilerErrors || Allocator.Error;

pub fn create(allocator: Allocator, memory: *Memory) !Compiler {
    const comp = Compiler{
        .memory = memory,
        .allocator = allocator,
        .scratch_pad = std.ArrayList(u8).init(allocator),
        .scratch_ptrs = std.ArrayList(usize).init(allocator),
        .emit_to_scratch = false,
        .last_statement = .ROOT,
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

pub fn compile(self: *Compiler, ast: *const Ast, start: usize) !void {
    const root_node = ast.nodes.get(0);
    const statements = ast.extra_data[root_node.node_data.lhs..root_node.node_data.rhs];

    try self.compile_program_statements(ast, statements[start..]);
    self.scratch_pad.deinit();
    self.scratch_ptrs.deinit();
}

fn compile_program_statements(self: *Compiler, ast: *const Ast, statements: []u32) !void {
    for (statements, 0..) |s, i| {
        try self.compile_statement(ast, s, .{ .is_last = statements.len - 1 - i == 0, .pop_last = true });
    }
}

fn compile_block_statements(
    self: *Compiler,
    ast: *const Ast,
    config: struct {
        statements: []u32,
        pop_last: bool,
        emit_to_scratch: bool = false,
    },
) !void {
    self.emit_to_scratch = config.emit_to_scratch;
    for (config.statements, 0..) |s, i| {
        try self.compile_statement(
            ast,
            s,
            .{
                .is_last = config.statements.len - 1 - i == 0,
                .pop_last = config.pop_last,
            },
        );
    }
}

fn compile_statement(
    self: *Compiler,
    ast: *const Ast,
    node: Ast.Node.NodeIndex,
    config: struct {
        is_last: bool,
        pop_last: bool,
    },
) !void {
    if (node >= ast.nodes.len) {
        return Error.IllegalAstNodeReference;
    }

    if (node == 0) {
        return Error.ReferencingRootNode;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .EXPRESSION_STATEMENT => {
            if (config.is_last) {
                try self.compile_expression(ast, ast_node.node_data.lhs);
                if (config.pop_last) {
                    try self.emit(.pop, &[_]u32{});
                }
            } else {
                const start = self.get_current_ins_ptr();
                const obj_start = self.memory.constants.items.len;
                try self.compile_expression(ast, ast_node.node_data.lhs);
                const end = self.get_current_ins_ptr();
                var i = start;
                var is_modifying = false;
                while (i < end) {
                    // const op: Code.Opcode = @enumFromInt(self.memory.instructions.items[i]);
                    const op: Code.Opcode = @enumFromInt(self.get_ins_byte(i));
                    if (op == .set_global or op == .set_local or op == .call) {
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
                    self.shrink_instructions(start);
                    self.memory.shrink_constants(obj_start);
                    self.last_statement = ast_node.tag;
                    return;
                }
                try self.emit(.pop, &[_]u32{});
            }
        },
        .VAR_STATEMENT => {
            try self.compile_expression(ast, ast_node.node_data.rhs);
            const ident = ast.nodes.get(ast_node.node_data.lhs);
            const symbol = SymbolTree.identifier_to_symbol(ident.node_data);
            if (symbol.scope == .local) {
                try self.emit(.set_local, &[_]u32{ symbol.depth, ident.node_data.lhs });
            } else {
                try self.emit(.set_global, &[_]u32{ident.node_data.lhs});
            }
        },
        .ASSIGNMENT_STATEMENT => {
            try self.compile_expression(ast, ast_node.node_data.rhs);
            const ident = ast.nodes.get(ast_node.node_data.lhs);
            const symbol = SymbolTree.identifier_to_symbol(ident.node_data);
            if (symbol.type == .constant) {
                return Error.AssigningToConst;
            }
            if (symbol.scope == .local) {
                try self.emit(.set_local, &[_]u32{ symbol.depth, symbol.index });
            } else {
                try self.emit(.set_global, &[_]u32{symbol.index});
            }
        },
        .WHILE_LOOP => {
            try self.compile_while_loop(ast, ast_node);
        },
        .RETURN_STATEMENT => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.emit(.op_return, &[_]u32{});
        },
        else => unreachable,
    }
    self.last_statement = ast_node.tag;
}

fn compile_while_loop(self: *Compiler, ast: *const Ast, node: Ast.Node) Error!void {
    const eval_start: u32 = @intCast(self.get_current_ins_ptr());
    try self.compile_expression(ast, node.node_data.lhs);

    const jn_pos = self.get_current_ins_ptr();
    try self.emit(.jn, &[_]u32{9090});

    const block_node_tag = ast.nodes.items(.tag)[node.node_data.rhs];
    std.debug.assert(block_node_tag == .BLOCK);
    const block_node_data = ast.nodes.items(.node_data)[node.node_data.rhs];
    const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
    // const num_locals = statements[block_node_data.rhs - 1];
    // _ = num_locals;
    try self.compile_block_statements(ast, .{
        .statements = statements[0 .. statements.len - 1],
        .pop_last = false,
        .emit_to_scratch = self.emit_to_scratch,
    });

    try self.emit(.jmp, &[_]u32{eval_start});

    self.overwrite_jmp_pos(jn_pos, self.get_current_ins_ptr());
    try self.emit(.lnull, &[_]u32{});
    try self.emit(.pop, &[_]u32{});
}

fn compile_expression(self: *Compiler, ast: *const Ast, node: Ast.Node.NodeIndex) Error!void {
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
        .STRING_LITERAL => {
            const output = try std.fmt.allocPrint(
                self.allocator,
                "{s}",
                .{ast.source_buffer[ast_node.node_data.lhs..ast_node.node_data.rhs]},
            );
            const obj = try self.memory.register_constant(.string, @ptrCast(&output));
            try self.emit(.load_const, &[_]u32{obj});
        },
        .ARRAY_LITERAL => {
            try self.emit_array_literal(ast, ast_node);
        },
        .INDEX_RANGE => {
            // the array or string
            try self.compile_expression(ast, ast_node.node_data.lhs);
            // the left and right ranges
            const loc = ast_node.node_data.rhs;
            try self.compile_expression(ast, ast.extra_data[loc]);
            try self.compile_expression(ast, ast.extra_data[loc + 1]);
            try self.emit(.index_range, &[_]u32{});
        },
        .FUNCTION_EXPRESSION => try self.compile_function(ast, ast_node),
        .FUNCTION_CALL => try self.compile_function_call(ast, ast_node),
        .INDEX_INTO => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.compile_expression(ast, ast_node.node_data.rhs);
            try self.emit(.index_into, &[_]u32{});
        },
        .HASH_LITERAL => {
            try self.emit_hash_literal(ast, ast_node);
        },
        .IDENTIFIER => {
            const symbol = SymbolTree.identifier_to_symbol(ast_node.node_data);
            if (symbol.scope == .local) {
                try self.emit(.get_local, &[_]u32{ symbol.depth, ast_node.node_data.lhs });
            } else {
                try self.emit(.get_global, &[_]u32{ast_node.node_data.lhs});
            }
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
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.compile_expression(ast, ast_node.node_data.rhs);
            try self.emit_infix_op(ast_node.tag);
        },
        .LESS_THAN,
        .LESS_THAN_EQUAL,
        => {
            try self.compile_expression(ast, ast_node.node_data.rhs);
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.emit_infix_op(ast_node.tag);
        },
        .NOT_EQUAL => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.compile_expression(ast, ast_node.node_data.rhs);
            try self.emit(.neq, &[_]u32{});
        },
        .DOUBLE_EQUAL => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.compile_expression(ast, ast_node.node_data.rhs);
            try self.emit(.eq, &[_]u32{});
        },
        .BOOL_NOT => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.emit(.not, &[_]u32{});
        },
        .NEGATION => {
            try self.compile_expression(ast, ast_node.node_data.lhs);
            try self.emit(.neg, &[_]u32{});
        },
        .NAKED_IF, .IF_ELSE => {
            try self.emit_if_expression(ast, ast_node);
        },
        else => unreachable,
    }
}

fn compile_function_call(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    // A function call expression
    // main_token = (
    // lhs = node of identifier with function's name
    // rhs = location in extra_datas array
    // the argument expressions nodes are in extra_data[rhs..rhs+2]
    const state = self.emit_to_scratch;
    try self.compile_expression(ast, ast_node.node_data.lhs);
    self.emit_to_scratch = state;
    try self.emit(.call, &[_]u32{});
}

fn compile_function(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    // fn <FUNCTION_PARAMETER_BLOCK> <FUNCTION_BLOCK>
    // function expression main_token = fn
    // lhs = <FUNCTION_PARAMETER_BLOCK>
    // rhs = <FUNCTION_BLOCK> of type BLOCK
    const state = self.emit_to_scratch;
    const start = self.scratch_pad.items.len;

    try self.scratch_ptrs.append(start);

    const block_node = ast.extra_data[ast_node.node_data.rhs];
    const block_node_tag = ast.nodes.items(.tag)[block_node];
    std.debug.assert(block_node_tag == .BLOCK);
    const block_node_data = ast.nodes.items(.node_data)[block_node];
    const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
    try self.compile_block_statements(
        ast,
        .{
            .statements = statements,
            .pop_last = false,
            .emit_to_scratch = true,
        },
    );
    if (self.last_statement != .RETURN_STATEMENT) {
        try self.emit(.op_return, &[_]u32{});
    }
    const num_locals = ast.extra_data[ast_node.node_data.rhs + 1];
    const const_id = try self.memory.register_function(self.scratch_pad.items[start..], @intCast(num_locals));
    self.scratch_pad.shrinkRetainingCapacity(start);
    _ = self.scratch_ptrs.pop();
    self.emit_to_scratch = state;
    try self.emit(.load_const, &[_]u32{const_id});
}

fn emit_hash_literal(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    const hash_start = ast_node.node_data.lhs;
    const hash_end = ast_node.node_data.rhs;
    if (hash_end == 0) {
        return Error.EmptyHashLiteral;
    }

    const hashes = ast.extra_data[hash_start..hash_end];

    for (hashes) |hash| {
        const node = ast.nodes.get(hash);
        try self.compile_expression(ast, node.node_data.lhs);
        try self.compile_expression(ast, node.node_data.rhs);
    }

    try self.emit(.make_hash, &[_]u32{@as(u32, @intCast(hashes.len))});
}

fn emit_array_literal(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    const expression_start = ast_node.node_data.lhs;
    const expression_end = ast_node.node_data.rhs;
    if (expression_end == 0) {
        try self.emit(.array, &[_]u32{0});
    }

    const expressions = ast.extra_data[expression_start..expression_end];
    for (0..expressions.len) |i| {
        try self.compile_expression(ast, expressions[expressions.len - 1 - i]);
    }
    try self.emit(.array, &[_]u32{@as(u32, @intCast(expressions.len))});
}

fn emit_if_expression(self: *Compiler, ast: *const Ast, ast_node: Ast.Node) Error!void {
    // Emit condition evaluation
    try self.compile_expression(ast, ast_node.node_data.lhs);

    const jn_pos = self.get_current_ins_ptr();
    try self.emit(.jn, &[_]u32{9090});

    switch (ast_node.tag) {
        .NAKED_IF => {
            const block_node_tag = ast.nodes.items(.tag)[ast_node.node_data.rhs];
            std.debug.assert(block_node_tag == .BLOCK);
            const block_node_data = ast.nodes.items(.node_data)[ast_node.node_data.rhs];
            const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            try self.compile_block_statements(ast, .{
                .statements = statements[0 .. statements.len - 1],
                .pop_last = false,
                .emit_to_scratch = self.emit_to_scratch,
            });

            const jmp_pos = self.get_current_ins_ptr();
            try self.emit(.jmp, &[_]u32{9090});

            self.overwrite_jmp_pos(jn_pos, self.get_current_ins_ptr());

            try self.emit(.lnull, &[_]u32{});

            self.overwrite_jmp_pos(jmp_pos, self.get_current_ins_ptr());
        },
        .IF_ELSE => {
            const blocks = ast.extra_data[ast_node.node_data.rhs .. ast_node.node_data.rhs + 2];
            var block_node_data: Ast.Node.NodeData = undefined;
            var block_node_tag = ast.nodes.items(.tag)[blocks[0]];
            std.debug.assert(block_node_tag == .BLOCK);
            block_node_data = ast.nodes.items(.node_data)[blocks[0]];
            const statements_if = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            try self.compile_block_statements(ast, .{
                .statements = statements_if[0 .. statements_if.len - 1],
                .pop_last = false,
                .emit_to_scratch = self.emit_to_scratch,
            });

            const jmp_pos = self.get_current_ins_ptr();
            try self.emit(.jmp, &[_]u32{9090});

            self.overwrite_jmp_pos(jn_pos, self.get_current_ins_ptr());

            block_node_tag = ast.nodes.items(.tag)[blocks[1]];
            std.debug.assert(block_node_tag == .BLOCK);
            block_node_data = ast.nodes.items(.node_data)[blocks[1]];
            const statements_else = ast.extra_data[block_node_data.lhs..block_node_data.rhs];

            try self.compile_block_statements(ast, .{
                .statements = statements_else[0 .. statements_else.len - 1],
                .pop_last = false,
                .emit_to_scratch = self.emit_to_scratch,
            });

            self.overwrite_jmp_pos(jmp_pos, self.get_current_ins_ptr());
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
    const scratch_ptr = self.scratch_ptrs.getLastOrNull();
    if (scratch_ptr) |s| {
        for (0..4) |i| {
            self.scratch_pad.items[s + jmp_loc + 1 + i] = slice[i];
        }
    } else {
        for (0..4) |i| {
            self.memory.instructions.items[jmp_loc + 1 + i] = slice[i];
        }
    }
}

fn emit(self: *Compiler, op: Code.Opcode, operands: []const u32) !void {
    if (self.emit_to_scratch) {
        try Code.make(&self.scratch_pad, op, operands);
    } else {
        try Code.make(&self.memory.instructions, op, operands);
    }
}

fn get_current_ins_ptr(self: *Compiler) usize {
    const scratch_ptr = self.scratch_ptrs.getLastOrNull();
    if (scratch_ptr) |s| {
        return self.scratch_pad.items.len - s;
    } else {
        return self.memory.instructions.items.len;
    }
}

fn get_ins_byte(self: *Compiler, i: usize) u8 {
    const scratch_ptr = self.scratch_ptrs.getLastOrNull();
    if (scratch_ptr) |_| {
        return self.scratch_pad.items[i];
    } else {
        return self.memory.instructions.items[i];
    }
}

fn shrink_instructions(self: *Compiler, new_len: usize) void {
    const scratch_ptr = self.scratch_ptrs.getLastOrNull();
    if (scratch_ptr) |_| {
        return self.scratch_pad.shrinkRetainingCapacity(new_len);
    } else {
        return self.memory.instructions.shrinkRetainingCapacity(new_len);
    }
}

const CompilerTest = struct {
    source: [:0]const u8,
    expected_instructions: []const u8,
    expected_constant_tags: []const MemoryTypes,
    expected_data: []const Memory.MemoryObject.ObjectData,
    expected_function: []const u8,
};

test "test_arithmatic_compile" {
    var str1 = std.ArrayListUnmanaged(u8){};
    var str2 = std.ArrayListUnmanaged(u8){};
    try str1.appendSlice(testing.allocator, "foo");
    try str2.appendSlice(testing.allocator, "bar");
    defer str1.deinit(testing.allocator);
    defer str2.deinit(testing.allocator);
    const tests = [_]CompilerTest{
        .{
            .source = "1 + 2; 1 + 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
            .expected_function = &[_]u8{},
        },

        .{
            .source = "true",
            .expected_instructions = &[_]u8{ 11, 18 },
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
            .expected_function = &[_]u8{},
        },
        .{
            .source = "false",
            .expected_instructions = &[_]u8{ 12, 18 },
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
            .expected_function = &[_]u8{},
        },
        .{
            .source = "1 - 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 2, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
            .expected_function = &[_]u8{},
        },
        .{
            .source = "true == true",
            .expected_instructions = &[_]u8{ 11, 11, 9, 18 },
            .expected_constant_tags = &[_]MemoryTypes{},
            .expected_data = &[_]Memory.MemoryObject.ObjectData{},
            .expected_function = &[_]u8{},
        },
        .{
            .source = "1 * 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 3, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 1 },
                .{ .integer = 2 },
            },
            .expected_function = &[_]u8{},
        },
        .{
            .source = "1 < 2",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 7, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 2 },
                .{ .integer = 1 },
            },
            .expected_function = &[_]u8{},
        },
        .{
            .source = "!!-5",
            .expected_instructions = &[_]u8{ 0, 0, 0, 5, 8, 8, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 5 },
            },
            .expected_function = &[_]u8{},
        },
        .{
            .source = "if (1 < 2) { 10; } else { 50; 60;}",
            .expected_instructions = &[_]u8{ 0, 0, 0, 0, 1, 0, 7, 14, 20, 0, 0, 0, 0, 2, 0, 13, 23, 0, 0, 0, 0, 3, 0, 18 },
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
            .expected_function = &[_]u8{},
        },
        .{
            .source = "if (true) { 10; }",
            .expected_instructions = &[_]u8{ 11, 14, 14, 0, 0, 0, 0, 0, 0, 13, 15, 0, 0, 0, 15, 18 },
            .expected_constant_tags = &[_]MemoryTypes{
                .integer,
            },
            .expected_data = &[_]Memory.MemoryObject.ObjectData{
                .{ .integer = 10 },
            },
            .expected_function = &[_]u8{},
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
            .expected_function = &[_]u8{},
        },
        .{
            .source = "var i = 0; while (i < 5) { i = i + 1}",
            .expected_instructions = &[_]u8{ 0, 0, 0, 16, 0, 0, 0, 1, 0, 17, 0, 0, 7, 14, 33, 0, 0, 0, 17, 0, 0, 0, 2, 0, 1, 16, 0, 0, 13, 6, 0, 0, 0, 15, 18 },
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
            .expected_function = &[_]u8{},
        },
    };

    try test_compiler(&tests);
}

// test "compile function" {
//     const tests = [_]CompilerTest{
//         .{
//             .source = "fn() { 5 + 10 }",
//             .expected_instructions = &[_]u8{ 0, 2, 0, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24 },
//         },
//         .{
//             .source = "fn() { return 5 + 10 }",
//             .expected_instructions = &[_]u8{ 0, 2, 0, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24 },
//         },
//         .{
//             .source = "fn() { return fn() { 5 + 10} }",
//             .expected_instructions = &[_]u8{ 0, 3, 0, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//                 .{ .function = .{ .ptr = 8, .len = 4 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24, 0, 2, 0, 24 },
//         },
//     };
//     try test_compiler(&tests);
// }
//
// test "compile function calls" {
//     const tests = [_]CompilerTest{
//         .{
//             .source = "fn() { 5 + 10 }()",
//             .expected_instructions = &[_]u8{ 0, 2, 0, 23, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24 },
//         },
//         .{
//             .source = "fn() { return 5 + 10 }()",
//             .expected_instructions = &[_]u8{ 0, 2, 0, 23, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24 },
//         },
//         .{
//             .source = "var a = fn() { return 5 + 10 }; a()",
//             .expected_instructions = &[_]u8{ 0, 2, 0, 16, 0, 0, 17, 0, 0, 23, 18 },
//             .expected_constant_tags = &[_]MemoryTypes{
//                 .integer,
//                 .integer,
//                 .compiled_function,
//             },
//             .expected_data = &[_]Memory.MemoryObject.ObjectData{
//                 .{ .integer = 5 },
//                 .{ .integer = 10 },
//                 .{ .function = .{ .ptr = 0, .len = 8 } },
//             },
//             .expected_function = &[_]u8{ 0, 0, 0, 0, 1, 0, 1, 24 },
//         },
//     };
//     try test_compiler(&tests);
// }

fn test_compiler(tests: []const CompilerTest) !void {
    for (tests) |t| {
        // std.debug.print("{s}\n", .{t.source});
        var symbol_tree = SymbolTree.init(testing.allocator);
        defer symbol_tree.deinit();
        var memory = try Memory.initCapacity(testing.allocator, 2048);
        defer memory.deinit();
        var compiler = try Compiler.create(testing.allocator, &memory);
        var ast = try Parser.parse_program(t.source, testing.allocator, &symbol_tree);
        defer ast.deinit(testing.allocator);
        // ast.print_to_stderr();
        try compiler.compile(&ast, 0);
        const out = try Code.code_to_str(testing.allocator, memory.instructions.items);
        defer testing.allocator.free(out);
        // std.debug.print("Ins: {any}\n", .{memory.instructions.items});
        // std.debug.print("Function Data: {any}\n", .{memory.function_storage.items});
        // std.debug.print("Scratch Data: {any}\n", .{compiler.scratch_pad.items});
        // std.debug.print("Instructions:\n{s}\n", .{out});
        try testing.expectEqual(t.expected_instructions.len, memory.instructions.items.len);
        try testing.expectEqual(t.expected_data.len, memory.constants.items.len);
        try testing.expectEqual(
            t.expected_constant_tags.len,
            memory.constants.items.len,
        );

        try testing.expectEqual(t.expected_function.len, memory.function_storage.items.len);
        try testing.expectEqualSlices(u8, t.expected_function, memory.function_storage.items);

        try testing.expectEqualSlices(u8, t.expected_instructions, memory.instructions.items);
        for (0..memory.constants.items.len) |i| {
            const data = memory.get_constant(@as(Memory.ConstantID, @intCast(i)));
            try testing.expectEqual(t.expected_constant_tags[i], data.dtype);
            switch (data.dtype) {
                .integer => try testing.expectEqual(
                    t.expected_data[i].integer,
                    data.data.integer,
                ),
                .compiled_function => try testing.expectEqual(
                    t.expected_data[i].function,
                    data.data.function,
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
const Builtins = @import("builtins.zig");
const Memory = @import("memory.zig");
const SymbolTree = @import("symbol_tree.zig");
