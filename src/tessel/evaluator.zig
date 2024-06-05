pub const Evaluator = @This();

object_pool: ObjectPool,
environment_pool: EnvironmentPool,

pub const Error = error{ReferencingNodeZero} || ObjectPool.Error;

pub fn init(allocator: Allocator, env: EnvironmentIndex, map: *SymbolTable) !Evaluator {
    var eval = Evaluator{
        .object_pool = try ObjectPool.init(allocator),
        .environment_pool = try EnvironmentPool.init(allocator),
    };

    inline for (std.meta.fields(Builtins)) |f| {
        const position = try eval.object_pool.create(allocator, .builtin, @ptrCast(&@field(Builtins.default, f.name)));
        const hash = try map.define(allocator, f.name, .constant, .global);
        try eval.environment_pool.create_variable(env, allocator, hash.index, position, .constant);
    }
    return eval;
}

pub fn deinit(self: *Evaluator, allocator: Allocator) void {
    self.environment_pool.deinit(allocator, &self.object_pool);
    self.object_pool.deinit(allocator);
}

pub fn evaluate_program(self: *Evaluator, ast: *const Ast, start: u32, allocator: Allocator, env: EnvironmentIndex) Error!ObjectIndex {
    const root_node = ast.nodes.get(0);
    const statements = ast.extra_data[root_node.node_data.lhs..root_node.node_data.rhs];

    return self.evaluate_program_statements(ast, statements[start..], allocator, env);
}

fn evaluate_program_statements(
    self: *Evaluator,
    ast: *const Ast,
    statemnts: []u32,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    for (statemnts, 0..) |s, i| {
        const obj_pos = try self.eval_statement(ast, s, allocator, env);

        switch (self.object_pool.get_tag(obj_pos)) {
            .return_expression => {
                const packed_object = self.object_pool.get_data(obj_pos).return_value;
                self.object_pool.free(allocator, obj_pos);
                return packed_object;
            },
            .runtime_error => {
                return obj_pos;
            },
            .break_statement, .continue_statement => {
                self.object_pool.free(allocator, obj_pos);
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Illegal break or continue statement",
                    .{},
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            },
            else => {},
        }

        if (i == statemnts.len - 1) {
            return obj_pos;
        }
        self.object_pool.free(allocator, obj_pos);
    }
    return null_object;
}

fn evaluate_block(
    self: *Evaluator,
    ast: *const Ast,
    statemnts: []u32,
    allocator: Allocator,
    env: EnvironmentIndex,
    allow_break_continue: bool,
) Error!ObjectIndex {
    for (statemnts, 0..) |s, i| {
        const obj_pos = try self.eval_statement(ast, s, allocator, env);

        switch (self.object_pool.get_tag(obj_pos)) {
            .return_expression, .runtime_error => {
                return obj_pos;
            },
            .break_statement, .continue_statement => {
                if (allow_break_continue) {
                    return obj_pos;
                } else {
                    self.object_pool.free(allocator, obj_pos);
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Illegal break or continue statement",
                        .{},
                    );
                    return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
                }
            },
            else => {},
        }

        if (i == statemnts.len - 1) {
            return obj_pos;
        }
        self.object_pool.free(allocator, obj_pos);
    }
    return null_object;
}

fn eval_statement(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node.NodeIndex,
    allocator: Allocator,
    env: EnvironmentIndex,
) !ObjectIndex {
    if (node >= ast.nodes.len) {
        return null_object;
    }

    if (node == 0) {
        return Error.ReferencingNodeZero;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .EXPRESSION_STATEMENT => {
            return self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
        },
        .RETURN_STATEMENT => {
            const value = try self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            const return_pos = self.object_pool.create(allocator, .return_expression, @ptrCast(&value));
            return return_pos;
        },
        .VAR_STATEMENT => return self.eval_var_decl(ast, ast_node, allocator, env),
        .ASSIGNMENT_STATEMENT => return self.eval_var_assign(ast, ast_node, allocator, env),
        .BREAK_STATEMENT => return break_object,
        .CONTINUE_STATEMENT => return continue_object,
        .WHILE_LOOP => return self.eval_while_loop(ast, ast_node, allocator, env),
        else => {
            const output = try std.fmt.allocPrint(
                allocator,
                "FATAL: Something has gone horribly wrong. Unknown statement of type {s}.",
                .{@tagName(ast_node.tag)},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
    }
}

fn eval_var_decl(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const value_pos = try self.eval_expression(ast, node.node_data.rhs, allocator, env);
    const value_tag = self.object_pool.get_tag(value_pos);
    if (value_pos == break_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Illegal break. Cannot create variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (value_pos == continue_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Illegal continue. Cannot create variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (value_pos == null_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Expression returns nothing. Cannot store nothing into a variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (value_tag == .runtime_error) {
        return value_pos;
    }

    const var_tag = get_token_literal(ast, node.main_token);
    var tag: Environment.StorageType.Tag = .constant;
    if (std.mem.eql(u8, var_tag, "var")) {
        tag = .variable;
    }

    var actual_pos: ObjectIndex = undefined;
    switch (value_tag) {
        .return_expression => {
            actual_pos = self.object_pool.get_data(value_pos).return_value;
            self.object_pool.free(allocator, value_pos);
        },
        else => {
            actual_pos = value_pos;
        },
    }

    const ident_node = ast.nodes.get(node.node_data.lhs);

    _ = self.environment_pool.create_variable(env, allocator, ident_node.node_data.lhs, actual_pos, tag) catch |err| switch (err) {
        Environment.Error.VariableAlreadyInitialised => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Identifier \"{s}\" has already been initialised",
                .{get_token_literal(ast, ident_node.main_token)},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        Environment.Error.NonExistantVariable => unreachable,
        Environment.Error.ConstVariableModification => unreachable,
        Environment.Error.ExceedingMaxDepth => unreachable,
        EnvironmentPool.EnvPoolError.AccessingFreeEnv => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Something has gone horribly wrong. Accessing environment that is not initialized",
                .{},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        else => |overflow| return overflow,
    };
    self.object_pool.increase_ref(actual_pos);
    return null_object;
}

fn eval_var_assign(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const value_pos = try self.eval_expression(ast, node.node_data.rhs, allocator, env);
    const value_tag = self.object_pool.get_tag(value_pos);
    if (value_tag == .runtime_error) {
        return value_pos;
    }
    if (value_pos == break_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Illegal break. Cannot store this variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (value_pos == continue_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Illegal continue. Cannot store this into variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (value_pos == null_object) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Expression returns nothing. Cannot store nothing into a variable",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    var actual_pos: ObjectIndex = undefined;
    switch (value_tag) {
        .return_expression => {
            actual_pos = self.object_pool.get_data(value_pos).return_value;
            self.object_pool.free(allocator, value_pos);
        },
        else => {
            actual_pos = value_pos;
        },
    }

    const ident_node = ast.nodes.get(node.node_data.lhs);
    const hash = ident_node.node_data.lhs;
    const old_obj = self.environment_pool.update_variable(env, hash, actual_pos) catch |err| switch (err) {
        Environment.Error.NonExistantVariable => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Identifier \"{s}\" does not exist. Cannot assign anything to it.",
                .{get_token_literal(ast, ident_node.main_token)},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        Environment.Error.ConstVariableModification => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Identifier \"{s}\" is declared as a constant and cannot be modified",
                .{get_token_literal(ast, ident_node.main_token)},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        EnvironmentPool.EnvPoolError.AccessingFreeEnv => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Something has gone horribly wrong. Accessing environment that is not initialized",
                .{},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        Environment.Error.VariableAlreadyInitialised => unreachable,
        Environment.Error.ExceedingMaxDepth => unreachable,
        else => |overflow| return overflow,
    };
    self.object_pool.free(allocator, old_obj);

    return null_object;
}

fn eval_while_loop(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    while (true) {
        const truth = try self.eval_expression(ast, node.node_data.lhs, allocator, env);
        if (truth == true_object) {
            const block_node_tag = ast.nodes.items(.tag)[node.node_data.rhs];
            std.debug.assert(block_node_tag == .BLOCK);
            const block_node_data = ast.nodes.items(.node_data)[node.node_data.rhs];
            const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            const result = try self.evaluate_block(ast, statements, allocator, env, true);
            const result_tag = self.object_pool.get_tag(result);
            if (result_tag == .runtime_error) {
                return result;
            }
            if (result_tag == .break_statement) {
                return null_object;
            }
            continue;
        } else {
            return null_object;
        }
    }
}

fn eval_expression(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node.NodeIndex,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    if (node >= ast.nodes.len) {
        return null_object;
    }

    if (node == 0) {
        return Error.ReferencingNodeZero;
    }

    const ast_node = ast.nodes.get(node);
    var obj: ObjectIndex = undefined;
    switch (ast_node.tag) {
        .INTEGER_LITERAL => {
            obj = try self.object_pool.create(allocator, .integer, @ptrCast(&ast.integer_literals[ast_node.node_data.lhs]));
        },
        .STRING_LITERAL => {
            const output = try std.fmt.allocPrint(
                allocator,
                "{s}",
                .{ast.source_buffer[ast_node.node_data.lhs..ast_node.node_data.rhs]},
            );
            obj = try self.object_pool.create(allocator, .string, @ptrCast(&output));
        },
        .BOOLEAN_LITERAL => {
            obj = if (ast_node.node_data.lhs == 1) true_object else false_object;
        },
        .ARRAY_LITERAL => {
            obj = try self.eval_array_literal(ast, ast_node, allocator, env);
        },
        .HASH_LITERAL => {
            obj = try self.eval_hash_map(ast, ast_node, allocator, env);
        },
        .INDEX_INTO => {
            obj = try self.eval_index_into(ast, ast_node, allocator, env);
        },
        .INDEX_RANGE => {
            obj = try self.eval_index_range(ast, ast_node, allocator, env);
        },
        .IDENTIFIER => {
            const ident_name = get_token_literal(ast, ast_node.main_token);
            const hash = ast_node.node_data.lhs;
            const value_pos = self.environment_pool.get_object(env, hash) catch {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Something has gone horribly wrong. Accessing environment that is not initialized",
                    .{},
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            };
            if (value_pos == null_object) {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Identifier not found: {s}",
                    .{ident_name},
                );
                obj = try self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            } else {
                self.object_pool.increase_ref(value_pos);
                return value_pos;
            }
        },
        .NEGATION, .BOOL_NOT => {
            const left = try self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (self.object_pool.get_tag(left) == .runtime_error) return left;
            obj = try self.eval_prefix_operation(ast_node.tag, left, allocator);
        },
        .DOUBLE_EQUAL,
        .NOT_EQUAL,
        => {
            const left = try self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (self.object_pool.get_tag(left) == .runtime_error) return left;
            const right = try self.eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            if (self.object_pool.get_tag(right) == .runtime_error) {
                self.object_pool.free(allocator, left);
                return right;
            }
            obj = try self.eval_intboolean_infix_operation(ast, &ast_node, left, right, allocator);
        },
        .LESS_THAN,
        .GREATER_THAN,
        .LESS_THAN_EQUAL,
        .GREATER_THAN_EQUAL,
        .ADDITION,
        .SUBTRACTION,
        .MULTIPLY,
        .DIVIDE,
        => {
            const left = try self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (self.object_pool.get_tag(left) == .runtime_error) return left;
            const right = try self.eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            if (self.object_pool.get_tag(right) == .runtime_error) {
                self.object_pool.free(allocator, left);
                return right;
            }

            obj = try self.eval_infix_operation(ast, &ast_node, left, right, allocator);
        },
        .NAKED_IF, .IF_ELSE => {
            obj = try self.eval_if_expression(ast, ast_node, allocator, env);
        },
        .FUNCTION_EXPRESSION => {
            obj = try self.eval_function_expression(ast, ast_node, allocator, env);
        },
        .FUNCTION_CALL => {
            obj = try self.eval_function_call(ast, ast_node, allocator, env);
        },
        else => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Expected an expression but got {s}",
                .{@tagName(ast_node.tag)},
            );
            obj = try self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
    }

    return obj;
}

fn eval_hash_map(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const map_start = node.node_data.lhs;
    const map_end = node.node_data.rhs;
    const elements = ast.extra_data[map_start..map_end];

    const map = try self.object_pool.create(allocator, .hash_map, @ptrCast(&elements.len));
    for (elements) |e| {
        const element_node = ast.nodes.get(e);
        const key = try self.eval_expression(ast, element_node.node_data.lhs, allocator, env);
        const key_tag = self.object_pool.get_tag(key);
        if (key_tag != .integer and key_tag != .string and key_tag != .boolean) {
            const output = try std.fmt.allocPrint(
                allocator,
                "Expected key for hashmap to be INTEGER, STRING, OR BOOLEAN. Got {s}",
                .{self.object_pool.get_tag_string(key)},
            );
            self.object_pool.free(allocator, key);
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        }
        const hash_key = ObjectPool.InternalObject.HashKey.create(self.object_pool.get(key));
        const value = try self.eval_expression(ast, element_node.node_data.rhs, allocator, env);
        if (value == null_object) {
            const output = try std.fmt.allocPrint(
                allocator,
                "Missing value for key in hash map.",
                .{},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        }
        try self.object_pool.object_pool.items(.data)[map].hash_map.map.put(allocator, hash_key, value);
        try self.object_pool.object_pool.items(.data)[map].hash_map.keys.append(allocator, key);
        self.object_pool.increase_ref(value);
        self.object_pool.increase_ref(key);
    }
    return map;
}

fn eval_index_range(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const literal = try self.eval_expression(ast, node.node_data.lhs, allocator, env);
    const literal_tag = self.object_pool.get_tag(literal);
    if (literal_tag == .runtime_error) {
        return literal;
    }

    defer self.object_pool.free(allocator, literal);

    if (literal_tag != .array and literal_tag != .string) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Cannot slice into type: {s}",
            .{self.object_pool.get_tag_string(literal)},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    const l_index = try self.eval_expression(ast, ast.extra_data[node.node_data.rhs], allocator, env);
    const l_index_tag = self.object_pool.get_tag(l_index);

    if (l_index_tag == .runtime_error) {
        return literal;
    }

    defer self.object_pool.free(allocator, l_index);

    if (l_index_tag != .integer) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Left of an index range should evaluate to an integer. Got {d}",
            .{self.object_pool.get_tag_string(l_index)},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    const r_index = try self.eval_expression(ast, ast.extra_data[node.node_data.rhs + 1], allocator, env);
    const r_index_tag = self.object_pool.get_tag(r_index);

    if (r_index_tag == .runtime_error) {
        return literal;
    }

    defer self.object_pool.free(allocator, r_index);

    if (r_index_tag != .integer) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Right of an index range should evaluate to an integer. Got {d}",
            .{self.object_pool.get_tag_string(r_index)},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    const len = switch (literal_tag) {
        .array => @as(i64, @intCast(self.object_pool.get_data(literal).array.items.len)),
        .string => @as(i64, @intCast(self.object_pool.get_data(literal).string_type.items.len)),
        else => unreachable,
    };

    const l_index_value = self.object_pool.get_data(l_index).integer;
    const r_index_value = self.object_pool.get_data(r_index).integer;
    if (l_index_value >= len or l_index_value < -(len - 1)) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Left index out of bounds for {s} with length: {d}. Got {d}",
            .{ self.object_pool.get_tag_string(literal), len, l_index_value },
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    if (r_index_value > len or r_index_value < -(len - 1)) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Left index out of bounds for {s} with length: {d}. Got {d}",
            .{ self.object_pool.get_tag_string(literal), len, r_index_value },
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    var left: usize = undefined;
    var right: usize = undefined;
    if (l_index_value >= 0) {
        left = @as(usize, @intCast(l_index_value));
    } else {
        left = @as(usize, @intCast(len + l_index_value));
    }
    if (r_index_value >= 0) {
        right = @as(usize, @intCast(r_index_value));
    } else {
        right = @as(usize, @intCast(len + r_index_value));
    }
    if (r_index_value > l_index) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Left index must be lower than right index. Got left: {d} right: {d}",
            .{ left, right },
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    switch (literal_tag) {
        .array => {
            const array_data = self.object_pool.get_data(literal).array;
            const items = array_data.items[left..right];
            for (items) |obj| {
                self.object_pool.increase_ref(obj);
            }
            const array_storage = ObjectPool.InternalObject.ArrayType{
                .data = items,
            };
            const return_object = try self.object_pool.create(allocator, .array, @ptrCast(&array_storage));

            return return_object;
        },
        .string => {
            const string_data = self.object_pool.get_data(literal).string_type;
            const out_str = try std.fmt.allocPrint(allocator, "{s}", .{string_data.items});
            const return_object = try self.object_pool.create(allocator, .string, @ptrCast(&out_str));

            return return_object;
        },
        else => unreachable,
    }
}

fn eval_index_into(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const literal = try self.eval_expression(ast, node.node_data.lhs, allocator, env);
    const literal_tag = self.object_pool.get_tag(literal);
    if (literal_tag == .runtime_error) {
        return literal;
    }

    defer self.object_pool.free(allocator, literal);

    if (literal_tag != .array and literal_tag != .string and literal_tag != .hash_map) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Cannot Index into type: {s}",
            .{self.object_pool.get_tag_string(literal)},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    const index = try self.eval_expression(ast, node.node_data.rhs, allocator, env);
    const index_tag = self.object_pool.get_tag(index);

    if (index_tag == .runtime_error) {
        return literal;
    }

    defer self.object_pool.free(allocator, index);

    if (index_tag != .integer and (literal_tag == .array or literal_tag == .string)) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Cannot Index into {s} with index of type: {s}",
            .{ self.object_pool.get_tag_string(literal), self.object_pool.get_tag_string(index) },
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }
    switch (literal_tag) {
        .array => {
            const index_value = self.object_pool.get_data(index).integer;
            const array_data = self.object_pool.get_data(literal).array;
            const len = @as(i64, @intCast(array_data.items.len));
            if (index_value >= len or index_value < -(len - 1)) {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Index out of bounds for array with length: {d}. Got {d}",
                    .{ len, index_value },
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            }

            var return_object: ObjectIndex = null_object;
            if (index_value >= 0) {
                const i = @as(usize, @intCast(index_value));
                return_object = array_data.items[i];
            } else {
                const i = @as(usize, @intCast(len + index_value));
                return_object = array_data.items[i];
            }
            self.object_pool.increase_ref(return_object);

            return return_object;
        },
        .string => {
            const index_value = self.object_pool.get_data(index).integer;
            const string_data = self.object_pool.get_data(literal).string_type;
            const len = @as(i64, @intCast(string_data.items.len));
            if (index_value >= len or index_value < -(len - 1)) {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Index out of bounds for string with length: {d}. Got {d}",
                    .{ len, index_value },
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            }

            var return_object: ObjectIndex = null_object;
            if (index_value >= 0) {
                const i = @as(usize, @intCast(index_value));
                const out_str = try std.fmt.allocPrint(allocator, "{c}", .{string_data.items[i]});
                return_object = try self.object_pool.create(allocator, .string, @ptrCast(&out_str));
            } else {
                const i = @as(usize, @intCast(len + index_value));
                const out_str = try std.fmt.allocPrint(allocator, "{c}", .{string_data.items[i]});
                return_object = try self.object_pool.create(allocator, .string, @ptrCast(&out_str));
            }

            return return_object;
        },
        .hash_map => {
            const map = self.object_pool.get_data(literal).hash_map.map;
            if (index_tag != .string and index_tag != .integer and index_tag != .boolean) {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Cannot Index into a hash map with index of type: {s}",
                    .{self.object_pool.get_tag_string(index)},
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            }
            const index_hash = ObjectPool.InternalObject.HashKey.create(self.object_pool.get(index));
            const value = map.get(index_hash) orelse {
                var buffer: [4096]u8 = undefined;
                const str = self.object_pool.ToString(&buffer, index) catch unreachable;
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Provided key {s} does not exist in the hashmap",
                    .{str},
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
            };
            self.object_pool.increase_ref(value);
            return value;
        },
        else => unreachable,
    }
}

fn eval_array_literal(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const elements = try self.eval_expression_list(ast, node.node_data.lhs, node.node_data.rhs, allocator, env);
    defer allocator.free(elements);
    if (elements.len >= 1) {
        const possible_err_tag = self.object_pool.get_tag(elements[elements.len - 1]);
        if (possible_err_tag == .runtime_error) {
            defer allocator.free(elements);
            if (elements.len > 1) {
                for (0..elements.len - 1) |i| {
                    self.object_pool.free(allocator, elements[i]);
                }
            }
            return elements[elements.len - 1];
        }
    }
    const ArrayData = ObjectPool.InternalObject.ArrayType{
        .data = elements,
    };

    return self.object_pool.create(allocator, .array, @ptrCast(&ArrayData));
}

fn eval_function_call(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const function = try self.eval_expression(ast, node.node_data.lhs, allocator, env);
    const function_tag = self.object_pool.get_tag(function);
    if (function_tag == .runtime_error) {
        return function;
    }
    defer self.object_pool.free(allocator, function);

    if (function_tag != .function_expression and function_tag != .builtin) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Cannot call on type: {s}",
            .{self.object_pool.get_tag_string(function)},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    const expression_range = if (node.node_data.rhs != 0)
        ast.extra_data[node.node_data.rhs .. node.node_data.rhs + 2]
    else
        &[_]u32{ 0, 0 };
    const arguments = try self.eval_expression_list(ast, expression_range[0], expression_range[1], allocator, env);
    if (arguments.len >= 1) {
        const possible_err_tag = self.object_pool.get_tag(arguments[arguments.len - 1]);
        if (possible_err_tag == .runtime_error) {
            defer allocator.free(arguments);
            if (arguments.len > 1) {
                for (0..arguments.len - 1) |i| {
                    self.object_pool.free(allocator, arguments[i]);
                }
            }
            return arguments[arguments.len - 1];
        }
    }

    return self.call_function(ast, function, arguments, allocator);
}

fn call_function(
    self: *Evaluator,
    ast: *const Ast,
    func: ObjectIndex,
    args: []const ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    const function_tag = self.object_pool.get_tag(func);
    const function_data = self.object_pool.get_data(func);
    if (function_tag == .builtin) {
        const out_data = function_data.builtin(self, &allocator, args.ptr, @as(u32, @intCast(args.len)));
        defer allocator.free(args);
        for (0..args.len) |i| {
            defer self.object_pool.free(allocator, args[i]);
        }
        return out_data;
    }

    const num_expected_params = function_data.function.parameters_len;
    if (num_expected_params != args.len) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Calling function with expected: {d} parameters. Got {d} arguments.",
            .{ num_expected_params, args.len },
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    }

    const function_body_env = self.get_function_body_env(ast, function_data, args, allocator) catch |err| switch (err) {
        Environment.Error.ExceedingMaxDepth => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Exceeded depth of {d} in function calls.",
                .{Environment.MaxEnvDepth},
            );
            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
        },
        Environment.Error.ConstVariableModification => unreachable,
        Environment.Error.NonExistantVariable => unreachable,
        Environment.Error.VariableAlreadyInitialised => unreachable,
        else => |overflow| return overflow,
    } orelse {
        const output = try std.fmt.allocPrint(
            allocator,
            "Function has parameters with duplicate names. Cannot call this function",
            .{},
        );
        return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
    };

    defer allocator.free(args);
    for (0..args.len) |i| {
        defer self.object_pool.free(allocator, args[i]);
    }

    const out = try self.evaluate_block(
        ast,
        function_data.function.block_ptr[0..function_data.function.block_len],
        allocator,
        function_body_env,
        false,
    );
    const out_type = self.object_pool.get_tag(out);
    const out_data = self.object_pool.get_data(out);
    switch (out_type) {
        .return_expression => {
            defer self.object_pool.free(allocator, out);
            const return_value_type = self.object_pool.get_tag(out_data.return_value);
            switch (return_value_type) {
                .function_expression => {
                    self.environment_pool.add_child(function_data.function.env, allocator, function_body_env) catch |err| switch (err) {
                        EnvironmentPool.EnvPoolError.AccessingFreeEnv => {
                            const output = try std.fmt.allocPrint(
                                allocator,
                                "Something has gone horribly wrong. Accessing environment that is not initialized",
                                .{},
                            );
                            return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
                        },
                        else => |overflow| return overflow,
                    };
                    // try function_data.function.env.add_child(allocator, function_body_env);
                    return out_data.return_value;
                },
                else => {
                    defer self.environment_pool.free_env(allocator, function_body_env, &self.object_pool);
                    return out_data.return_value;
                },
            }
        },
        .function_expression => {
            self.environment_pool.add_child(function_data.function.env, allocator, function_body_env) catch |err| switch (err) {
                EnvironmentPool.EnvPoolError.AccessingFreeEnv => {
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Something has gone horribly wrong. Accessing environment that is not initialized",
                        .{},
                    );
                    return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
                },
                else => |overflow| return overflow,
            };
            // try function_data.function.env.add_child(allocator, function_body_env);
            return out;
        },
        else => {
            defer self.environment_pool.free_env(allocator, function_body_env, &self.object_pool);
            return out;
        },
    }
}

fn get_function_body_env(
    self: *Evaluator,
    ast: *const Ast,
    func: ObjectPool.InternalObject.ObjectData,
    args: []const ObjectIndex,
    allocator: Allocator,
) !?EnvironmentIndex {
    const env = try self.environment_pool.create_env(allocator, func.function.env);
    if (func.function.parameters_len == 0) {
        return env;
    }
    _ = ast;

    for (func.function.parameters_ptr[0..func.function.parameters_len], 0..args.len) |param, arg| {
        self.environment_pool.create_variable(env, allocator, param, args[arg], .constant) catch |err| switch (err) {
            Environment.Error.VariableAlreadyInitialised => {
                return null;
            },
            EnvironmentPool.EnvPoolError.AccessingFreeEnv => return null,
            Environment.Error.NonExistantVariable => unreachable,
            Environment.Error.ConstVariableModification => unreachable,
            else => |overflow| return overflow,
        };
        self.object_pool.increase_ref(args[arg]);
    }
    return env;
}

fn eval_expression_list(
    self: *Evaluator,
    ast: *const Ast,
    expression_start: Ast.Node.NodeIndex,
    expression_end: Ast.Node.NodeIndex,
    allocator: Allocator,
    env: EnvironmentIndex,
) ![]ObjectIndex {
    var arguments = std.ArrayList(ObjectIndex).init(allocator);
    defer arguments.deinit();
    if (expression_end == 0) {
        return arguments.toOwnedSlice();
    }
    const expressions = ast.extra_data[expression_start..expression_end];
    for (expressions) |e| {
        const obj_pos = try self.eval_expression(ast, e, allocator, env);
        try arguments.append(obj_pos);
        if (self.object_pool.get_tag(obj_pos) == .runtime_error) {
            return arguments.toOwnedSlice();
        }
    }
    return arguments.toOwnedSlice();
}

fn eval_function_expression(
    self: *Evaluator,
    ast: *const Ast,
    node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    var parameters = std.ArrayList(Ast.Node.NodeIndex).init(allocator);
    defer parameters.deinit();
    if (node.node_data.lhs != 0) {
        const parameters_node = ast.nodes.get(node.node_data.lhs);
        std.debug.assert(parameters_node.tag == .FUNCTION_PARAMETER_BLOCK);
        const num_nodes = parameters_node.node_data.rhs - parameters_node.node_data.lhs;
        try parameters.ensureUnusedCapacity(num_nodes);
        for (parameters_node.node_data.lhs..parameters_node.node_data.rhs) |i| {
            const hash = ast.nodes.get(i).node_data.lhs;
            try parameters.append(@as(u32, @intCast(hash)));
        }
    }

    const block_node = ast.nodes.get(node.node_data.rhs);
    const statements = ast.extra_data[block_node.node_data.lhs..block_node.node_data.rhs];
    std.debug.assert(block_node.tag == .BLOCK);
    var block_statements = try std.ArrayList(Ast.Node.NodeIndex).initCapacity(
        allocator,
        block_node.node_data.rhs - block_node.node_data.lhs,
    );
    defer block_statements.deinit();
    try block_statements.appendSlice(statements);
    const function_declaration = ObjectPool.InternalObject.FunctionData{
        .parameters = try parameters.toOwnedSlice(),
        .block = try block_statements.toOwnedSlice(),
        .env = env,
    };
    return self.object_pool.create(allocator, .function_expression, @ptrCast(&function_declaration));
}

fn eval_prefix_operation(
    self: *Evaluator,
    tag: Ast.Node.Tag,
    value_pos: ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    const value_tag = self.object_pool.get_tag(value_pos);
    const value_data = self.object_pool.get_data(value_pos);
    switch (tag) {
        .BOOL_NOT => {
            defer self.object_pool.free(allocator, value_pos);
            switch (value_tag) {
                .integer => {
                    const result = value_data.integer == 0;
                    if (result) {
                        return true_object;
                    } else {
                        return false_object;
                    }
                },
                .boolean => {
                    if (value_data.boolean) {
                        return false_object;
                    } else {
                        return true_object;
                    }
                },
                inline else => {
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Unknown Operation: !<{s}>",
                        .{self.object_pool.get_tag_string(value_pos)},
                    );
                    return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
                },
            }
        },
        .NEGATION => {
            switch (value_tag) {
                .integer => {
                    self.object_pool.object_pool.items(.data)[value_pos].integer *= -1;
                    return value_pos;
                },
                inline else => {
                    defer self.object_pool.free(allocator, value_pos);
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Unknown Operation: -<{s}>",
                        .{self.object_pool.get_tag_string(value_pos)},
                    );
                    return self.object_pool.create(allocator, .runtime_error, @ptrCast(&output));
                },
            }
        },
        inline else => unreachable,
    }
    return null_object;
}

fn eval_infix_operation(
    self: *Evaluator,
    ast: *const Ast,
    node: *const Ast.Node,
    left: ObjectIndex,
    right: ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    const left_tag = self.object_pool.get_tag(left);
    const right_tag = self.object_pool.get_tag(right);

    if (!(left_tag == .integer and right_tag == .integer) and !(left_tag == .string and right_tag == .string)) {
        defer self.object_pool.free(allocator, left);
        defer self.object_pool.free(allocator, right);
        const outstr = try std.fmt.allocPrint(
            allocator,
            "Unknown Operation: <{s}> {s} <{s}>",
            .{
                self.object_pool.get_tag_string(left),
                get_token_literal(ast, node.main_token),
                self.object_pool.get_tag_string(right),
            },
        );
        return try self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
    }

    switch (left_tag) {
        .integer => return self.eval_intint_infix_operation(node, left, right, allocator),

        .string => return self.eval_string_infix_operation(ast, node, left, right, allocator),
        else => unreachable,
    }
}

fn eval_string_infix_operation(
    self: *Evaluator,
    ast: *const Ast,
    node: *const Ast.Node,
    left: ObjectIndex,
    right: ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    defer self.object_pool.free(allocator, left);
    defer self.object_pool.free(allocator, right);
    const left_data = self.object_pool.get_data(left).string_type;
    const right_data = self.object_pool.get_data(right).string_type;
    switch (node.tag) {
        .ADDITION => {
            const outstr = try std.fmt.allocPrint(
                allocator,
                "{s}{s}",
                .{ left_data.items, right_data.items },
            );
            return self.object_pool.create(allocator, .string, @ptrCast(&outstr));
        },
        inline else => {
            const outstr = try std.fmt.allocPrint(
                allocator,
                "Unknown Operation: <{s}> {s} <{s}>",
                .{
                    self.object_pool.get_tag_string(left),
                    get_token_literal(ast, node.main_token),
                    self.object_pool.get_tag_string(right),
                },
            );
            return try self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
        },
    }
}

fn eval_intint_infix_operation(
    self: *Evaluator,
    node: *const Ast.Node,
    left: ObjectIndex,
    right: ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    defer self.object_pool.free(allocator, left);
    defer self.object_pool.free(allocator, right);
    const left_data = self.object_pool.get_data(left);
    const right_data = self.object_pool.get_data(right);
    var result: bool = false;
    switch (node.tag) {
        .LESS_THAN => {
            result = left_data.integer < right_data.integer;
        },
        .GREATER_THAN => {
            result = left_data.integer > right_data.integer;
        },
        .LESS_THAN_EQUAL => {
            result = left_data.integer <= right_data.integer;
        },
        .GREATER_THAN_EQUAL => {
            result = left_data.integer >= right_data.integer;
        },
        .ADDITION => {
            const res: i64 = left_data.integer + right_data.integer;
            return self.object_pool.create(allocator, .integer, @ptrCast(&res));
        },
        .SUBTRACTION => {
            const res: i64 = left_data.integer - right_data.integer;
            return self.object_pool.create(allocator, .integer, @ptrCast(&res));
        },
        .MULTIPLY => {
            const res: i64 = left_data.integer * right_data.integer;
            return self.object_pool.create(allocator, .integer, @ptrCast(&res));
        },
        .DIVIDE => {
            const res: i64 = @divFloor(left_data.integer, right_data.integer);
            return self.object_pool.create(allocator, .integer, @ptrCast(&res));
        },
        inline else => unreachable,
    }
    if (result) {
        return true_object;
    } else {
        return false_object;
    }
}

fn eval_if_expression(
    self: *Evaluator,
    ast: *const Ast,
    ast_node: Ast.Node,
    allocator: Allocator,
    env: EnvironmentIndex,
) Error!ObjectIndex {
    const condition_pos = try self.eval_expression(ast, ast_node.node_data.lhs, allocator, env);
    const condition_type = self.object_pool.get_tag(condition_pos);
    const condition_data = self.object_pool.get_data(condition_pos);
    var result: bool = false;
    switch (condition_type) {
        .integer => result = condition_data.integer != 0,
        .boolean => result = condition_data.boolean,
        .runtime_error => return condition_pos,
        else => unreachable,
    }
    defer self.object_pool.free(allocator, condition_pos);

    switch (ast_node.tag) {
        .NAKED_IF => {
            if (result) {
                const block_node_tag = ast.nodes.items(.tag)[ast_node.node_data.rhs];
                std.debug.assert(block_node_tag == .BLOCK);
                const block_node_data = ast.nodes.items(.node_data)[ast_node.node_data.rhs];
                const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
                return self.evaluate_block(ast, statements, allocator, env, true);
            } else {
                return null_object;
            }
        },
        .IF_ELSE => {
            const blocks = ast.extra_data[ast_node.node_data.rhs .. ast_node.node_data.rhs + 2];
            var block_node_data: Ast.Node.NodeData = undefined;
            if (result) {
                const block_node_tag = ast.nodes.items(.tag)[blocks[0]];
                std.debug.assert(block_node_tag == .BLOCK);
                block_node_data = ast.nodes.items(.node_data)[blocks[0]];
            } else {
                const block_node_tag = ast.nodes.items(.tag)[blocks[1]];
                std.debug.assert(block_node_tag == .BLOCK);
                block_node_data = ast.nodes.items(.node_data)[blocks[1]];
            }
            const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
            return self.evaluate_block(ast, statements, allocator, env, true);
        },
        inline else => unreachable,
    }
}

fn eval_intboolean_infix_operation(
    self: *Evaluator,
    ast: *const Ast,
    node: *const Ast.Node,
    left: ObjectIndex,
    right: ObjectIndex,
    allocator: Allocator,
) Error!ObjectIndex {
    defer self.object_pool.free(allocator, left);
    defer self.object_pool.free(allocator, right);
    var result: bool = false;

    const left_tag = self.object_pool.get_tag(left);
    const left_data = self.object_pool.get_data(left);
    const right_tag = self.object_pool.get_tag(right);
    const right_data = self.object_pool.get_data(right);
    if (left_tag == .integer and right_tag == .integer) {
        switch (node.tag) {
            .DOUBLE_EQUAL => result = right_data.integer == left_data.integer,
            .NOT_EQUAL => result = right_data.integer != left_data.integer,
            inline else => unreachable,
        }
    } else {
        const left_bool = switch (left_tag) {
            .integer => left_data.integer != 0,
            .boolean => left_data.boolean,
            else => {
                const outstr = try std.fmt.allocPrint(
                    allocator,
                    "Unknown Operation: <{s}> {s} <{s}>",
                    .{
                        self.object_pool.get_tag_string(left),
                        get_token_literal(ast, node.main_token),
                        self.object_pool.get_tag_string(right),
                    },
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
            },
        };
        const right_bool = switch (right_tag) {
            .integer => right_data.integer != 0,
            .boolean => right_data.boolean,
            else => {
                const outstr = try std.fmt.allocPrint(
                    allocator,
                    "Unknown Operation: <{s}> {s} <{s}>",
                    .{
                        self.object_pool.get_tag_string(left),
                        get_token_literal(ast, node.main_token),
                        self.object_pool.get_tag_string(right),
                    },
                );
                return self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
            },
        };
        switch (node.tag) {
            .DOUBLE_EQUAL => result = left_bool == right_bool,
            .NOT_EQUAL => result = left_bool != right_bool,
            inline else => unreachable,
        }
    }

    if (result) {
        return true_object;
    } else {
        return false_object;
    }
}

pub fn convert_ast_to_string(ast: *const Ast, root_node: usize, list: *std.ArrayList(u8)) !void {
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

fn get_token_literal(ast: *const Ast, tok_loc: Ast.TokenArrayIndex) []const u8 {
    const tok = ast.tokens.get(tok_loc);
    return ast.source_buffer[tok.start..tok.end];
}

const test_struct = struct {
    source: [:0]const u8,
    output: []const u8,
};

test "evaluate_booleans" {
    const tests = [_]test_struct{
        .{ .source = "true", .output = "true" },
        .{ .source = "false", .output = "false" },
        .{ .source = "1 < 2", .output = "true" },
        .{ .source = "1 > 2", .output = "false" },
        .{ .source = "1 < 1", .output = "false" },
        .{ .source = "1 > 1", .output = "false" },
        .{ .source = "1 == 1", .output = "true" },
        .{ .source = "1 != 1", .output = "false" },
        .{ .source = "1 == 2", .output = "false" },
        .{ .source = "1 != 2", .output = "true" },
        .{ .source = "1 != true", .output = "false" },
        .{ .source = "true == true", .output = "true" },
        .{ .source = "false == false", .output = "true" },
        .{ .source = "true == false", .output = "false" },
        .{ .source = "true != false", .output = "true" },
        .{ .source = "false != true", .output = "true" },
        .{ .source = "(1 < 2) == true", .output = "true" },
        .{ .source = "(1 < 2) == false", .output = "false" },
        .{ .source = "(1 > 2) == true", .output = "false" },
        .{ .source = "(1 > 2) == false", .output = "true" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_prefix_not" {
    const tests = [_]test_struct{
        .{
            .source = "!5",
            .output = "false",
        },
        .{
            .source = "!false",
            .output = "true",
        },
        .{
            .source = "!!true",
            .output = "true",
        },
        .{
            .source = "!!5",
            .output = "true",
        },
        .{
            .source = "!!!5",
            .output = "false",
        },
    };

    try eval_tests(&tests, false);
}

test "evaluate_integer_expressions" {
    const tests = [_]test_struct{
        .{ .source = "5", .output = "5" },
        .{ .source = "10", .output = "10" },
        .{ .source = "-5", .output = "-5" },
        .{ .source = "-10", .output = "-10" },
        .{ .source = "5 + 5 + 5 + 5 - 10", .output = "10" },
        .{ .source = "2 * 2 * 2 * 2 * 2", .output = "32" },
        .{ .source = "-50 + 100 + -50", .output = "0" },
        .{ .source = "5 * 2 + 10", .output = "20" },
        .{ .source = "5 + 2 * 10", .output = "25" },
        .{ .source = "20 + 2 * -10", .output = "0" },
        .{ .source = "50 / 2 * 2 + 10", .output = "60" },
        .{ .source = "2 * (5 + 10)", .output = "30" },
        .{ .source = "3 * 3 * 3 + 10", .output = "37" },
        .{ .source = "3 * (3 * 3) + 10", .output = "37" },
        .{ .source = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .output = "50" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_string_expressions" {
    const tests = [_]test_struct{
        .{ .source = "\"foobar\"", .output = "foobar" },
        .{ .source = "const a = \"foobar\"; a;", .output = "foobar" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a + b;", .output = "foobar" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a + \"\" + b;", .output = "foobar" },
        .{
            .source = "const a = \"foo\"; const b = \"bar\"; var c = fn(x) { return x + \"baz\";}; c(a) + \" \" +c(b);",
            .output = "foobaz barbaz",
        },
        .{
            .source =
            \\  const fn_call = fn(x) {
            \\      const b = fn(y) {
            \\          x + y
            \\      };
            \\      return b;
            \\  };
            \\  const add_foo = fn_call("foo");
            \\  add_foo("bar");
            ,
            .output = "foobar",
        },
        .{ .source = "\"foobar\"[-1]", .output = "r" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_builtin_len" {
    const tests = [_]test_struct{
        .{ .source = "len(\"\")", .output = "0" },
        .{ .source = "len(\"test\")", .output = "4" },
        .{ .source = "len(\"hello world!\")", .output = "12" },
        .{ .source = "len([1, 2, 3])", .output = "3" },
        .{ .source = "const a = [1, 2, 3]; len(a);", .output = "3" },
        .{ .source = "const a = \"test\";len(a)", .output = "4" },
        .{ .source = "len(1)", .output = "Argument of type: INTEGER is not supported by builtin 'len'" },
        .{ .source = "len(\"foo\", \"bar\")", .output = "Wrong number of arguments to builin function len. Expected 1 got 2" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_if_else_expressions" {
    const tests = [_]test_struct{
        .{ .source = "if (true) { 10 }", .output = "10" },
        .{ .source = "if (false) { 10 }", .output = "null" },
        .{ .source = "if (1) { 10 }", .output = "10" },
        .{ .source = "if (1 < 2) { 10 }", .output = "10" },
        .{ .source = "if (1 > 2) { 10 }", .output = "null" },
        .{ .source = "if (1 > 2) { 10 } else { 20 }", .output = "20" },
        .{ .source = "if (1 < 2) { 10 } else { 20 }", .output = "10" },
        .{ .source = "if (1 < 2) { if(1 < 2) { return 10; } return 1; } else { 20 }", .output = "10" },
        .{ .source = "if (1 < 2) { if ( 3 < 2 ) { 30 } else{ if (1 < 2 * 5 + 3) { 10 } }} else { 20 }", .output = "10" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_arrays" {
    const tests = [_]test_struct{
        .{ .source = "[1, 2, 3]", .output = "[1, 2, 3, ]" },
        .{ .source = "[1, 2, 3]", .output = "[1, 2, 3, ]" },
        .{ .source = "[1, 2, 3][0]", .output = "1" },
        .{ .source = "[1, \"two\", 3][1]", .output = "two" },
        .{ .source = "const a = [1, 2, 3]; a;", .output = "[1, 2, 3, ]" },
        .{ .source = "const a = [1, 2, 3]; a[-1];", .output = "3" },
        .{ .source = "const last = fn(x) { return x[-1] }; last([1, 2, 3]);", .output = "3" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_return_statements" {
    const tests = [_]test_struct{
        .{ .source = "return 10", .output = "10" },
        .{ .source = "5; return 10", .output = "10" },
        .{ .source = "return 10 * 10; 5;", .output = "100" },
        .{ .source = "9; return 10 * 10; return 5;", .output = "100" },
        .{ .source = "if (1 < 2) { return 10 * 5; }", .output = "50" },
        .{ .source = "10 * if (1 < 2) { 10 * 5; }", .output = "500" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_hash_map" {
    const tests = [_]test_struct{
        .{
            .source = "const a = {false: 1 < 2, 1: \"o\" + \"n\" + \"e\", \"two\": 5 * 4 - 3 * 6}; a[false]",
            .output = "true",
        },
        .{
            .source = "const a = {false: 1 < 2, 1: \"o\" + \"n\" + \"e\", \"two\": 5 * 4 - 3 * 6}; a[1]",
            .output = "one",
        },
        .{
            .source = "const a = {false: 1 < 2, 1: \"o\" + \"n\" + \"e\", \"two\": 5 * 4 - 3 * 6}; a[\"two\"]",
            .output = "2",
        },
        .{
            .source = "const a = {false: 1 < 2, 1: \"o\" + \"n\" + \"e\", \"two\": 5 * 4 - 3 * 6, 3: fn(a, b) { return a + b;}}; a[3](2, 4);",
            .output = "6",
        },
    };

    try eval_tests(&tests, false);
}

test "evaluate_while_loops" {
    const tests = [_]test_struct{
        .{ .source = "while (false) { 10; }", .output = "null" },
        .{ .source = "var a = 0; while (a < 10) { a = a + 1; } a", .output = "10" },
        .{
            .source =
            \\  const fn_call = fn(x) {
            \\      const b = fn(y) {
            \\          var a = y;
            \\          while (a < x ) {
            \\              a = a + 1;
            \\              if ( a >= 10 ) {
            \\                  break;
            \\              }
            \\          }
            \\          return a;
            \\      };
            \\      return b;
            \\  };
            \\  const t = fn_call(20);
            \\  t(10);
            ,
            .output = "11",
        },
    };

    try eval_tests(&tests, false);
}

test "evaluate_function_expressions" {
    const tests = [_]test_struct{
        .{ .source = "const a = fn(x, y) { x + y};", .output = "null" },
        .{ .source = "fn(x, y) { x + y}(1, 2)", .output = "3" },
        .{ .source = "const a = fn(x, y) { x + y }; a(2, 4);", .output = "6" },
        .{ .source = "const call_fn = fn(x, y) { x(y) }; call_fn(fn(x) { return 2 * x; }, 4);", .output = "8" },
        .{
            .source = "const fn_call = fn(x) { const b = fn(y) { x + y}; return b; }; const a = fn_call(2); a(3)",
            .output = "5",
        },
        .{
            .source =
            \\  const fn_call = fn(x) {
            \\      const b = fn(y) {
            \\          x + y
            \\      };
            \\      return b;
            \\  };
            \\  const a = fn_call(2);
            \\  const b = fn_call(3);
            \\  a(3);
            \\  b(7);
            ,
            .output = "10",
        },
        .{
            .source =
            \\  const a = 10;
            \\  const fn_call = fn(x) {
            \\      const b = fn(y) {
            \\          a + x + y
            \\      };
            \\      return b;
            \\  };
            \\  const add_two = fn_call(2);
            \\  const add_three = fn_call(3);
            \\  add_three(3);
            \\  add_three(7);
            ,
            .output = "20",
        },
        .{ .source = "const add = fn(x, y) { return x + y; }; add( 5 * 5, add(5, 5))", .output = "35" },
        .{ .source = "const b = fn() { 10; }; const add = fn(a, b) { a() + b }; add(b, 10);", .output = "20" },
    };

    try eval_tests(&tests, false);
}

test "evaluate_identifiers" {
    const tests = [_]test_struct{
        .{ .source = "const a = 10; a;", .output = "10" },
        .{ .source = "const a = 10; const b = 10; a;", .output = "10" },
        .{ .source = "const a = 10; const b = 11; a; b;", .output = "11" },
        .{ .source = "const a = 10; const b = 11; const c = a * b; b + c;", .output = "121" },
        .{ .source = "const a = 2 * 2; const b = a + 3; if ( a < b ) { a; } else { b; } ", .output = "4" },
        .{
            .source = "const a = 2 * 2; const b = a + 3; const c = if ( a < b ) { a + 3; } else { b; }; c; ",
            .output = "7",
        },
        .{
            .source = "var a = 2 * 2; const b = a + 3; if ( a < b ) { a = 5; } else { a = 2; }; a; ",
            .output = "5",
        },
        .{
            .source = "var a = 2 * 2; const b = a + 3; const c = if ( a < b ) { return a + 5; } else { return true; }; c; ",
            .output = "9",
        },
    };

    try eval_tests(&tests, false);
}

test "evaluate_errors" {
    const tests = [_]test_struct{
        .{ .source = "5 + true", .output = "Unknown Operation: <INTEGER> + <BOOLEAN>" },
        .{ .source = "true - true", .output = "Unknown Operation: <BOOLEAN> - <BOOLEAN>" },
        .{ .source = "-true", .output = "Unknown Operation: -<BOOLEAN>" },
        .{ .source = "if ( 1 < 10 ) { return false + 5; }", .output = "Unknown Operation: <BOOLEAN> + <INTEGER>" },
        .{ .source = "if ( 1 + true < 10 ) { return false + 5; }", .output = "Unknown Operation: <INTEGER> + <BOOLEAN>" },
        .{ .source = "if ( 10 > 1 + true ) { return false + 5; }", .output = "Unknown Operation: <INTEGER> + <BOOLEAN>" },
        .{ .source = "5 + 5; 5 + true; if ( 1 < 10 ) { return false + 5; }", .output = "Unknown Operation: <INTEGER> + <BOOLEAN>" },
    };

    try eval_tests(&tests, false);
}

fn eval_tests(tests: []const test_struct, enable_debug_print: bool) !void {
    var buffer: [1024]u8 = undefined;
    for (tests) |t| {
        if (enable_debug_print) {
            std.debug.print("Testing: Source: {s}\n", .{t.source});
        }
        var identifier_map = SymbolTable.init();
        defer identifier_map.deinit(testing.allocator);
        // var env = try Environment.Create(testing.allocator);
        var eval = try Evaluator.init(testing.allocator, global_env, &identifier_map);
        defer {
            // env.deinit(testing.allocator, &eval.object_pool);
            eval.deinit(testing.allocator);
        }
        var ast = try Parser.parse_program(t.source, testing.allocator, &identifier_map);
        defer ast.deinit(testing.allocator);

        const output = try eval.evaluate_program(&ast, 0, testing.allocator, global_env);
        const outstr = try eval.object_pool.ToString(&buffer, output);
        if (enable_debug_print) {
            std.debug.print("Expected: {s} \t Got: {s}\n", .{ t.output, outstr });
            try Parser.print_parser_errors_to_stderr(&ast);
            identifier_map.print_env_hashmap_stderr();
            eval.environment_pool.print_to_stderr();
            try eval.object_pool.print_object_pool_to_stderr();
            for (0..ast.nodes.len) |i| {
                const n = ast.nodes.get(i);
                std.debug.print("Nodes({d}): {any}\r\n", .{ i, n });
            }

            std.debug.print("Extra Data: ", .{});
            for (0..ast.extra_data.len) |i| {
                const n = ast.extra_data[i];
                std.debug.print("{d}, ", .{n});
            }

            std.debug.print("\n", .{});
        }
        try testing.expectEqualSlices(u8, t.output, outstr);
        eval.object_pool.free(testing.allocator, output);
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const token = @import("token.zig");
const Environment = @import("environment.zig");
const Ast = @import("ast.zig");
const Parser = @import("parser.zig");
const ObjectPool = @import("object.zig");
const ObjectTypes = ObjectPool.ObjectTypes;
const ObjectIndex = ObjectPool.ObjectIndex;
const null_object = ObjectPool.null_object;
const true_object = ObjectPool.true_object;
const false_object = ObjectPool.false_object;
const break_object = ObjectPool.break_object;
const continue_object = ObjectPool.continue_object;
const SymbolTable = @import("symbol_table.zig");
const Builtins = @import("builtins.zig");
const EnvironmentPool = @import("environment_pool.zig");
const EnvironmentIndex = EnvironmentPool.EnvironmentIndex;
const global_env = EnvironmentPool.global_env;
