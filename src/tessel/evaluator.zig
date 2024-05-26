pub const Evaluator = @This();

pub const Error = error{ReferencingNodeZero} || object.Error;

pub fn evaluate_program(ast: *const Ast, allocator: Allocator, env: *Environment) Error!Object {
    const root_node = ast.nodes.get(0);
    const statements = ast.extra_data[root_node.node_data.lhs..root_node.node_data.rhs];

    return evaluate_program_statements(ast, statements, allocator, env);
}

fn evaluate_program_statements(ast: *const Ast, statemnts: []u32, allocator: Allocator, env: *Environment) Error!Object {
    for (statemnts, 0..) |s, i| {
        const obj = eval_statement(ast, s, allocator, env) catch |err| switch (err) {
            object.Error.InactiveField => {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Something has gone terribly wrong. Refer to the GLWTS licence.",
                    .{},
                );
                return Object.Create(.runtime_error, allocator, @ptrCast(&output));
            },
            else => |overflow| return overflow,
        };

        switch (obj) {
            .return_expression => {
                defer obj.deinit(allocator);
                return obj.return_expression.value;
            },
            .runtime_error => {
                return obj;
            },
            else => {},
        }

        if (i == statemnts.len - 1) {
            return obj;
        }
        obj.deinit(allocator);
    }
    return .null;
}

fn evaluate_block(ast: *const Ast, statemnts: []u32, allocator: Allocator, env: *Environment) Error!Object {
    for (statemnts, 0..) |s, i| {
        const obj = eval_statement(ast, s, allocator, env) catch |err| switch (err) {
            object.Error.InactiveField => {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Something has gone terribly wrong. Refer to the GLWTS licence.",
                    .{},
                );
                return Object.Create(.runtime_error, allocator, @ptrCast(&output));
            },
            else => |overflow| return overflow,
        };

        switch (obj) {
            .return_expression, .runtime_error => {
                return obj;
            },
            else => {},
        }

        if (i == statemnts.len - 1) {
            return obj;
        }
        obj.deinit(allocator);
    }
    return .null;
}

fn eval_statement(ast: *const Ast, node: Ast.Node.NodeIndex, allocator: Allocator, env: *Environment) !Object {
    if (node >= ast.nodes.len) {
        return .null;
    }

    if (node == 0) {
        return Error.ReferencingNodeZero;
    }

    const ast_node = ast.nodes.get(node);
    switch (ast_node.tag) {
        .EXPRESSION_STATEMENT => {
            return eval_expression(ast, ast_node.node_data.lhs, allocator, env);
        },
        .RETURN_STATEMENT => {
            const value = try eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            const obj = Object.Create(.return_expression, allocator, @ptrCast(&value));
            return obj;
        },
        .VAR_STATEMENT => {
            const value = try eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            if (value.getEnumTag() == .runtime_error) {
                return value;
            }
            const var_tag = get_token_literal(ast, ast_node.main_token);
            var tag: Environment.StorageType.Tag = .constant;
            if (std.mem.eql(u8, var_tag, "var")) {
                tag = .variable;
            }
            const ident_token = ast.nodes.get(ast_node.node_data.lhs).main_token;
            var actual_value: Object = undefined;
            switch (value) {
                .return_expression => {
                    actual_value = value.return_expression.value;
                    value.deinit(allocator);
                },
                else => {
                    actual_value = value;
                },
            }
            defer actual_value.deinit(allocator);
            _ = env.create_variable(allocator, get_token_literal(ast, ident_token), actual_value, tag) catch |err| switch (err) {
                Environment.Error.VariableAlreadyInitialised => {
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Identifier \"{s}\" has already been initialised",
                        .{get_token_literal(ast, ident_token)},
                    );
                    return Object.Create(.runtime_error, allocator, @ptrCast(&output));
                },
                Environment.Error.NonExistantVariable => unreachable,
                Environment.Error.ConstVariableModification => unreachable,
                Environment.Error.ExceedingMaxDepth => unreachable,
                else => |overflow| return overflow,
            };
            return .null;
        },
        .ASSIGNMENT_STATEMENT => {
            const value = try eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            var actual_value: Object = undefined;
            switch (value) {
                .return_expression => {
                    actual_value = value.return_expression.value;
                    value.deinit(allocator);
                },
                else => {
                    actual_value = value;
                },
            }
            defer actual_value.deinit(allocator);
            const ident_token = ast.nodes.get(ast_node.node_data.lhs).main_token;
            env.update_variable(allocator, get_token_literal(ast, ident_token), actual_value) catch |err| switch (err) {
                Environment.Error.NonExistantVariable => {
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Identifier \"{s}\" does not exist. Cannot assign anything to it.",
                        .{get_token_literal(ast, ident_token)},
                    );
                    return Object.Create(.runtime_error, allocator, @ptrCast(&output));
                },
                Environment.Error.ConstVariableModification => {
                    const output = try std.fmt.allocPrint(
                        allocator,
                        "Identifier \"{s}\" is declared as a constant and cannot be modified",
                        .{get_token_literal(ast, ident_token)},
                    );
                    return Object.Create(.runtime_error, allocator, @ptrCast(&output));
                },
                Environment.Error.VariableAlreadyInitialised => unreachable,
                Environment.Error.ExceedingMaxDepth => unreachable,
                else => |overflow| return overflow,
            };

            return .null;
        },
        else => return .null,
    }
}

fn eval_expression(ast: *const Ast, node: Ast.Node.NodeIndex, allocator: Allocator, env: *Environment) Error!Object {
    if (node >= ast.nodes.len) {
        return .null;
    }

    if (node == 0) {
        return Error.ReferencingNodeZero;
    }

    // std.debug.print("Env#eval_expression: contains a: {any}\n", .{env.memory.contains("a")});
    const ast_node = ast.nodes.get(node);
    var obj: Object = undefined;
    switch (ast_node.tag) {
        .INTEGER_LITERAL => {
            obj = try Object.Create(.integer, allocator, @ptrCast(&ast.integer_literals[ast_node.node_data.lhs]));
        },
        .BOOLEAN_LITERAL => {
            const value = ast_node.node_data.lhs == 1;
            obj = try Object.Create(.boolean, allocator, @ptrCast(&value));
        },
        .IDENTIFIER => {
            const ident_name = get_token_literal(ast, ast_node.main_token);
            const value = try env.get_object(ident_name, allocator);
            if (value.getEnumTag() == .null) {
                const output = try std.fmt.allocPrint(
                    allocator,
                    "Identifier not found: {s}",
                    .{ident_name},
                );
                obj = try Object.Create(.runtime_error, allocator, @ptrCast(&output));
            } else {
                return value;
            }
        },
        .NEGATION, .BOOL_NOT => {
            const left = try eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (left.getEnumTag() == .runtime_error) return left;
            obj = try eval_prefix_operation(ast_node.tag, left, allocator);
        },
        .DOUBLE_EQUAL,
        .NOT_EQUAL,
        => {
            const left = try eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (left.getEnumTag() == .runtime_error) return left;
            const right = try eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            if (right.getEnumTag() == .runtime_error) {
                defer left.deinit(allocator);
                return right;
            }
            obj = try eval_intboolean_infix_operation(ast, &ast_node, left, right, allocator);
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
            const left = try eval_expression(ast, ast_node.node_data.lhs, allocator, env);
            if (left.getEnumTag() == .runtime_error) return left;
            const right = try eval_expression(ast, ast_node.node_data.rhs, allocator, env);
            if (right.getEnumTag() == .runtime_error) {
                defer left.deinit(allocator);
                return right;
            }

            obj = try eval_intint_infix_operation(ast, &ast_node, left, right, allocator);
        },
        .NAKED_IF => {
            obj = try eval_if_expression(ast, ast_node, allocator, env);
        },
        .IF_ELSE => {
            obj = try eval_if_expression(ast, ast_node, allocator, env);
        },
        .FUNCTION_EXPRESSION => {
            obj = try eval_function_expression(ast, ast_node, allocator, env);
        },
        .FUNCTION_CALL => {
            obj = try eval_function_call(ast, ast_node, allocator, env);
        },
        else => {
            return .null;
        },
    }

    return obj;
}

fn eval_function_call(ast: *const Ast, node: Ast.Node, allocator: Allocator, env: *Environment) Error!Object {
    const function_node = ast.nodes.get(node.node_data.lhs);
    const function = try eval_expression(ast, node.node_data.lhs, allocator, env);
    defer function.deinit(allocator);
    if (function.getEnumTag() != .function_expression) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Cannot call on type: {s}",
            .{@tagName(function_node.tag)},
        );
        return Object.Create(.runtime_error, allocator, @ptrCast(&output));
    }
    // std.debug.print("Function call\n: {any}\n", .{function.function_expression});
    // std.debug.print("Function calling node\n: {any}\n", .{node});
    const arguments = try eval_function_arguments(ast, node.node_data.rhs, allocator, env);
    if (arguments.len >= 1) {
        if (arguments[arguments.len - 1].getEnumTag() == .runtime_error) {
            defer allocator.free(arguments);
            if (arguments.len > 1) {
                for (0..arguments.len - 1) |i| {
                    arguments[i].deinit(allocator);
                }
            }
            return arguments[arguments.len - 1];
        }
    }

    return call_function(ast, function, arguments, allocator);
}

fn call_function(ast: *const Ast, func: Object, args: []const Object, allocator: Allocator) Error!Object {
    const function_expression: *ObjectStructures.FunctionExpressionType = try func.get(.function_expression);
    const num_expected_params = function_expression.value.parameters.len;
    if (num_expected_params != args.len) {
        const output = try std.fmt.allocPrint(
            allocator,
            "Calling function with expected: {d} parameters. Got {d}",
            .{ num_expected_params, args.len },
        );
        return Object.Create(.runtime_error, allocator, @ptrCast(&output));
    }

    const function_body_env = get_function_body_env(ast, function_expression, args, allocator) catch |err| switch (err) {
        Environment.Error.ExceedingMaxDepth => {
            const output = try std.fmt.allocPrint(
                allocator,
                "Exceeded depth of {d} in function calls.",
                .{Environment.MaxEnvDepth},
            );
            return Object.Create(.runtime_error, allocator, @ptrCast(&output));
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
        return Object.Create(.runtime_error, allocator, @ptrCast(&output));
    };

    defer allocator.free(args);
    for (0..args.len) |i| {
        defer args[i].deinit(allocator);
    }

    const out = try evaluate_block(
        ast,
        function_expression.value.block_statements,
        allocator,
        function_body_env,
    );
    switch (out) {
        .return_expression => {
            defer out.deinit(allocator);
            switch (out.return_expression.value) {
                .function_expression => {
                    try function_expression.value.env.add_child(allocator, function_body_env);
                    return out.return_expression.value;
                },
                else => {
                    defer function_body_env.deinit(allocator);
                    return out.return_expression.value;
                },
            }
        },
        .function_expression => {
            try function_expression.value.env.add_child(allocator, function_body_env);
            return out;
        },
        else => {
            defer function_body_env.deinit(allocator);
            return out;
        },
    }
}

fn get_function_body_env(
    ast: *const Ast,
    func: *ObjectStructures.FunctionExpressionType,
    args: []const Object,
    allocator: Allocator,
) !?*Environment {
    const env = try Environment.CreateEnclosed(allocator, func.value.env);
    if (func.value.parameters.len == 0) {
        return env;
    }

    for (func.value.parameters, 0..args.len) |param, arg| {
        const identifier = get_token_literal(ast, ast.nodes.items(.main_token)[param]);
        _ = env.create_variable(allocator, identifier, args[arg], .constant) catch |err| switch (err) {
            Environment.Error.VariableAlreadyInitialised => {
                return null;
            },
            Environment.Error.NonExistantVariable => unreachable,
            Environment.Error.ConstVariableModification => unreachable,
            else => |overflow| return overflow,
        };
    }
    return env;
}

fn eval_function_arguments(
    ast: *const Ast,
    expression_location: Ast.Node.NodeIndex,
    allocator: Allocator,
    env: *Environment,
) ![]Object {
    var arguments = std.ArrayList(Object).init(allocator);
    defer arguments.deinit();
    if (expression_location == 0) {
        return arguments.toOwnedSlice();
    }
    const expression_range = ast.extra_data[expression_location .. expression_location + 2];
    const expressions = ast.extra_data[expression_range[0]..expression_range[1]];
    for (expressions) |e| {
        const obj = try eval_expression(ast, e, allocator, env);
        try arguments.append(obj);
        if (obj.getEnumTag() == .runtime_error) {
            return arguments.toOwnedSlice();
        }
    }
    return arguments.toOwnedSlice();
}

fn eval_function_expression(ast: *const Ast, node: Ast.Node, allocator: Allocator, env: *Environment) Error!Object {
    var parameters = std.ArrayList(Ast.Node.NodeIndex).init(allocator);
    defer parameters.deinit();
    if (node.node_data.lhs != 0) {
        const parameters_node = ast.nodes.get(node.node_data.lhs);
        std.debug.assert(parameters_node.tag == .FUNCTION_PARAMETER_BLOCK);
        const num_nodes = parameters_node.node_data.rhs - parameters_node.node_data.lhs;
        try parameters.ensureUnusedCapacity(num_nodes);
        for (parameters_node.node_data.lhs..parameters_node.node_data.rhs) |i| {
            try parameters.append(@as(u32, @intCast(i)));
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
    const function_declaration = ObjectStructures.FunctionValueType{
        .parameters = try parameters.toOwnedSlice(),
        .block_statements = try block_statements.toOwnedSlice(),
        .env = env,
    };
    return Object.Create(.function_expression, allocator, @ptrCast(&function_declaration));
}

fn eval_prefix_operation(tag: Ast.Node.Tag, value: Object, allocator: Allocator) Error!Object {
    switch (tag) {
        .BOOL_NOT => {
            switch (value) {
                .integer => |i| {
                    defer value.deinit(allocator);
                    const result = i.value == 0;
                    const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
                    return obj;
                },
                .boolean => |b| {
                    b.value = !b.value;
                    return value;
                },
                inline else => {
                    defer value.deinit(allocator);
                    const output = try std.fmt.allocPrint(allocator, "Unknown Operation: !<{s}>", .{value.getEnumTagAsString()});
                    const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&output));
                    return obj;
                },
            }
        },
        .NEGATION => {
            switch (value) {
                .integer => |i| {
                    i.value *= -1;
                    return value;
                },
                inline else => {
                    defer value.deinit(allocator);
                    const output = try std.fmt.allocPrint(allocator, "Unknown Operation: -<{s}>", .{value.getEnumTagAsString()});
                    const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&output));
                    return obj;
                },
            }
        },
        inline else => unreachable,
    }
    return value;
}

fn eval_intint_infix_operation(ast: *const Ast, node: *const Ast.Node, left: Object, right: Object, allocator: Allocator) Error!Object {
    defer left.deinit(allocator);
    defer right.deinit(allocator);
    const left_type = left.getEnumTag();
    const right_type = right.getEnumTag();

    if (left_type != right_type) {
        const outstr = try std.fmt.allocPrint(
            allocator,
            "Type mismatch: <{s}> {s} <{s}>",
            .{ left.getEnumTagAsString(), get_token_literal(ast, node.main_token), right.getEnumTagAsString() },
        );
        const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&outstr));
        return obj;
    }
    if (left_type != .integer) {
        const outstr = try std.fmt.allocPrint(
            allocator,
            "Unknown Operation: <{s}> {s} <{s}>",
            .{ left.getEnumTagAsString(), get_token_literal(ast, node.main_token), right.getEnumTagAsString() },
        );
        const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&outstr));
        return obj;
    }

    const l = try left.get(.integer);
    const r = try right.get(.integer);
    switch (node.tag) {
        .LESS_THAN => {
            const result: bool = l.value < r.value;
            const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
            return obj;
        },
        .GREATER_THAN => {
            const result: bool = l.value > r.value;
            const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
            return obj;
        },
        .LESS_THAN_EQUAL => {
            const result: bool = l.value <= r.value;
            const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
            return obj;
        },
        .GREATER_THAN_EQUAL => {
            const result: bool = l.value >= r.value;
            const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
            return obj;
        },
        .ADDITION => {
            const result: i64 = l.value + r.value;
            const obj = try Object.Create(.integer, allocator, @ptrCast(&result));
            return obj;
        },
        .SUBTRACTION => {
            const result: i64 = l.value - r.value;
            const obj = try Object.Create(.integer, allocator, @ptrCast(&result));
            return obj;
        },
        .MULTIPLY => {
            const result: i64 = l.value * r.value;
            const obj = try Object.Create(.integer, allocator, @ptrCast(&result));
            return obj;
        },
        .DIVIDE => {
            const result: i64 = @divFloor(l.value, r.value);
            const obj = try Object.Create(.integer, allocator, @ptrCast(&result));
            return obj;
        },
        inline else => unreachable,
    }
    return .null;
}

fn eval_if_expression(ast: *const Ast, ast_node: Ast.Node, allocator: Allocator, env: *Environment) Error!Object {
    const condition = try eval_expression(ast, ast_node.node_data.lhs, allocator, env);

    var result: bool = false;
    switch (condition) {
        .integer => |i| result = i.value != 0,
        .boolean => |b| result = b.value,
        .runtime_error => return condition,
        else => unreachable,
    }
    defer condition.deinit(allocator);

    switch (ast_node.tag) {
        .NAKED_IF => {
            if (result) {
                const block_node_tag = ast.nodes.items(.tag)[ast_node.node_data.rhs];
                std.debug.assert(block_node_tag == .BLOCK);
                const block_node_data = ast.nodes.items(.node_data)[ast_node.node_data.rhs];
                const statements = ast.extra_data[block_node_data.lhs..block_node_data.rhs];
                return evaluate_block(ast, statements, allocator, env);
            } else {
                return .null;
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
            return evaluate_block(ast, statements, allocator, env);
        },
        inline else => return .null,
    }
}

fn eval_intboolean_infix_operation(ast: *const Ast, node: *const Ast.Node, left: Object, right: Object, allocator: Allocator) Error!Object {
    defer left.deinit(allocator);
    defer right.deinit(allocator);
    var result: bool = false;
    switch (left) {
        .integer => |il| {
            switch (right) {
                .integer => |ir| {
                    switch (node.tag) {
                        .DOUBLE_EQUAL => result = il.value == ir.value,
                        .NOT_EQUAL => result = il.value != ir.value,
                        inline else => unreachable,
                    }
                },
                .boolean => |br| {
                    switch (node.tag) {
                        .DOUBLE_EQUAL => result = (il.value != 0) == br.value,
                        .NOT_EQUAL => result = (il.value != 0) != br.value,
                        inline else => unreachable,
                    }
                },
                inline else => {
                    const outstr = try std.fmt.allocPrint(
                        allocator,
                        "Unknown Operation: <{s}> {s} <{s}>",
                        .{
                            left.getEnumTagAsString(),
                            get_token_literal(ast, node.main_token),
                            right.getEnumTagAsString(),
                        },
                    );
                    const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&outstr));
                    return obj;
                },
            }
        },
        .boolean => |bl| {
            switch (right) {
                .integer => |ir| {
                    switch (node.tag) {
                        .DOUBLE_EQUAL => result = bl.value == (ir.value != 0),
                        .NOT_EQUAL => result = bl.value != (ir.value != 0),
                        inline else => unreachable,
                    }
                },
                .boolean => |br| {
                    switch (node.tag) {
                        .DOUBLE_EQUAL => result = bl.value == br.value,
                        .NOT_EQUAL => result = bl.value != br.value,
                        inline else => unreachable,
                    }
                },
                inline else => {
                    const outstr = try std.fmt.allocPrint(
                        allocator,
                        "Unknown Operation: <{s}> {s} <{s}>",
                        .{
                            left.getEnumTagAsString(),
                            get_token_literal(ast, node.main_token),
                            right.getEnumTagAsString(),
                        },
                    );
                    const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&outstr));
                    return obj;
                },
            }
        },
        inline else => {
            const outstr = try std.fmt.allocPrint(
                allocator,
                "Unknown Operation: <{s}> {s} <{s}>",
                .{
                    left.getEnumTagAsString(),
                    get_token_literal(ast, node.main_token),
                    right.getEnumTagAsString(),
                },
            );
            const obj = try Object.Create(.runtime_error, allocator, @ptrCast(&outstr));
            return obj;
        },
    }
    const obj = try Object.Create(.boolean, allocator, @ptrCast(&result));
    return obj;
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

test "evaluate_function_expressions" {
    const tests = [_]test_struct{
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
            \\  b(3);
            \\  b(7);
            ,
            .output = "10",
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
        .{ .source = "5 + true", .output = "Type mismatch: <INTEGER> + <BOOLEAN>" },
        .{ .source = "foobar", .output = "Identifier not found: foobar" },
        .{ .source = "foobar * 10", .output = "Identifier not found: foobar" },
        .{ .source = "5; fizzbuzz * 10", .output = "Identifier not found: fizzbuzz" },
        .{ .source = "if ( 1 + 2 < a ) { return false + 5; }", .output = "Identifier not found: a" },
        .{ .source = "if ( 1 + 2 < 10 ) { 10; c; return b + 5; }", .output = "Identifier not found: c" },
        .{ .source = "true - true", .output = "Unknown Operation: <BOOLEAN> - <BOOLEAN>" },
        .{ .source = "-true", .output = "Unknown Operation: -<BOOLEAN>" },
        .{ .source = "if ( 1 < 10 ) { return false + 5; }", .output = "Type mismatch: <BOOLEAN> + <INTEGER>" },
        .{ .source = "if ( 1 + true < 10 ) { return false + 5; }", .output = "Type mismatch: <INTEGER> + <BOOLEAN>" },
        .{ .source = "if ( 10 > 1 + true ) { return false + 5; }", .output = "Type mismatch: <INTEGER> + <BOOLEAN>" },
        .{ .source = "5 + 5; 5 + true; if ( 1 < 10 ) { return false + 5; }", .output = "Type mismatch: <INTEGER> + <BOOLEAN>" },
        .{ .source = "const a = 10; a = 11;", .output = "Identifier \"a\" is declared as a constant and cannot be modified" },
        .{ .source = "const a = 10; b = 11;", .output = "Identifier \"b\" does not exist. Cannot assign anything to it." },
        .{ .source = "const a = 10; const a = 11;", .output = "Identifier \"a\" has already been initialised" },
        .{ .source = "const add = fn(x, y) { return x + y}; add(1, b)", .output = "Identifier not found: b" },
    };

    try eval_tests(&tests, false);
}

fn eval_tests(tests: []const test_struct, enable_debug_print: bool) !void {
    var buffer: [1024]u8 = undefined;
    for (tests) |t| {
        var env = try Environment.Create(testing.allocator);
        defer env.deinit(testing.allocator);
        var ast = try Parser.parse_program(t.source, testing.allocator);
        defer ast.deinit(testing.allocator);

        const output = try Evaluator.evaluate_program(&ast, testing.allocator, env);
        defer output.deinit(testing.allocator);
        const outstr = try output.ToString(&buffer);
        if (enable_debug_print) {
            std.debug.print("Testing: Source: {s}\n Expected: {s} \t Got: {s}\n", .{ t.source, t.output, outstr });
        }
        try testing.expectEqualSlices(u8, t.output, outstr);
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const token = @import("token.zig");
const Environment = @import("environment.zig");
const Ast = @import("ast.zig");
const Parser = @import("parser.zig");
const object = @import("object.zig");
const Object = object.Object;
const ObjectStructures = object.ObjectStructures;
const ObjectTypes = object.ObjectTypes;
