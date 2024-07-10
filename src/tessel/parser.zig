pub const Parser = @This();
// TODO: If a scope has 0 local variables then pretend there is no new scope removing the need for enter and leave scope

/// The parser holds a reference to the source code to reference in case of errors
source_buffer: [:0]const u8,
/// The allocator to be used in all internal allocations
allocator: std.mem.Allocator,
tree: *SymbolTree,
/// List of token types for each array. Seperated here for ease of access
/// so that we dont have to get a Token struct out of a multiArray every time
token_tags: []const token.TokenType,
/// The starting positions of the tokens in the source code
token_starts: []const u32,
/// The ending poistions of all the tokens in the source code
token_ends: []const u32,

/// The current index in the token array we are parsing
token_current: Ast.TokenArrayIndex,

/// Multi array list of nodes that make up the AST
nodes: Ast.NodeArrayType,
/// A list of indicies that some expressions might need for extra storage.
/// Some nodes in the AST cannot make do with just 2 children so for them this data exists so that
/// one of the nodes can point to a location in this array and we can parse those nodes
extra_data: std.ArrayListUnmanaged(Ast.Node.NodeIndex),
/// In the function expression node and block nodes we are required to hold the list of statemnts
/// locally before flushign it into the extra_data list to avoid interleaving with the parent function call
/// This might happen for example if we have a function inside of a function
///
/// The scratch_pad exists as a intermediate buffer of data to store to before writing to extra_data
/// this avoids having to create and destroy arraylists within the function calls themselves
///
/// This kind of data management is taken from zig's own Parser
scratch_pad: std.ArrayListUnmanaged(Ast.Node.NodeIndex),
/// A list to store all the parsing errors we encounter.
errors: std.ArrayListUnmanaged(Ast.Error),
/// When we go on to evaluate the program integer literals will have to be converted from strings to int.
/// To avoid having to do this conversion many times especially in repeated function calls we are trading off
/// a bit more space in the AST by converting integer literals as they are parsed and pointing to a location
/// in this array. This results in at worst the same number of string to int calls.
integer_literal_program_memory: std.ArrayListUnmanaged(i64),
scope_stack: std.ArrayListUnmanaged(SymbolTree.SymbolIndex),

/// Since the root node does not have any data and is always at 0 we can use 0 as a null value
pub const null_node: Ast.Node.NodeIndex = 0;

pub const Error = error{ParsingError} || Allocator.Error;

pub fn parse_program(source_buffer: [:0]const u8, allocator: std.mem.Allocator, tree: *SymbolTree) !Ast {
    std.debug.assert(source_buffer.len > 0);

    var tokens_local = Ast.TokenArrayType{};

    var local_lexer = Lexer.init(source_buffer);
    while (true) {
        const tok = local_lexer.next_token();
        try tokens_local.append(allocator, .{
            .tag = tok.type, //
            .start = @as(u32, @intCast(tok.loc.start)),
            .end = @as(u32, @intCast(tok.loc.end)),
        });
        if (tok.type == .EOF) break;
    }

    if (tree.tree.items.len == 0) {
        _ = try tree.create_table(null);
    }

    var parser = Parser{
        .source_buffer = source_buffer, //
        .allocator = allocator,
        .tree = tree,
        .token_tags = tokens_local.items(.tag),
        .token_starts = tokens_local.items(.start),
        .token_ends = tokens_local.items(.end),
        .token_current = 0,
        .nodes = .{},
        .errors = .{},
        .extra_data = .{},
        .scratch_pad = .{},
        .integer_literal_program_memory = .{},
        .scope_stack = .{},
    };
    defer parser.deinit(allocator);
    try parser.scope_stack.append(allocator, 0);

    const estimated_nodes = tokens_local.len / 2 + 2;
    try parser.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    try parser.begin_parsing();

    return Ast{
        .source_buffer = source_buffer,
        .tokens = tokens_local.toOwnedSlice(), //
        .nodes = parser.nodes.toOwnedSlice(),
        .errors = try parser.errors.toOwnedSlice(allocator),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
        .integer_literals = try parser.integer_literal_program_memory.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *Parser, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    self.errors.deinit(allocator);
    self.extra_data.deinit(allocator);
    self.scratch_pad.deinit(allocator);
    self.integer_literal_program_memory.deinit(allocator);
    self.scope_stack.deinit(allocator);
}

pub fn begin_parsing(self: *Parser) !void {
    self.nodes.appendAssumeCapacity(.{
        .tag = .ROOT, //
        .main_token = 0,
        .node_data = .{
            .lhs = 0,
            .rhs = 0,
        },
    });
    const scratch_top = self.scratch_pad.items.len;
    defer self.scratch_pad.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const node = try self.parse_statement(.global);
        if (node != null_node) {
            try self.scratch_pad.append(self.allocator, node);
        }
        if (self.is_current_token(.EOF)) {
            break;
        }
        if (self.token_current >= self.token_tags.len) {
            break;
        }
    }
    const extra_data_loc = try self.append_slice_to_extra_data(self.scratch_pad.items[scratch_top..]);
    self.nodes.items(.node_data)[0].lhs = extra_data_loc.start;
    self.nodes.items(.node_data)[0].rhs = extra_data_loc.end;
}

fn parse_statement(self: *Parser, scope: SymbolTable.SymbolScope) !Ast.Node.NodeIndex {
    switch (self.token_tags[self.token_current]) {
        .EOF => return null_node,
        .CONST, .VAR => {
            return self.parse_var_decl(scope) catch |err| switch (err) {
                Error.ParsingError => return null_node,
                else => |overflow| return overflow,
            };
        },
        .RETURN => {
            return self.parse_return_statement() catch |err| switch (err) {
                Error.ParsingError => return null_node,
                else => |overflow| return overflow,
            };
        },
        .WHILE => {
            return self.parse_while_loop() catch |err| switch (err) {
                Error.ParsingError => return null_node,
                else => |overflow| return overflow,
            };
        },
        .BREAK => {
            const break_token = self.next_token();
            _ = self.eat_token(.SEMICOLON);
            return self.add_node(.{
                .tag = .BREAK_STATEMENT, //
                .main_token = break_token,
                .node_data = .{ .lhs = 0, .rhs = 0 },
            });
        },
        .CONTINUE => {
            const cont_token = self.next_token();
            _ = self.eat_token(.SEMICOLON);
            return self.add_node(.{
                .tag = .CONTINUE_STATEMENT, //
                .main_token = cont_token,
                .node_data = .{ .lhs = 0, .rhs = 0 },
            });
        },
        else => {
            if (self.is_current_token(.IDENT)) {
                return self.maybe_parse_assign_statement() catch |err| switch (err) {
                    Error.ParsingError => return null_node,
                    else => |overflow| return overflow,
                };
            } else {
                return self.parse_expression_statement() catch |err| switch (err) {
                    Error.ParsingError => return null_node,
                    else => |overflow| return overflow,
                };
            }
        },
    }
    return null_node;
}

fn parse_while_loop(self: *Parser) Error!Ast.Node.NodeIndex {
    const while_node = try self.add_node(.{
        .tag = .WHILE_LOOP, //
        .main_token = self.next_token(),
        .node_data = .{ .lhs = 0, .rhs = 0 },
    });
    const current_scope = self.scope_stack.getLast();
    const new_scope = self.tree.create_table(current_scope) catch |err| switch (err) {
        SymbolTree.SymbolTreeError.ReinitialisingGlobalTree => unreachable,
        SymbolTree.Error.UnkownIdentifier => unreachable,
        SymbolTree.Error.IdentifierRedecleration => unreachable,
        else => |overflow| return overflow,
    };

    try self.scope_stack.append(self.allocator, new_scope);
    _ = try self.expect_token(.LPAREN);
    const condition = try self.parse_expect_expression();
    _ = try self.expect_token(.RPAREN);
    const block = try self.parse_block(true, new_scope);
    self.nodes.items(.node_data)[while_node].lhs = condition;

    const num_locals = self.tree.tree.items[new_scope].num_locals;
    const block_start = self.extra_data.items.len;
    try self.extra_data.append(self.allocator, block);
    try self.extra_data.append(self.allocator, num_locals);
    self.nodes.items(.node_data)[while_node].rhs = @intCast(block_start);

    _ = self.scope_stack.pop();
    return while_node;
}

fn parse_expression_statement(self: *Parser) Error!Ast.Node.NodeIndex {
    const expr_node = try self.add_node(.{
        .tag = .EXPRESSION_STATEMENT, //
        .main_token = self.token_current,
        .node_data = .{ .lhs = 0, .rhs = 0 },
    });
    const node = self.parse_expect_expression() catch |err| switch (err) {
        Error.ParsingError => {
            self.print_last_error();
            return null_node;
        },
        else => |overflow| return overflow,
    };
    _ = self.eat_token(.SEMICOLON);
    self.nodes.items(.node_data)[expr_node].lhs = node;
    return expr_node;
}

fn maybe_parse_assign_statement(self: *Parser) Error!Ast.Node.NodeIndex {
    std.debug.assert(self.is_current_token(.IDENT));
    const next_tok = self.peek_next_token_tag() orelse {
        return self.parse_expression_statement();
    };
    // TODO: Add .COMMA here and have multi variable assignment
    switch (next_tok) {
        .ASSIGN => {},
        else => return self.parse_expression_statement(),
    }
    const ident_token = try self.expect_token(.IDENT);

    const assign_token = try self.expect_token(.ASSIGN);
    const assign_node = try self.add_node(.{
        .tag = .ASSIGNMENT_STATEMENT,
        .main_token = assign_token,
        .node_data = .{
            .lhs = 0,
            .rhs = 0,
        },
    });
    const scope = self.scope_stack.getLast();
    const hash = self.tree.resolve(scope, self.get_token_literal(ident_token)) catch |err| switch (err) {
        SymbolTable.SymbolError.UnkownIdentifier => {
            try self.add_error(.{
                .tag = .unkown_variable, //
                .token = ident_token,
                .expected = .IDENT,
            });
            _ = self.eat_token_till(.SEMICOLON);
            return Error.ParsingError;
        },
        SymbolTable.SymbolError.IdentifierRedecleration => unreachable,
        else => |overflow| return overflow,
    };

    if (hash.type == .constant) {
        try self.add_error(.{
            .tag = .reassigning_const, //
            .token = ident_token,
            .expected = .IDENT,
        });
        _ = self.eat_token_till(.SEMICOLON);
        return Error.ParsingError;
    }
    const ident_node = try self.add_node(.{
        .tag = .IDENTIFIER,
        .main_token = ident_token,
        .node_data = SymbolTree.symbol_to_identifier(hash, self.tree.get_depth(scope)),
    });

    const expression = try self.parse_expect_expression();
    self.nodes.items(.node_data)[assign_node].lhs = ident_node;
    self.nodes.items(.node_data)[assign_node].rhs = expression;
    _ = self.eat_token(.SEMICOLON);
    return assign_node;
}

fn parse_var_decl(self: *Parser, scope: SymbolTable.SymbolScope) Error!Ast.Node.NodeIndex {
    _ = scope;
    const var_str = self.get_token_literal(self.token_current);
    const node = try self.add_node(.{
        .tag = Ast.Node.Tag.VAR_STATEMENT, //
        .main_token = self.next_token(),
        .node_data = undefined,
    });

    var var_tag: SymbolTable.Symbol.Tag = .constant;
    if (std.mem.eql(u8, var_str, "var")) {
        var_tag = .variable;
    }

    // After a const or var statement you expect a identifier
    if (self.is_current_token(.IDENT)) {
        const current_scope = self.scope_stack.getLast();
        const hash_node = self.tree.define_node_data(
            current_scope,
            self.get_token_literal(self.token_current),
            var_tag,
        ) catch |err| switch (err) {
            SymbolTable.Error.IdentifierRedecleration => {
                try self.add_error(.{
                    .tag = .variable_redecleration, //
                    .token = self.token_current,
                    .expected = .IDENT,
                });
                _ = self.eat_token_till(.SEMICOLON);
                return Error.ParsingError;
            },
            SymbolTable.Error.UnkownIdentifier => unreachable,
            else => |overflow| return overflow,
        };

        const ident = try self.add_node(.{
            .tag = .IDENTIFIER, //
            .main_token = self.next_token(),
            .node_data = hash_node,
        });
        self.nodes.items(.node_data)[node].lhs = ident;
    } else {
        try self.add_error(.{
            .tag = .expected_identifier_after_const, //
            .token = self.token_current,
            .expected = .IDENT,
        });
        _ = self.eat_token_till(.SEMICOLON);
        return Error.ParsingError;
    }

    if (!self.is_current_token(.ASSIGN)) {
        try self.add_error(.{
            .tag = .expected_assignent_after_var_decl, //
            .token = self.token_current,
            .expected = .ASSIGN,
        });
        _ = self.eat_token_till(.SEMICOLON);
        return Error.ParsingError;
    }

    _ = self.eat_token(.ASSIGN);

    const rhs = try self.parse_expect_expression();
    if (rhs == 0) {
        return node;
    }
    _ = self.eat_token(.SEMICOLON) orelse null;
    self.nodes.items(.node_data)[node].rhs = rhs;
    return node;
}

fn parse_return_statement(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.add_node(.{
        .tag = .RETURN_STATEMENT, //
        .main_token = self.next_token(),
        .node_data = .{
            .lhs = 0,
            .rhs = 0,
        },
    });

    const lhs = try self.parse_expect_expression();
    if (lhs == 0) {
        return node;
    }
    _ = self.eat_token(.SEMICOLON) orelse null;
    self.nodes.items(.node_data)[node].lhs = lhs;

    return node;
}

const OperatorPrecedence = struct {
    precedence: i8,
    tag: Ast.Node.Tag,
};

const OperatorHierarchy = std.enums.directEnumArrayDefault(token.TokenType, OperatorPrecedence, .{ .precedence = -1, .tag = .ROOT }, 0, .{
    .EQ = .{ .precedence = 30, .tag = .DOUBLE_EQUAL }, //
    .NEQ = .{ .precedence = 30, .tag = .NOT_EQUAL },

    .LT = .{ .precedence = 50, .tag = .LESS_THAN },
    .LTE = .{ .precedence = 50, .tag = .LESS_THAN_EQUAL },
    .GT = .{ .precedence = 50, .tag = .GREATER_THAN },
    .GTE = .{ .precedence = 50, .tag = .GREATER_THAN_EQUAL },

    .PLUS = .{ .precedence = 70, .tag = .ADDITION },
    .MINUS = .{ .precedence = 70, .tag = .SUBTRACTION },

    .ASTRIX = .{ .precedence = 90, .tag = .MULTIPLY },
    .SLASH = .{ .precedence = 90, .tag = .DIVIDE },

    // Functions are of the form .IDENT(.ARGUMENTS. So we can treat ( as an infix operator and declare it
    // a function call. In no other instance can you have an expression followed by a (.
    .LPAREN = .{ .precedence = 110, .tag = .FUNCTION_CALL },
    .LBRACKET = .{ .precedence = 110, .tag = .INDEX_INTO },
});

fn parse_expect_expression(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.parse_expression_precedence(0);
    if (node == 0) {
        try self.add_error(.{
            .tag = .expected_expression, //
            .token = self.token_current,
            .expected = .ILLEGAL,
        });
        _ = self.next_token();
        return null_node;
    } else {
        return node;
    }
}

fn parse_expression_precedence(self: *Parser, min_presedence: i32) Error!Ast.Node.NodeIndex {
    std.debug.assert(min_presedence >= 0);

    var cur_node = try self.parse_prefix();

    if (cur_node == null_node) {
        return null_node;
    }

    while (true) {
        const current_tag = self.token_tags[self.token_current];
        if (current_tag == .SEMICOLON or current_tag == .EOF) {
            break;
        }

        const operator_info: OperatorPrecedence = OperatorHierarchy[@as(usize, @intCast(@intFromEnum(current_tag)))];
        if (operator_info.precedence < min_presedence) {
            break;
        }

        const operator_token = self.next_token();

        if (self.token_tags[operator_token] == .LPAREN) {
            const curr_tag = self.nodes.get(cur_node).tag;
            if (curr_tag != .IDENTIFIER and
                curr_tag != .FUNCTION_EXPRESSION and
                curr_tag != .INDEX_INTO and
                curr_tag != .FUNCTION_CALL)
            {
                self.token_current -= 1;
                break;
            }
            cur_node = try self.parse_function_call(cur_node, operator_token);
            continue;
        }
        if (self.token_tags[operator_token] == .LBRACKET) {
            const curr_node_tag = self.nodes.get(cur_node).tag;
            if (curr_node_tag != .IDENTIFIER and
                curr_node_tag != .ARRAY_LITERAL and
                curr_node_tag != .STRING_LITERAL and
                curr_node_tag != .HASH_LITERAL and
                curr_node_tag != .INDEX_INTO and
                curr_node_tag != .INDEX_RANGE)
            {
                self.token_current -= 1;
                break;
            }
            cur_node = try self.parse_index_into(cur_node, operator_token);
            continue;
        }

        const rhs = try self.parse_expression_precedence(operator_info.precedence + 1);

        if (rhs == 0) {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            return cur_node;
        }

        cur_node = try self.add_node(.{
            .tag = operator_info.tag, //
            .main_token = operator_token,
            .node_data = .{
                .lhs = cur_node, //
                .rhs = rhs,
            },
        });
    }
    return cur_node;
}

fn parse_prefix(self: *Parser) Error!Ast.Node.NodeIndex {
    // std.debug.print("Entering prefix parser token: {s}\r\n", .{@tagName(self.token_tags[self.token_current])});
    const tag_type: Ast.Node.Tag = switch (self.token_tags[self.token_current]) {
        .BANG => .BOOL_NOT,
        .MINUS => .NEGATION,
        else => return self.parse_other_expressions(),
    };
    // std.debug.print("Parsing Prefix tag: {s}\r\n", .{@tagName(tag_type)});
    const main_token = self.next_token();
    return self.add_node(.{
        .tag = tag_type, //
        .main_token = main_token,
        .node_data = .{ //
            .lhs = try self.parse_expect_prefix_expression(),
            .rhs = 0,
        },
    });
}

fn parse_expect_prefix_expression(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.parse_prefix();
    if (node == 0) {
        try self.add_error(.{
            .tag = .expected_prefix_expression, //
            .token = self.token_current,
            .expected = .ILLEGAL,
        });
        return Error.ParsingError;
    }
    return node;
}

fn parse_other_expressions(self: *Parser) Error!Ast.Node.NodeIndex {
    switch (self.token_tags[self.token_current]) {
        .IDENT => {
            const scope = self.scope_stack.getLast();
            const hash = self.tree.resolve_node_data(scope, self.get_token_literal(self.token_current)) catch |err| switch (err) {
                SymbolTable.SymbolError.UnkownIdentifier => {
                    try self.add_error(.{
                        .tag = .unkown_variable, //
                        .token = self.token_current,
                        .expected = .IDENT,
                    });
                    _ = self.eat_token_till(.SEMICOLON);
                    return Error.ParsingError;
                },
                SymbolTable.SymbolError.IdentifierRedecleration => unreachable,
            };
            return self.add_node(.{
                .tag = .IDENTIFIER, //
                .main_token = self.next_token(),
                .node_data = hash,
            });
        },
        .INT => {
            const location = self.register_integer_literal() catch |err| switch (err) {
                std.fmt.ParseIntError.Overflow => @panic("Unable to parse Integer Literal! Something has gone terribly wrong\n"),
                std.fmt.ParseIntError.InvalidCharacter => @panic("Unable to parse Integer Literal! Something has gone terribly wrong\n"),
                else => |overflow| return overflow,
            };
            return self.add_node(.{
                .tag = .INTEGER_LITERAL,
                .main_token = self.next_token(),
                .node_data = .{ .lhs = location, .rhs = 0 },
            });
        },
        .STRING => {
            const start = self.token_starts[self.token_current];
            const end = self.token_ends[self.token_current];
            return self.add_node(.{
                .tag = .STRING_LITERAL, //
                .main_token = self.next_token(),
                .node_data = .{
                    .lhs = start,
                    .rhs = end,
                },
            });
        },
        .FALSE => return self.add_node(.{
            .tag = .BOOLEAN_LITERAL, //
            .main_token = self.next_token(),
            .node_data = .{ .lhs = 0, .rhs = 0 },
        }),
        .TRUE => return self.add_node(.{
            .tag = .BOOLEAN_LITERAL, //
            .main_token = self.next_token(),
            .node_data = .{ .lhs = 1, .rhs = 0 },
        }),
        .LPAREN => {
            _ = self.next_token();
            const node = try self.parse_expression_precedence(0);
            _ = try self.expect_token(.RPAREN);
            return node;
        },
        .LBRACE => {
            return self.parse_hash_map();
        },
        .LBRACKET => return self.parse_array_literal(),
        .IF => return self.parse_if_expression(),
        .FUNCTION => return self.parse_function_expression(),
        else => {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            _ = self.next_token();
            return Error.ParsingError;
        },
    }
}

fn parse_hash_map(self: *Parser) Error!Ast.Node.NodeIndex {
    const hash_token = try self.expect_token(.LBRACE);
    if (self.is_current_token(.RBRACE)) {
        _ = self.eat_token(.RBRACKET);
        return self.add_node(.{
            .tag = .HASH_LITERAL,
            .main_token = hash_token,
            .node_data = .{
                .lhs = 0,
                .rhs = 0,
            },
        });
    }
    const scratch_top = self.scratch_pad.items.len;
    defer self.scratch_pad.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const map = try self.parse_expect_map();
        if (map == 0) {
            try self.add_error(.{
                .tag = .expected_map, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            continue;
        }
        try self.scratch_pad.append(self.allocator, map);
        if (self.is_current_token(.RBRACE)) break;
        _ = try self.expect_token(.COMMA);
    }
    const extra_data_locs = try self.append_slice_to_extra_data(self.scratch_pad.items[scratch_top..]);
    _ = self.eat_token(.RBRACE);
    return self.add_node(.{
        .tag = .HASH_LITERAL, //
        .main_token = hash_token,
        .node_data = .{
            .lhs = extra_data_locs.start, //
            .rhs = extra_data_locs.end,
        },
    });
}

fn parse_expect_map(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.parse_map();
    if (node == 0) {
        try self.add_error(.{
            .tag = .expected_expression, //
            .token = self.token_current,
            .expected = .ILLEGAL,
        });
        _ = self.next_token();
        return null_node;
    } else {
        return node;
    }
}

fn parse_map(self: *Parser) Error!Ast.Node.NodeIndex {
    const map = try self.add_node(.{
        .tag = .HASH_ELEMENT,
        .main_token = 0,
        .node_data = .{
            .lhs = 0,
            .rhs = 0,
        },
    });

    const lhs = try self.parse_expect_expression();
    if (lhs == 0) {
        return null_node;
    }
    const colon = try self.expect_token(.COLON);
    const rhs = try self.parse_expect_expression();
    if (rhs == 0) {
        return null_node;
    }
    self.nodes.items(.main_token)[map] = colon;
    self.nodes.items(.node_data)[map].lhs = lhs;
    self.nodes.items(.node_data)[map].rhs = rhs;
    return map;
}

fn parse_array_literal(self: *Parser) Error!Ast.Node.NodeIndex {
    std.debug.assert(self.is_current_token(.LBRACKET));
    // const last_node_tag = self.nodes.get(self.nodes.len - 1).tag;
    // if (last_node_tag == .INDEX_INTO or last_node_tag == .INDEX_RANGE) {
    //     std.debug.print("Getting a double index operation\n", .{});
    //     return self.parse_index_into(@as(u32, @intCast(self.nodes.len - 1)), self.next_token());
    // }
    const array_literal_token = self.next_token();
    if (self.is_current_token(.RBRACKET)) {
        _ = self.eat_token(.RBRACKET);
        return self.add_node(.{
            .tag = .ARRAY_LITERAL,
            .main_token = array_literal_token,
            .node_data = .{
                .lhs = 0,
                .rhs = 0,
            },
        });
    }

    const scratch_top = self.scratch_pad.items.len;
    defer self.scratch_pad.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const arg = try self.parse_expect_expression();
        if (arg == 0) {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            continue;
        }
        try self.scratch_pad.append(self.allocator, arg);
        if (self.is_current_token(.RBRACKET)) break;
        _ = try self.expect_token(.COMMA);
    }
    const extra_data_locs = try self.append_slice_to_extra_data(self.scratch_pad.items[scratch_top..]);
    _ = self.eat_token(.RBRACKET);
    return self.add_node(.{
        .tag = .ARRAY_LITERAL, //
        .main_token = array_literal_token,
        .node_data = .{
            .lhs = extra_data_locs.start, //
            .rhs = extra_data_locs.end,
        },
    });
}

fn parse_function_expression(self: *Parser) Error!Ast.Node.NodeIndex {
    std.debug.assert(self.is_current_token(.FUNCTION));
    const current_scope = self.scope_stack.getLast();
    const new_scope = self.tree.create_table(current_scope) catch |err| switch (err) {
        SymbolTree.SymbolTreeError.ReinitialisingGlobalTree => unreachable,
        SymbolTree.Error.UnkownIdentifier => unreachable,
        SymbolTree.Error.IdentifierRedecleration => unreachable,
        else => |overflow| return overflow,
    };
    try self.scope_stack.append(self.allocator, new_scope);
    const func_token = self.next_token();
    _ = try self.expect_token(.LPAREN);

    const parameters = try self.parse_function_parameters(func_token);
    _ = try self.expect_token(.RPAREN);

    const func_block = try self.parse_block(false, new_scope);

    _ = self.scope_stack.pop();
    const num_locals = self.tree.tree.items[new_scope].num_locals;
    const function_storage_start = self.extra_data.items.len;
    try self.extra_data.append(self.allocator, func_block);
    try self.extra_data.append(self.allocator, num_locals);

    return self.add_node(.{
        .tag = .FUNCTION_EXPRESSION, //
        .main_token = func_token,
        .node_data = .{
            .lhs = parameters, //
            .rhs = @intCast(function_storage_start),
        },
    });
}

/// .IDENT, .IDENT ... OR empty
fn parse_function_parameters(self: *Parser, func_token: Ast.TokenArrayIndex) Error!Ast.Node.NodeIndex {
    if (self.is_current_token(.RPAREN)) {
        return null_node;
    }
    const start_pos = @as(u32, @intCast(self.nodes.len));
    while (true) {
        const current_scope = self.scope_stack.getLast();
        const next_tok = try self.expect_token(.IDENT);
        const hash = self.tree.define_node_data(current_scope, self.get_token_literal(next_tok), .constant) catch |err| switch (err) {
            SymbolTable.Error.IdentifierRedecleration => {
                try self.add_error(.{
                    .tag = .variable_redecleration, //
                    .token = self.token_current,
                    .expected = .IDENT,
                });
                _ = self.eat_token_till(.SEMICOLON);
                return Error.ParsingError;
            },
            SymbolTable.Error.UnkownIdentifier => unreachable,
            else => |overflow| return overflow,
        };
        _ = try self.add_node(.{
            .tag = .IDENTIFIER, //
            .main_token = next_tok,
            .node_data = hash,
        });
        if (self.is_current_token(.RPAREN)) break;
        _ = try self.expect_token(.COMMA);
    }
    const end_pos = @as(u32, @intCast(self.nodes.len));
    return self.add_node(.{
        .tag = .FUNCTION_PARAMETER_BLOCK, //
        .main_token = func_token,
        .node_data = .{
            .lhs = start_pos, //
            .rhs = end_pos,
        },
    });
}

fn parse_if_expression(self: *Parser) Error!Ast.Node.NodeIndex {
    // We sould only come here if we have an if token
    std.debug.assert(self.token_tags[self.token_current] == .IF);
    const if_token = self.token_current;
    _ = try self.expect_token(.IF);
    _ = try self.expect_token(.LPAREN);

    const condition = try self.parse_expect_expression();
    if (condition == null_node) {
        return null_node;
    }
    _ = try self.expect_token(.RPAREN);

    const if_block = try self.parse_block(true, null);
    if (!self.is_current_token(.ELSE)) {
        return self.add_node(.{
            .tag = .NAKED_IF,
            .main_token = if_token, //
            .node_data = .{
                .lhs = condition, //
                .rhs = if_block,
            },
        });
    }
    _ = try self.expect_token(.ELSE);

    const else_block = try self.parse_block(true, null);
    const block_storage_start = self.extra_data.items.len;
    try self.extra_data.append(self.allocator, if_block);
    try self.extra_data.append(self.allocator, else_block);
    return self.add_node(.{
        .tag = .IF_ELSE,
        .main_token = if_token, //
        .node_data = .{
            .lhs = condition, //
            .rhs = @as(u32, @intCast(block_storage_start)),
        },
    });
}

fn parse_block(self: *Parser, allow_break_continue: bool, scope: ?SymbolTree.SymbolIndex) Error!Ast.Node.NodeIndex {
    const left_brace = try self.expect_token(.LBRACE);

    const scratch_top = self.scratch_pad.items.len;
    defer self.scratch_pad.shrinkRetainingCapacity(scratch_top);
    var new_scope: SymbolTree.SymbolIndex = 0;
    if (scope) |s| {
        new_scope = s;
    } else {
        const current_scope = self.scope_stack.getLast();
        new_scope = self.tree.create_table(current_scope) catch |err| switch (err) {
            SymbolTree.SymbolTreeError.ReinitialisingGlobalTree => unreachable,
            SymbolTree.Error.UnkownIdentifier => unreachable,
            SymbolTree.Error.IdentifierRedecleration => unreachable,
            else => |overflow| return overflow,
        };
        try self.scope_stack.append(self.allocator, new_scope);
    }

    // We will keep looping through statements till we encounter a closing brace
    while (true) {
        if (self.is_current_token(.RBRACE)) {
            break;
        }
        if ((self.is_current_token(.BREAK) or self.is_current_token(.CONTINUE)) and !allow_break_continue) {
            try self.add_error(.{
                .tag = .illegal_break_or_continue, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            return Error.ParsingError;
        }
        const statement = try self.parse_statement(.local);
        if (statement == 0) {
            break;
        }
        try self.scratch_pad.append(self.allocator, statement);
    }
    _ = try self.expect_token(.RBRACE);
    if (scope) |_| {} else {
        const num_locals = self.tree.tree.items[new_scope].num_locals;
        try self.scratch_pad.append(self.allocator, num_locals);
    }

    const extra_data_loc = try self.append_slice_to_extra_data(self.scratch_pad.items[scratch_top..]);
    if (scope) |_| {} else {
        _ = self.scope_stack.pop();
    }
    return self.add_node(.{
        .tag = .BLOCK, //
        .main_token = left_brace,
        .node_data = .{
            .lhs = @as(u32, @intCast(extra_data_loc.start)), //
            .rhs = @as(u32, @intCast(extra_data_loc.end)),
        },
    });
}

fn parse_index_into(
    self: *Parser,
    array_literal: Ast.Node.NodeIndex,
    operator: Ast.Node.NodeIndex,
) Error!Ast.Node.NodeIndex {
    const index = try self.parse_expect_expression();
    if (index == 0) {
        try self.add_error(.{
            .tag = .expected_expression, //
            .token = self.token_current,
            .expected = .ILLEGAL,
        });
        return null_node;
    }
    if (self.is_current_token(.COLON)) {
        _ = self.eat_token(.COLON);
        const index_end = try self.parse_expect_expression();
        if (index_end == 0) {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            return null_node;
        }
        _ = try self.expect_token(.RBRACKET);

        const location = self.extra_data.items.len;
        try self.extra_data.append(self.allocator, index);
        try self.extra_data.append(self.allocator, index_end);
        return self.add_node(.{
            .tag = .INDEX_RANGE, //
            .main_token = operator,
            .node_data = .{
                .lhs = array_literal, //
                .rhs = @as(u32, @intCast(location)),
            },
        });
    }
    _ = try self.expect_token(.RBRACKET);
    return self.add_node(.{
        .tag = .INDEX_INTO, //
        .main_token = operator,
        .node_data = .{
            .lhs = array_literal, //
            .rhs = index,
        },
    });
}

fn parse_function_call(
    self: *Parser,
    function_name: Ast.Node.NodeIndex,
    operator: Ast.Node.NodeIndex,
) Error!Ast.Node.NodeIndex {
    if (self.is_current_token(.RPAREN)) {
        _ = self.eat_token(.RPAREN);
        return self.add_node(.{
            .tag = .FUNCTION_CALL, //
            .main_token = operator,
            .node_data = .{
                .lhs = function_name, //
                .rhs = 0,
            },
        });
    }
    // We need to maintain a local array of nodes for this function call
    // because we might have function calls as expressions that can be an argument
    // so in every level we store a local list of nodes we need to visit for arugments
    // and only after we ahve parsed all the arguments do we dump it into the list
    // so we will always have a contiguous list of values
    // node1, ..nodeN, start, end in the extra_data array
    const scratch_top = self.scratch_pad.items.len;
    // We want to shrink the scratch_pad back down to the size we started with at the end of
    // our operations here so that the parent functin call can continue writing to the scratchpad
    // in a contiguous manner and wont have junk data from a child call
    defer self.scratch_pad.shrinkRetainingCapacity(scratch_top);
    while (true) {
        const arg = try self.parse_expect_expression();
        if (arg == 0) {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            continue;
        }
        try self.scratch_pad.append(self.allocator, arg);
        if (self.is_current_token(.RPAREN)) break;
        _ = try self.expect_token(.COMMA);
    }
    const extra_data_locs = try self.append_slice_to_extra_data(self.scratch_pad.items[scratch_top..]);
    try self.extra_data.append(self.allocator, extra_data_locs.start);
    try self.extra_data.append(self.allocator, extra_data_locs.end);
    _ = self.eat_token(.RPAREN);
    return self.add_node(.{
        .tag = .FUNCTION_CALL, //
        .main_token = operator,
        .node_data = .{
            .lhs = function_name, //
            .rhs = extra_data_locs.end,
        },
    });
}

fn append_slice_to_extra_data(self: *Parser, slice: []const Ast.Node.NodeIndex) !Ast.Node.ExtraDataRange {
    try self.extra_data.appendSlice(self.allocator, slice);
    const slice_len = @as(Ast.Node.NodeIndex, @intCast(slice.len));
    const end = @as(Ast.Node.NodeIndex, @intCast(self.extra_data.items.len));
    return Ast.Node.ExtraDataRange{
        .start = end - slice_len,
        .end = end,
    };
}

fn add_node(self: *Parser, node: Ast.Node) Allocator.Error!Ast.Node.NodeIndex {
    const node_index = @as(Ast.Node.NodeIndex, @intCast(self.nodes.len));
    try self.nodes.append(self.allocator, node);
    return node_index;
}

fn add_error(self: *Parser, err: Ast.Error) Allocator.Error!void {
    try self.errors.append(self.allocator, err);
}

fn expect_token(self: *Parser, tag: token.TokenType) Error!Ast.TokenArrayIndex {
    if (self.is_current_token(tag)) {
        return self.next_token();
    } else {
        return self.token_parsing_error(.{
            .tag = .expected_token, //
            .token = self.token_current,
            .expected = tag,
        });
    }
}

fn token_parsing_error(self: *Parser, err: Ast.Error) Error {
    try self.add_error(err);
    return Error.ParsingError;
}

fn next_token(self: *Parser) Ast.TokenArrayIndex {
    const result = self.token_current;
    self.token_current += 1;
    return result;
}

fn peek_next_token_tag(self: *Parser) ?token.TokenType {
    if (self.token_current >= self.token_tags.len - 1) {
        return null;
    }
    return self.token_tags[self.token_current + 1];
}

fn eat_token(self: *Parser, tag: token.TokenType) ?Ast.TokenArrayIndex {
    if (self.is_current_token(tag)) {
        return self.next_token();
    } else {
        return null;
    }
}

fn eat_token_till(self: *Parser, tag: token.TokenType) Ast.TokenArrayIndex {
    while (!self.is_current_token(tag) and !self.is_current_token(.EOF)) {
        _ = self.next_token();
    }
    _ = self.eat_token(tag) orelse null;
    return self.token_current;
}

fn is_current_token(self: *Parser, tag: token.TokenType) bool {
    return self.token_current < self.token_ends.len and self.token_tags[self.token_current] == tag;
}

fn is_next_token(self: *Parser, tag: token.TokenType) bool {
    return self.token_current < self.token_ends.len - 1 and self.token_tags[self.token_current + 1] == tag;
}

fn get_token_literal(self: *Parser, tok_loc: Ast.TokenArrayIndex) []const u8 {
    const tok_start = self.token_starts[tok_loc];
    const tok_end = self.token_ends[tok_loc];
    return self.source_buffer[tok_start..tok_end];
}

fn print_last_error(self: *Parser) void {
    const len = self.errors.items.len;
    if (len > 0) {
        const err_node = self.errors.getLast();
        std.debug.print("Parsing error: {any}", .{err_node});
    }
}

fn register_integer_literal(self: *Parser) !u32 {
    std.debug.assert(self.is_current_token(.INT));
    const tok_start = self.token_starts[self.token_current];
    const tok_end = self.token_ends[self.token_current];
    const value = try std.fmt.parseInt(i64, self.source_buffer[tok_start..tok_end], 0);
    const location = @as(u32, @intCast(self.integer_literal_program_memory.items.len));
    try self.integer_literal_program_memory.append(self.allocator, value);
    return location;
}

pub fn print_parser_errors_to_stdout(ast: *const Ast, stdout: anytype) !void {
    for (ast.errors) |err| {
        const tok = ast.tokens.get(err.token);
        try stdout.print("Parser Errors! ({s}): At location {d} in the source code," ++
            " expected token of type {s} but instead got token of type {s}: {s}\n", .{
            @tagName(err.tag),
            tok.start, //
            @tagName(err.expected),
            @tagName(tok.tag),
            ast.source_buffer[tok.start..tok.end],
        });
    }
}

pub fn print_parser_errors_to_stderr(ast: *const Ast) !void {
    for (ast.errors) |err| {
        const tok = ast.tokens.get(err.token);
        std.debug.print("Parser Errors! ({s}): At location {d} in the source code," ++
            " expected token of type {s} but instead got token of type {s}: {s}\n", .{
            @tagName(err.tag),
            tok.start, //
            @tagName(err.expected),
            @tagName(tok.tag),
            ast.source_buffer[tok.start..tok.end],
        });
    }
}

test "parser_initialization_test_only_root_node" {
    const input = "\n";

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    try testing.expectEqual(ast.nodes.len, 1);
    try testing.expectEqual(ast.nodes.get(0).tag, .ROOT);
    try testing.expectEqual(ast.nodes.get(0).main_token, 0);
}

test "parser_test_var_decl" {
    const input =
        \\ const a = 10;
        \\ const b = true;
        \\ const c = false;
    ;

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 5,
            .expectedDataLHS = 5,
            .expectedDataRHS = 6,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 6,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BOOLEAN_LITERAL, //
            .expectedMainToken = 8,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 10,
            .expectedDataLHS = 8,
            .expectedDataRHS = 9,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 11,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BOOLEAN_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
    };

    const int_literals = [_]i64{10};
    const test_extras = [_]Ast.Node.NodeIndex{ 1, 4, 7 };
    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

test "parse_test_var_decl_errors" {
    const input =
        \\ const a 10;
        \\ const = 15;
        \\ var foobar = 1121414;
        \\ const b = ;
    ;

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);
    try testing.expectEqual(ast.errors.len, 3);

    const tests = [_]struct {
        expectedErrTag: Ast.Error.Tag, //
        expectedTokenIndex: Ast.TokenArrayIndex,
        expectedExpectedToken: token.TokenType,
    }{
        .{
            .expectedErrTag = .expected_assignent_after_var_decl, //
            .expectedTokenIndex = 2,
            .expectedExpectedToken = .ASSIGN,
        },
        .{
            .expectedErrTag = .expected_identifier_after_const, //
            .expectedTokenIndex = 5,
            .expectedExpectedToken = .IDENT,
        },
        .{
            .expectedErrTag = .expected_expression, //
            .expectedTokenIndex = 16,
            .expectedExpectedToken = .ILLEGAL,
        },
    };

    for (ast.errors, tests) |err, t| {
        try testing.expectEqual(err.token, t.expectedTokenIndex);
        try testing.expectEqual(err.tag, t.expectedErrTag);
        try testing.expectEqual(err.expected, t.expectedExpectedToken);
        // const tok = ast.tokens.get(err.token);
        // std.debug.print("Parser Errors! ({s}): At location {d} in the source code," ++
        //     " expected token of type {s} but instead got token of type {s}: {s}\n", .{
        //     @tagName(err.tag),
        //     tok.start, //
        //     @tagName(err.expected),
        //     @tagName(tok.tag),
        //     input[tok.start..tok.end],
        // });
    }
}

test "parser_test_return_stmt" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ const a = 10;
        \\ return a;
    ;

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 3,
            .expectedDataLHS = 4,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 4,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 6,
            .expectedDataLHS = 6,
            .expectedDataRHS = 7,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 7,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 9,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 11,
            .expectedDataLHS = 9,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 12,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
    };

    const int_literals = [_]i64{ 5, 10, 10 };

    const test_extras = [_]Ast.Node.NodeIndex{ 1, 3, 5, 8 };
    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

test "parser_test_assignement_stmt" {
    const input =
        \\ var b = 5;
        \\ b = fn(c) { c }
    ;

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = 5,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .ASSIGNMENT_STATEMENT, //
            .expectedMainToken = 6,
            .expectedDataLHS = 5,
            .expectedDataRHS = 11,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 9,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .FUNCTION_PARAMETER_BLOCK, //
            .expectedMainToken = 7,
            .expectedDataLHS = 6,
            .expectedDataRHS = 7,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 12,
            .expectedDataLHS = 9,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 12,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 11,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
        .{
            .expectedNodeType = .FUNCTION_EXPRESSION, //
            .expectedMainToken = 7,
            .expectedDataLHS = 7,
            .expectedDataRHS = 1,
        },
    };

    const test_extras = [_]Ast.Node.NodeIndex{ 8, 10, 1, 1, 4 };
    const int_literals = [_]i64{5};

    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

test "parser_test_identifer" {
    const input = "const foobar = 10; foobar;";
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 5,
            .expectedDataLHS = 5,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
    };

    const int_literals = [_]i64{10};

    const test_extras = [_]Ast.Node.NodeIndex{ 1, 4 };
    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

test "parser_test_int_literal" {
    const input = "15;";
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);

    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
    };

    const int_literals = [_]i64{15};

    const test_extras = [_]Ast.Node.NodeIndex{1};
    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

test "parser_test_prefix_operators" {
    const input =
        \\!15;
        \\-25;
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BOOL_NOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 3,
            .expectedDataLHS = 6,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 4,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .NEGATION, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = 0,
        },
    };

    const int_literals = [_]i64{ 15, 25 };

    const test_extras = [_]Ast.Node.NodeIndex{ 1, 4 };
    try parser_testing_test_extra(&ast, tests, test_extras, int_literals[0..], false);
}

// test "parser_test_infix_operators" {
//     const tests = [_]struct { input: [:0]const u8, expected: [:0]const u8 }{
//         .{
//             .input = "-a * b", //
//             .expected = "((-a) * b);",
//         },
//         .{
//             .input = "!-a",
//             .expected = "(!(-a));",
//         },
//         .{
//             .input = "a + b + c",
//             .expected = "((a + b) + c);",
//         },
//         .{
//             .input = "a + b - c",
//             .expected = "((a + b) - c);",
//         },
//         .{
//             .input = "a * b * c",
//             .expected = "((a * b) * c);",
//         },
//         .{
//             .input = "a * b / c",
//             .expected = "((a * b) / c);",
//         },
//         .{
//             .input = "a + b / c",
//             .expected = "(a + (b / c));",
//         },
//         .{
//             .input = "a + b * c + d / e - f",
//             .expected = "(((a + (b * c)) + (d / e)) - f);",
//         },
//         .{
//             .input = "3 + 4; -5 * 5",
//             .expected = "(3 + 4);((-5) * 5);",
//         },
//         .{
//             .input = "5 > 4 == 3 < 4",
//             .expected = "((5 > 4) == (3 < 4));",
//         },
//         .{
//             .input = "5 < 4 != 3 > 4",
//             .expected = "((5 < 4) != (3 > 4));",
//         },
//         .{
//             .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
//             .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
//         },
//         .{
//             .input = "true",
//             .expected = "true;",
//         },
//         .{
//             .input = "false",
//             .expected = "false;",
//         },
//         .{
//             .input = "3 > 5 == false",
//             .expected = "((3 > 5) == false);",
//         },
//         .{
//             .input = "3 < 5 == true",
//             .expected = "((3 < 5) == true);",
//         },
//         .{
//             .input = "1 + (2 + 3) + 4",
//             .expected = "((1 + (2 + 3)) + 4);",
//         },
//         .{
//             .input = "(5 + 5) * 2",
//             .expected = "((5 + 5) * 2);",
//         },
//         .{
//             .input = "2 / (5 + 5);",
//             .expected = "(2 / (5 + 5));",
//         },
//         .{
//             .input = "(5 + 5) * 2 * (5 + 5);",
//             .expected = "(((5 + 5) * 2) * (5 + 5));",
//         },
//         .{
//             .input = "-(5 + 5);",
//             .expected = "(-(5 + 5));",
//         },
//         .{
//             .input = "!(true == true);",
//             .expected = "(!(true == true));",
//         },
//         .{
//             .input = "a + add(b * c) + d",
//             .expected = "((a + add((b * c))) + d);",
//         },
//         .{
//             .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
//             .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
//         },
//         .{
//             .input = "add(a + b + c * d / f + g);",
//             .expected = "add((((a + b) + ((c * d) / f)) + g));",
//         },
//     };
//     for (tests, 0..) |t, i| {
//         _ = i;
//         var identifier_map = SymbolTable.init();
//         defer identifier_map.deinit(testing.allocator);
//         var ast = try Parser.parse_program(t.input, testing.allocator, &identifier_map);
//         defer ast.deinit(testing.allocator);
//         var outlist = std.ArrayList(u8).init(testing.allocator);
//         defer outlist.deinit();
//         try convert_ast_to_string(&ast, 1, &outlist);
//         outlist.shrinkRetainingCapacity(outlist.items.len);
//         try testing.expectEqualSlices(u8, t.expected[0..t.expected.len], outlist.allocatedSlice()[0..outlist.items.len]);
//         // std.debug.print("Index: {d}\r\n", .{i});
//         // std.debug.print("Source: {s}\r\n", .{t.input});
//         // std.debug.print("Parsed: {s}\r\n", .{outlist.allocatedSlice()[0..outlist.items.len]});
//         // std.debug.print("Parsed: {s}\r\n", .{t.expected});
//     }
// }

test "parser_test_if_else_block" {
    const input =
        \\ const a = if ( a < 10) {
        \\      var b = 10;
        \\ } else {
        \\      var b = 15;
        \\ };
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    try Parser.print_parser_errors_to_stderr(&ast);
    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 6,
            .expectedDataRHS = 7,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 14,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 7,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .LESS_THAN, //
            .expectedMainToken = 6,
            .expectedDataLHS = 3,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 10,
            .expectedDataLHS = 7,
            .expectedDataRHS = 8,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 11,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b110,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 9,
            .expectedDataLHS = 0,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 18,
            .expectedDataLHS = 11,
            .expectedDataRHS = 12,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 19,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b110,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 21,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 17,
            .expectedDataLHS = 2,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .IF_ELSE, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = 4,
        },
    };

    const test_extra_data = [_]u32{ 6, 1, 10, 1, 9, 13, 1 };
    const int_literals = [_]i64{ 10, 10, 15 };

    // symbol_tree.print_tree_to_stderr();
    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

test "parser_test_naked_if" {
    const input =
        \\ const a = if ( a < 10) {
        \\      var b = 10;
        \\ };
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    try Parser.print_parser_errors_to_stderr(&ast);
    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 10,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 7,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .LESS_THAN, //
            .expectedMainToken = 6,
            .expectedDataLHS = 3,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 10,
            .expectedDataLHS = 7,
            .expectedDataRHS = 8,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 11,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b110,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 9,
            .expectedDataLHS = 0,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .NAKED_IF, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = 9,
        },
    };

    const test_extra_data = [_]u32{ 6, 1, 1 };

    const int_literals = [_]i64{ 10, 10 };

    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

test "parser_array_expression" {
    const input =
        \\const three = 3;
        \\[1 , "two", three][1 * 2]
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = 5,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 5,
            .expectedDataLHS = 12,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 6,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .STRING_LITERAL, //
            .expectedMainToken = 8,
            .expectedDataLHS = 23,
            .expectedDataRHS = 26,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 10,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .ARRAY_LITERAL, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 15,
            .expectedDataLHS = 3,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .MULTIPLY, //
            .expectedMainToken = 14,
            .expectedDataLHS = 9,
            .expectedDataRHS = 10,
        },
        .{
            .expectedNodeType = .INDEX_INTO, //
            .expectedMainToken = 12,
            .expectedDataLHS = 8,
            .expectedDataRHS = 11,
        },
    };

    const test_extra_data = [_]u32{ 5, 6, 7, 1, 4 };

    const int_literals = [_]i64{ 3, 1, 1, 2 };

    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

test "parser_test_function_expression" {
    const input =
        \\ fn(a, b) {
        \\    return a + b;
        \\ }
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 10,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 2,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 4,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .FUNCTION_PARAMETER_BLOCK, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 7,
            .expectedDataLHS = 8,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 8,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 10,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0b010,
        },
        .{
            .expectedNodeType = .ADDITION, //
            .expectedMainToken = 9,
            .expectedDataLHS = 6,
            .expectedDataRHS = 7,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 6,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
        .{
            .expectedNodeType = .FUNCTION_EXPRESSION, //
            .expectedMainToken = 0,
            .expectedDataLHS = 4,
            .expectedDataRHS = 1,
        },
    };

    const test_extra_data = [_]u32{ 5, 9, 2, 1 };

    const int_literals = [_]i64{};

    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

test "parser_test_function_empty_param" {
    const input =
        \\ fn() {
        \\    return 10;
        \\ }
    ;
    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = 4,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 5,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 4,
            .expectedDataLHS = 3,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 5,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
        .{
            .expectedNodeType = .FUNCTION_EXPRESSION, //
            .expectedMainToken = 0,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
    };

    const test_extra_data = [_]u32{ 2, 4, 0, 1 };

    const int_literals = [_]i64{10};

    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

test "parser_test_function_call_expr" {
    const input =
        \\ const a = 10;
        \\ const b = 20;
        \\ a(1, 2, b(3, 4));
    ;

    var symbol_tree = SymbolTree.init(testing.allocator);
    defer symbol_tree.deinit();
    var ast = try Parser.parse_program(input, testing.allocator, &symbol_tree);
    defer ast.deinit(testing.allocator);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 9,
            .expectedDataRHS = 12,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = 3,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 1,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .VAR_STATEMENT, //
            .expectedMainToken = 5,
            .expectedDataLHS = 5,
            .expectedDataRHS = 6,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 6,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 8,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 10,
            .expectedDataLHS = 15,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 10,
            .expectedDataLHS = 0,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 12,
            .expectedDataLHS = 2,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 14,
            .expectedDataLHS = 3,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 16,
            .expectedDataLHS = 1,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 18,
            .expectedDataLHS = 4,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 20,
            .expectedDataLHS = 5,
            .expectedDataRHS = 0,
        },
        .{
            .expectedNodeType = .FUNCTION_CALL, //
            .expectedMainToken = 17,
            .expectedDataLHS = 11,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .FUNCTION_CALL, //
            .expectedMainToken = 11,
            .expectedDataLHS = 8,
            .expectedDataRHS = 7,
        },
    };

    const test_extra_data = [_]u32{ 12, 13, 0, 2, 9, 10, 14, 4, 7, 1, 4, 7 };

    const int_literals = [_]i64{ 10, 20, 1, 2, 3, 4 };

    try parser_testing_test_extra(&ast, tests, test_extra_data, int_literals[0..], false);
}

fn parser_testing_test_ast(ast: *Ast, test_nodes: anytype, int_literals: []const i64, enable_debug: bool) !void {
    try testing.expectEqual(test_nodes.len, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    try testing.expectEqual(int_literals.len, ast.integer_literals.len);
    try testing.expectEqualSlices(i64, int_literals[0..], ast.integer_literals);

    try testing_check_nodes(ast, test_nodes, enable_debug);
}

fn parser_testing_test_extra(
    ast: *Ast,
    test_nodes: anytype,
    test_extras: anytype,
    int_literals: []const i64,
    enable_debug: bool,
) !void {
    if (enable_debug) {
        ast.print_to_stderr();
    }
    try testing.expectEqual(test_nodes.len, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);
    try testing.expectEqual(test_extras.len, ast.extra_data.len);
    try testing.expectEqualSlices(u32, &test_extras, ast.extra_data[0..ast.extra_data.len]);
    try testing.expectEqual(int_literals.len, ast.integer_literals.len);
    try testing.expectEqualSlices(i64, int_literals[0..], ast.integer_literals);

    try testing_check_nodes(ast, test_nodes, enable_debug);
}

fn testing_check_nodes(ast: *Ast, tests: anytype, enable_debug: bool) !void {
    for (0..ast.nodes.len) |node| {
        const n = ast.nodes.get(node);
        if (enable_debug) {
            std.debug.print("Nodes: {any}\r\n", .{n});
        }
        try testing.expectEqual(tests[node].expectedNodeType, n.tag);
        try testing.expectEqual(tests[node].expectedMainToken, n.main_token);
        try testing.expectEqual(tests[node].expectedDataLHS, n.node_data.lhs);
        try testing.expectEqual(tests[node].expectedDataRHS, n.node_data.rhs);
    }
}

const std = @import("std");
const testing = std.testing;

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;
const convert_ast_to_string = @import("evaluator.zig").convert_ast_to_string;
const SymbolTable = @import("symbol_table.zig");
const SymbolTree = @import("symbol_tree.zig");
