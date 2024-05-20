pub const Parser = @This();

source_buffer: [:0]const u8,
allocator: std.mem.Allocator,
token_tags: []const token.TokenType,
token_starts: []const u32,
token_ends: []const u32,

token_current: Ast.TokenArrayIndex,

nodes: Ast.NodeArrayType,
extra_data: std.ArrayListUnmanaged(Ast.Node.NodeIndex),
errors: std.ArrayListUnmanaged(Ast.Error),

/// Since the root node does not have any data and is always at 0 we can use 0 as a null value
pub const null_node: Ast.Node.NodeIndex = 0;

pub const Error = error{ParsingError} || Allocator.Error;

pub fn parse_program(source_buffer: [:0]const u8, allocator: std.mem.Allocator) !Ast {
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

    var parser = Parser{
        .source_buffer = source_buffer, //
        .allocator = allocator,
        .token_tags = tokens_local.items(.tag),
        .token_starts = tokens_local.items(.start),
        .token_ends = tokens_local.items(.end),
        .token_current = 0,
        .nodes = .{},
        .errors = .{},
        .extra_data = .{},
    };
    defer parser.deinit(allocator);

    const estimated_nodes = tokens_local.len / 2 + 2;
    try parser.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    try parser.begin_parsing();

    return Ast{
        .source_buffer = source_buffer,
        .tokens = tokens_local.toOwnedSlice(), //
        .nodes = parser.nodes.toOwnedSlice(),
        .errors = try parser.errors.toOwnedSlice(allocator),
        .extra_data = try parser.extra_data.toOwnedSlice(allocator),
    };
}

pub fn deinit(self: *Parser, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    self.errors.deinit(allocator);
    self.extra_data.deinit(allocator);
}

pub fn begin_parsing(self: *Parser) !void {
    self.nodes.appendAssumeCapacity(.{
        .tag = .ROOT, //
        .main_token = 0,
        .node_data = undefined,
    });

    while (true) {
        // Lexer.print_token(self.source_buffer, .{
        //     .type = self.token_tags[self.token_current], //
        //     .loc = .{
        //         .start = self.token_starts[self.token_current], //
        //         .end = self.token_ends[self.token_current],
        //     },
        // }, "Parser#begin_parsing ");
        _ = try self.parse_statement();
        if (self.is_current_token(.EOF)) {
            break;
        }
        if (self.token_current >= self.token_tags.len) {
            break;
        }
    }
}

fn parse_statement(self: *Parser) !Ast.Node.NodeIndex {
    switch (self.token_tags[self.token_current]) {
        .EOF => return null_node,
        .CONST, .VAR => {
            return self.parse_var_decl() catch |err| switch (err) {
                Error.ParsingError => return null_node,
                else => return err,
            };
        },
        .RETURN => {
            return self.parse_return_statement() catch |err| switch (err) {
                Error.ParsingError => return null_node,
                else => return err,
            };
        },
        else => escape: {
            const expr_node = try self.add_node(.{
                .tag = .EXPRESSION_STATEMENT, //
                .main_token = self.token_current,
                .node_data = undefined,
            });
            const node = self.parse_expression() catch |err| switch (err) {
                Error.ParsingError => break :escape,
                else => return err,
            };
            self.nodes.items(.node_data)[expr_node].lhs = node;
            return expr_node;
        },
    }
    return null_node;
}

fn parse_var_decl(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.add_node(.{
        .tag = Ast.Node.Tag.VAR_STATEMENT, //
        .main_token = self.next_token(),
        .node_data = undefined,
    });

    // After a const or var statement you expect a identifier
    if (self.is_current_token(.IDENT)) {
        self.nodes.items(.node_data)[node].lhs = try self.add_node(.{
            .tag = .IDENTIFIER, //
            .main_token = self.next_token(),
            .node_data = .{
                .lhs = undefined, //
                .rhs = undefined,
            },
        });
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

    const rhs = try self.parse_expression();
    if (rhs == 0) {
        return node;
    }
    self.nodes.items(.node_data)[node].rhs = rhs;
    return node;
}

fn parse_return_statement(self: *Parser) Error!Ast.Node.NodeIndex {
    const node = try self.add_node(.{
        .tag = .RETURN_STATEMENT, //
        .main_token = self.next_token(),
        .node_data = .{ .lhs = undefined, .rhs = undefined },
    });

    const lhs = try self.parse_expression();
    if (lhs == 0) {
        return node;
    }
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

    .FUNCTION = .{ .precedence = 110, .tag = .FUNCTION_CALL },
});

fn parse_expression(self: *Parser) Error!Ast.Node.NodeIndex {
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
    // std.debug.print("Enter parse Expression prec: {d} token: {s}\r\n", .{ min_presedence, @tagName(self.token_tags[self.token_current]) });
    var cur_node = try self.parse_prefix();
    if (cur_node == null_node) {
        return null_node;
    }
    while (true) {
        const current_tag = self.token_tags[self.token_current];
        // std.debug.print("Entering while loop token: {s}\r\n", .{@tagName(current_tag)});
        if (current_tag == .SEMICOLON or current_tag == .EOF) {
            // std.debug.print("breaking\r\n", .{});
            break;
        }
        const operator_info: OperatorPrecedence = OperatorHierarchy[@as(usize, @intCast(@intFromEnum(current_tag)))];
        if (operator_info.precedence < min_presedence) {
            break;
        }

        const operator_token = self.next_token();

        const rhs = try self.parse_expression_precedence(operator_info.precedence + 1);
        if (rhs == 0) {
            try self.add_error(.{
                .tag = .expected_expression, //
                .token = self.token_current,
                .expected = .ILLEGAL,
            });
            return cur_node;
        }

        // std.debug.print("I am reaching here for some reason tag: {s}\r\n", .{@tagName(current_tag)});
        cur_node = try self.add_node(.{
            .tag = operator_info.tag, //
            .main_token = operator_token,
            .node_data = .{
                .lhs = cur_node, //
                .rhs = rhs,
            },
        });
    }
    _ = self.eat_token(.SEMICOLON);
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
        .IDENT => return self.add_node(.{
            .tag = .IDENTIFIER, //
            .main_token = self.next_token(),
            .node_data = .{ .lhs = undefined, .rhs = undefined },
        }),
        .INT => return self.add_node(.{
            .tag = .INTEGER_LITERAL, //
            .main_token = self.next_token(),
            .node_data = .{ .lhs = undefined, .rhs = undefined },
        }),
        .TRUE, .FALSE => return self.add_node(.{
            .tag = .BOOLEAN_LITERAL, //
            .main_token = self.next_token(),
            .node_data = .{ .lhs = undefined, .rhs = undefined },
        }),
        .LPAREN => {
            _ = self.next_token();
            const node = try self.parse_expression_precedence(0);
            _ = try self.expect_token(.RPAREN);
            return node;
        },
        .IF => return self.parse_if_expression(),
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

fn parse_if_expression(self: *Parser) Error!Ast.Node.NodeIndex {
    // We sould only come here if we have an if token
    std.debug.assert(self.token_tags[self.token_current] == .IF);
    const if_token = self.token_current;
    _ = try self.expect_token(.IF);
    _ = try self.expect_token(.LPAREN);

    const condition = try self.parse_expression();
    if (condition == null_node) {
        return null_node;
    }
    _ = try self.expect_token(.RPAREN);

    const if_block = try self.parse_block();
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

    const else_block = try self.parse_block();
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

fn parse_block(self: *Parser) Error!Ast.Node.NodeIndex {
    const left_brace = try self.expect_token(.LBRACE);
    const start_point = self.extra_data.items.len;
    // We will keep looping through statements till we encounter a closing brace
    while (true) {
        if (self.is_current_token(.RBRACE)) {
            break;
        }
        const statement = try self.parse_statement();
        try self.extra_data.append(self.allocator, statement);
    }
    const end_point = self.extra_data.items.len;
    _ = self.eat_token(.RBRACE) orelse null;

    return self.add_node(.{
        .tag = .BLOCK, //
        .main_token = left_brace,
        .node_data = .{
            .lhs = @as(u32, @intCast(start_point)), //
            .rhs = @as(u32, @intCast(end_point)),
        },
    });
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

    var ast = try Parser.parse_program(input, testing.allocator);
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

    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);
    // try testing.expectEqual(10, ast.nodes.len);
    // try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 3,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BOOLEAN_LITERAL, //
            .expectedMainToken = 8,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BOOLEAN_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
    };

    try testing_check_nodes(&ast, tests, false);
}

test "parse_test_var_decl_errors" {
    const input =
        \\ const a 10;
        \\ const = 15;
        \\ var foobar = 1121414;
        \\ const b = ;
    ;

    var ast = try Parser.parse_program(input, testing.allocator);
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
        \\ return a;
    ;

    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);
    try testing.expectEqual(ast.nodes.len, 7);
    try testing.expectEqual(ast.errors.len, 0);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 1,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 3,
            .expectedDataLHS = 4,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 4,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .RETURN_STATEMENT, //
            .expectedMainToken = 6,
            .expectedDataLHS = 6,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 7,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
    };

    try testing_check_nodes(&ast, tests, false);
}

test "parser_test_identifer" {
    const input = "foobar;";
    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);
    try testing.expectEqual(3, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
    };

    try testing_check_nodes(&ast, tests, false);
}

test "parser_test_int_literal" {
    const input = "15;";
    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);
    try testing.expectEqual(3, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
    };

    try testing_check_nodes(&ast, tests, false);
}

test "parser_test_prefix_operators" {
    const input =
        \\!15;
        \\-25;
    ;
    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);
    try testing.expectEqual(7, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 3,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 1,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BOOL_NOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = 2,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .EXPRESSION_STATEMENT, //
            .expectedMainToken = 3,
            .expectedDataLHS = 6,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 4,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .NEGATION, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = undefined,
        },
    };

    try testing_check_nodes(&ast, tests, false);
}

test "parser_test_infix_operators" {
    const tests = [_]struct { input: [:0]const u8, expected: [:0]const u8 }{
        .{
            .input = "-a * b", //
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        // .{
        //     .input = "3 + 4; -5 * 5",
        //     .expected = "(3 + 4)((-5) * 5)",
        // },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "(5 + 5) * 2 * (5 + 5)",
            .expected = "(((5 + 5) * 2) * (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
        // .{
        //     .input = "a + add(b * c) + d",
        //     .expected = "((a + add((b * c))) + d)",
        // },
        // .{
        // .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        // .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        // },
        // .{
        // .input = "add(a + b + c * d / f + g)",
        // .expected = "add((((a + b) + ((c * d) / f)) + g))",
        // },
    };
    for (tests, 0..) |t, i| {
        _ = i;
        var ast = try Parser.parse_program(t.input, testing.allocator);
        defer ast.deinit(testing.allocator);
        var outlist = std.ArrayList(u8).init(testing.allocator);
        defer outlist.deinit();
        try convert_ast_to_string(&ast, ast.nodes.len - 1, &outlist);
        outlist.shrinkRetainingCapacity(outlist.items.len);
        try testing.expectEqualSlices(u8, t.expected[0..t.expected.len], outlist.allocatedSlice()[0..outlist.items.len]);
        // std.debug.print("Index: {d}\r\n", .{i});
        // std.debug.print("Source: {s}\r\n", .{t.input});
        // std.debug.print("Parsed: {s}\r\n", .{outlist.allocatedSlice()[0..outlist.items.len]});
        // std.debug.print("Parsed: {s}\r\n", .{t.expected});
    }
}

test "parser_test_if_else_block" {
    const input =
        \\ const a = if ( a < b) {
        \\      var b = 10;
        \\ } else {
        \\      var b = 15;
        \\ }
    ;
    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);

    try testing.expectEqual(15, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 7,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 9,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 21,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 17,
            .expectedDataLHS = 1,
            .expectedDataRHS = 2,
        },
        .{
            .expectedNodeType = .IF_ELSE, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = 2,
        },
    };

    const test_extra_data = [_]u32{ 6, 10, 9, 13 };

    try testing.expectEqual(test_extra_data.len, ast.extra_data.len);
    try testing.expectEqualSlices(u32, ast.extra_data[0..ast.extra_data.len], &test_extra_data);

    try testing_check_nodes(&ast, tests, false);
}

test "parser_test_naked_if" {
    const input =
        \\ const a = if ( a < b) {
        \\      var b = 10;
        \\ }
    ;
    var ast = try Parser.parse_program(input, testing.allocator);
    defer ast.deinit(testing.allocator);

    try testing.expectEqual(11, ast.nodes.len);
    try testing.expectEqual(0, ast.errors.len);

    const tests = [_]struct {
        expectedNodeType: Ast.Node.Tag, //
        expectedMainToken: Ast.TokenArrayIndex,
        expectedDataLHS: Ast.Node.NodeIndex,
        expectedDataRHS: Ast.Node.NodeIndex,
    }{
        .{
            .expectedNodeType = .ROOT, //
            .expectedMainToken = 0,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 5,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .IDENTIFIER, //
            .expectedMainToken = 7,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
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
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .INTEGER_LITERAL, //
            .expectedMainToken = 13,
            .expectedDataLHS = undefined,
            .expectedDataRHS = undefined,
        },
        .{
            .expectedNodeType = .BLOCK, //
            .expectedMainToken = 9,
            .expectedDataLHS = 0,
            .expectedDataRHS = 1,
        },
        .{
            .expectedNodeType = .NAKED_IF, //
            .expectedMainToken = 3,
            .expectedDataLHS = 5,
            .expectedDataRHS = 9,
        },
    };

    const test_extra_data = [_]u32{6};

    try testing.expectEqual(test_extra_data.len, ast.extra_data.len);
    try testing.expectEqualSlices(u32, ast.extra_data[0..ast.extra_data.len], &test_extra_data);

    try testing_check_nodes(&ast, tests, false);
}

pub fn convert_ast_to_string(ast: *Ast, root_node: usize, list: *std.ArrayList(u8)) !void {
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
            const tok = ast.tokens.get(node.main_token);
            const literal = ast.source_buffer[tok.start..tok.end];
            try list.appendSlice(literal);
        },
        .NEGATION, .BOOL_NOT => {
            try list.append('(');
            try list.appendSlice(Ast.Node.Tag.get_operator_string(node.tag));
            try convert_ast_to_string(ast, node.node_data.lhs, list);
            try list.append(')');
        },
        else => {},
    }
}

fn testing_check_nodes(ast: *Ast, tests: anytype, enable_debug: bool) !void {
    for (0..ast.nodes.len) |node| {
        const n = ast.nodes.get(node);
        if (enable_debug) {
            std.debug.print("Nodes: {any}\r\n", .{n});
        }
        try testing.expectEqual(n.tag, tests[node].expectedNodeType);
        try testing.expectEqual(n.main_token, tests[node].expectedMainToken);
        try testing.expectEqual(n.node_data.lhs, tests[node].expectedDataLHS);
        try testing.expectEqual(n.node_data.rhs, tests[node].expectedDataRHS);
    }
}

const std = @import("std");
const testing = std.testing;

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;
