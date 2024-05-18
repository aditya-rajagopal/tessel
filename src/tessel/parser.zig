pub const Parser = @This();

source_buffer: [:0]const u8,
allocator: std.mem.Allocator,
tokens_tags: []const token.TokenType,
tokens_start: []const u32,
tokens_end: []const u32,

token_current: Ast.TokenArrayIndex,

nodes: Ast.NodeArrayType,

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
        .tokens_tags = tokens_local.items(.tag),
        .tokens_start = tokens_local.items(.start),
        .tokens_end = tokens_local.items(.end),
        .token_current = 0,
        .nodes = .{},
    };
    defer parser.deinit(allocator);

    const estimated_nodes = tokens_local.len / 2 + 2;
    try parser.nodes.ensureTotalCapacity(allocator, estimated_nodes);

    try parser.begin_parsing();

    return Ast{
        .tokens = tokens_local.toOwnedSlice(), //
        .nodes = parser.nodes.toOwnedSlice(),
    };
}

pub fn deinit(self: *Parser, allocator: Allocator) void {
    self.nodes.deinit(allocator);
}

pub fn begin_parsing(self: *Parser) !void {
    self.nodes.appendAssumeCapacity(.{
        .tag = .ROOT, //
        .main_token = 0,
        .node_data = undefined,
    });
}

test "parser_initialization_test_only_root_node" {
    const input = "a";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var ast = try Parser.parse_program(input, allocator);
    try testing.expectEqual(ast.nodes.len, 1);
    try testing.expectEqual(ast.nodes.get(0).tag, .ROOT);
    try testing.expectEqual(ast.nodes.get(0).main_token, 0);
}

const std = @import("std");
const testing = std.testing;

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;
