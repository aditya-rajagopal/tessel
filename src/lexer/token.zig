// pub const TokenType = ?[*:0]u8;
const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    IDENT,

    // Literals
    INT,
    FLOAT,

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTRIX,
    SLASH,

    // Comparisons
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    CONST,
    VAR,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
};

pub const Keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .FUNCTION },
    .{ "const", .CONST },
    .{ "var", .VAR },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
});

pub const Token = struct {
    type: TokenType,
    literal: []const u8,

    pub fn init(token_type: TokenType, token_literal: []const u8) Token {
        return .{ .type = token_type, .literal = token_literal };
    }
};

test "keywords" {
    var func = Keywords.get("fn") orelse .IDENT;
    try std.testing.expectEqual(.FUNCTION, func);
    func = Keywords.get("bool") orelse .IDENT;
    try std.testing.expectEqual(.IDENT, func);
}
