const std = @import("std");

/// Contains all the types of token currently accepted by the language
pub const TokenType = enum {
    ILLEGAL,
    EOF,

    IDENT,

    // Literals
    INT,
    FLOAT,
    STRING,

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
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    CONST,
    VAR,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
    WHILE,
    BREAK,
    CONTINUE,
};

/// Map of all valid keywords in tessel created at compiletime
pub const Keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .FUNCTION },
    .{ "const", .CONST },
    .{ "var", .VAR },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "while", .WHILE },
    .{ "break", .BREAK },
    .{ "continue", .CONTINUE },
});

/// Storage structure for tokens
pub const Token = struct {
    /// Type of token
    type: TokenType,
    /// The string literal that the token represents
    loc: Location,

    pub const Location = struct { start: usize, end: usize };
};

test "keywords" {
    var func = Keywords.get("fn") orelse .IDENT;
    try std.testing.expectEqual(.FUNCTION, func);
    func = Keywords.get("bool") orelse .IDENT;
    try std.testing.expectEqual(.IDENT, func);
}
