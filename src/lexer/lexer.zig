const std = @import("std");
const print = std.debug.print;
const testing = std.testing;

pub const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0, // Current position in the input (current character).
    read_head_position: usize = 0, // Current position of the read head.
    ch_current: u8 = 0, // Current character under examination.

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        l.read_next_character();
        return l;
    }

    pub fn next_token(self: *Lexer) token.Token {
        self.skip_white_spaces();
        const tok = switch (self.ch_current) {
            '=' => eq_tok: {
                const ch = self.peak_next_character();
                switch (ch) {
                    '=' => {
                        self.read_next_character();
                        break :eq_tok token.Token.init(.EQ, self.input[self.position - 1 .. self.position + 1]);
                    },
                    inline else => break :eq_tok token.Token.init(.ASSIGN, self.input[self.position .. self.position + 1]),
                }
            },
            '+' => token.Token.init(.PLUS, self.input[self.position .. self.position + 1]),
            '-' => token.Token.init(.MINUS, self.input[self.position .. self.position + 1]),
            '!' => bang_tok: {
                const ch = self.peak_next_character();
                switch (ch) {
                    '=' => {
                        self.read_next_character();
                        break :bang_tok token.Token.init(.NEQ, self.input[self.position - 1 .. self.position + 1]);
                    },
                    inline else => break :bang_tok token.Token.init(.BANG, self.input[self.position .. self.position + 1]),
                }
            },
            '*' => token.Token.init(.ASTRIX, self.input[self.position .. self.position + 1]),
            '/' => token.Token.init(.SLASH, self.input[self.position .. self.position + 1]),

            '<' => lt_tok: {
                const ch = self.peak_next_character();
                switch (ch) {
                    '=' => {
                        self.read_next_character();
                        break :lt_tok token.Token.init(.LTE, self.input[self.position - 1 .. self.position + 1]);
                    },
                    inline else => break :lt_tok token.Token.init(.LT, self.input[self.position .. self.position + 1]),
                }
            },
            '>' => gt_tok: {
                const ch = self.peak_next_character();
                switch (ch) {
                    '=' => {
                        self.read_next_character();
                        break :gt_tok token.Token.init(.GTE, self.input[self.position - 1 .. self.position + 1]);
                    },
                    else => break :gt_tok token.Token.init(.GT, self.input[self.position .. self.position + 1]),
                }
            },

            '(' => token.Token.init(.LPAREN, self.input[self.position .. self.position + 1]),
            ')' => token.Token.init(.RPAREN, self.input[self.position .. self.position + 1]),
            '{' => token.Token.init(.LBRACE, self.input[self.position .. self.position + 1]),
            '}' => token.Token.init(.RBRACE, self.input[self.position .. self.position + 1]),
            ',' => token.Token.init(.COMMA, self.input[self.position .. self.position + 1]),
            ';' => token.Token.init(.SEMICOLON, self.input[self.position .. self.position + 1]),
            0 => token.Token.init(.EOF, ""),
            else => |ch| else_tok: {
                if (is_letter(ch)) {
                    var t: token.Token = undefined;
                    const start_pos = self.position;
                    self.read_identifier();
                    t.literal = self.input[start_pos..self.position];
                    t.type = token.Keywords.get(t.literal) orelse .IDENT;
                    return t;
                } else {
                    if (is_digit(ch)) {
                        var t: token.Token = undefined;
                        const start_pos = self.position;
                        self.read_number_literal();
                        t.literal = self.input[start_pos..self.position];
                        t.type = .INT;
                        return t;
                    } else {
                        break :else_tok token.Token.init(.ILLEGAL, self.input[self.position .. self.position + 1]);
                    }
                }
            },
        };
        self.read_next_character();
        return tok;
    }

    pub fn print_debug_tokens(self: *Lexer, stdout: anytype) !void {
        var tok = self.next_token();
        try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
        try stdout.print("{s:<15}|{s}\n", .{ "Token.TokenType", "[]u8 literal" });
        try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
        while (tok.type != .EOF) : (tok = self.next_token()) {
            try stdout.print("{s:<15}| {s}\n", .{ @tagName(tok.type), tok.literal });
            try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
        }
    }

    fn read_next_character(self: *Lexer) void {
        if (self.read_head_position >= self.input.len) {
            self.ch_current = 0;
        } else {
            self.ch_current = self.input[self.read_head_position];
        }
        self.position = self.read_head_position;
        self.read_head_position += 1;
    }

    fn peak_next_character(self: *Lexer) u8 {
        if (self.read_head_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_head_position];
        }
    }

    fn is_letter(ch: u8) bool {
        return (('a' <= ch) and (ch <= 'z')) or (('A' <= ch) and (ch <= 'Z')) or ch == '_';
    }

    fn is_digit(ch: u8) bool {
        return ('0' <= ch) and (ch <= '9');
    }

    fn read_identifier(self: *Lexer) void {
        while (is_letter(self.ch_current)) {
            self.read_next_character();
        }
    }

    fn read_number_literal(self: *Lexer) void {
        while (is_digit(self.ch_current)) {
            self.read_next_character();
        }
    }

    fn skip_white_spaces(self: *Lexer) void {
        while (self.ch_current == ' ' or self.ch_current == '\t' or self.ch_current == '\n' or self.ch_current == '\r') {
            self.read_next_character();
        }
    }
};

test "test_lexing" {
    const input =
        \\ const five = 5;
        \\ var ten = 10;
        \\
        \\ const add = fn(x, y) {
        \\     x + y;
        \\ };
        \\ 
        \\ const result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\
        \\ if ( 5 < 10 ) {
        \\     return true;
        \\ } else {
        \\     return false;
        \\ }
        \\
        \\ 10 == 10;
        \\ 10 != 9;
        \\ 10 >= 10;
        \\ 9 <= 10;
    ;

    const tests = [_]struct { expectedType: token.TokenType, expectedLiteral: []const u8 }{
        .{ .expectedType = .CONST, .expectedLiteral = "const" },
        .{ .expectedType = .IDENT, .expectedLiteral = "five" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .VAR, .expectedLiteral = "var" },
        .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .CONST, .expectedLiteral = "const" },
        .{ .expectedType = .IDENT, .expectedLiteral = "add" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .FUNCTION, .expectedLiteral = "fn" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .IDENT, .expectedLiteral = "x" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .IDENT, .expectedLiteral = "y" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .IDENT, .expectedLiteral = "x" },
        .{ .expectedType = .PLUS, .expectedLiteral = "+" },
        .{ .expectedType = .IDENT, .expectedLiteral = "y" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .CONST, .expectedLiteral = "const" },
        .{ .expectedType = .IDENT, .expectedLiteral = "result" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .IDENT, .expectedLiteral = "add" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .IDENT, .expectedLiteral = "five" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .BANG, .expectedLiteral = "!" },
        .{ .expectedType = .MINUS, .expectedLiteral = "-" },
        .{ .expectedType = .SLASH, .expectedLiteral = "/" },
        .{ .expectedType = .ASTRIX, .expectedLiteral = "*" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .GT, .expectedLiteral = ">" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .IF, .expectedLiteral = "if" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .RETURN, .expectedLiteral = "return" },
        .{ .expectedType = .TRUE, .expectedLiteral = "true" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .ELSE, .expectedLiteral = "else" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .RETURN, .expectedLiteral = "return" },
        .{ .expectedType = .FALSE, .expectedLiteral = "false" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .EQ, .expectedLiteral = "==" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .NEQ, .expectedLiteral = "!=" },
        .{ .expectedType = .INT, .expectedLiteral = "9" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .GTE, .expectedLiteral = ">=" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .INT, .expectedLiteral = "9" },
        .{ .expectedType = .LTE, .expectedLiteral = "<=" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    };
    var lex = Lexer.init(input);

    for (tests) |t| {
        const tok = lex.next_token();
        try testing.expectEqual(t.expectedType, tok.type);
        try testing.expectEqualStrings(t.expectedLiteral, tok.literal);
    }
}
