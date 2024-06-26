/// A utility to hold a string of data and allows you to tokenize a provided string
pub const Lexer = @This();

/// Storage for the data we want to create tokens for
///
/// This is not meant to be accessed directly and it is to be provided when
/// the Lexer is initialized
input: [:0]const u8,
/// Current position in the input (current character).
position: u32 = 0,
/// Current position of the read head.
read_head_position: u32 = 0,
/// Current character under examination.
ch_current: u8 = 0,

/// Accepts a string and returns an intialized lexer that is ready
/// to start generating tokens with the next_token or get_token_list functions
pub fn init(input: [:0]const u8) Lexer {
    std.debug.assert(input.len > 0);
    var l = Lexer{ .input = input };
    l.read_next_character();
    return l;
}

/// Handy debug function to print a token to stderr
pub fn print_token(source_string: []const u8, tok: token.Token, debug_tag: []const u8) void {
    std.debug.assert(tok.loc.start <= source_string.len);
    std.debug.assert(tok.loc.end <= source_string.len);
    std.debug.print("{s}: {s:<15}| {s}, start:{d}\n", .{ debug_tag, @tagName(tok.type), source_string[tok.loc.start..tok.loc.end], tok.loc.start });
}

/// Parses the current string being read in the lexer and returns a token based on
/// the language rules
///
/// TODO(aditya): Make the rules something you can define elsewhere and pass to the lexer
pub fn next_token(self: *Lexer) token.Token {
    self.skip_white_spaces();

    var tok = token.Token{ .type = .EOF, .loc = .{ .start = self.position, .end = undefined } };

    switch (self.ch_current) {
        '=' => {
            const ch = self.peak_next_character();
            switch (ch) {
                '=' => {
                    self.read_next_character();
                    tok.type = .EQ;
                },
                else => {
                    tok.type = .ASSIGN;
                },
            }
        },
        '+' => tok.type = .PLUS,
        '-' => tok.type = .MINUS,
        '!' => {
            const ch = self.peak_next_character();
            switch (ch) {
                '=' => {
                    self.read_next_character();
                    tok.type = .NEQ;
                },
                inline else => tok.type = .BANG,
            }
        },
        '*' => tok.type = .ASTRIX,
        '/' => {
            const ch = self.peak_next_character();
            switch (ch) {
                '/' => {
                    // Skip comments
                    self.read_until('\n');
                    const t = self.next_token();
                    tok.type = t.type;
                    tok.loc = t.loc;
                    return tok;
                },
                else => tok.type = .SLASH,
            }
        },
        '<' => {
            const ch = self.peak_next_character();
            switch (ch) {
                '=' => {
                    self.read_next_character();
                    tok.type = .LTE;
                },
                inline else => tok.type = .LT,
            }
        },
        '>' => {
            const ch = self.peak_next_character();
            switch (ch) {
                '=' => {
                    self.read_next_character();
                    tok.type = .GTE;
                },
                else => tok.type = .GT,
            }
        },

        '(' => tok.type = .LPAREN,
        ')' => tok.type = .RPAREN,
        '{' => tok.type = .LBRACE,
        '}' => tok.type = .RBRACE,
        '[' => tok.type = .LBRACKET,
        ']' => tok.type = .RBRACKET,
        ',' => tok.type = .COMMA,
        ';' => tok.type = .SEMICOLON,
        ':' => tok.type = .COLON,
        '"' => {
            self.read_next_character();
            tok.type = .STRING;
            tok.loc.start = self.position;
            self.read_string_literal();
            tok.loc.end = self.position;
            if (self.ch_current == '"') {
                self.read_next_character();
            }
            return tok;
        },
        0 => {
            tok.loc.end = self.position;
            return tok;
        },
        else => |ch| {
            if (is_letter(ch)) {
                tok.loc.start = self.position;
                self.read_identifier();
                tok.loc.end = self.position;
                tok.type = token.Keywords.get(self.input[tok.loc.start..tok.loc.end]) orelse .IDENT;
                return tok;
            } else {
                if (is_digit(ch)) {
                    tok.loc.start = self.position;
                    const is_float = self.read_number_literal();
                    tok.loc.end = self.position;
                    tok.type = if (is_float) .FLOAT else .INT;
                    return tok;
                } else {
                    tok.type = .ILLEGAL;
                }
            }
        },
    }
    self.read_next_character();
    tok.loc.end = self.position;
    return tok;
}

/// Handy function to parse the entire input string and store the tokens in an ArrayList
///
/// Function must be passed an empty ArrayList and will reset the lexer to start from the beginning
/// Once the lexer reaches the end of the file it will reset again
pub fn get_token_list(self: *Lexer, tokens: *std.ArrayList(token.Token)) !void {
    std.debug.assert(tokens.items.len == 0);

    self.reset_lexer();

    var tok = self.next_token();
    while (tok.type != .EOF) : (tok = self.next_token()) {
        try tokens.append(tok);
    }
    // Append the EOF token
    try tokens.append(tok);
    self.reset_lexer();
}

/// Handy utility function to print all the tokens in a Lexer. Must be provided an handle for stdout
///
/// Will reset the lexer before it begins and after it ends
pub fn print_debug_tokens(self: *Lexer, stdout: anytype) !void {
    self.reset_lexer();
    var tok = self.next_token();
    try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
    try stdout.print("{s:<15}|{s}\n", .{ "Token.TokenType", "[]u8 literal" });
    try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
    while (tok.type != .EOF) : (tok = self.next_token()) {
        try stdout.print("{s:<15}| {s}\n", .{ @tagName(tok.type), self.input[tok.loc.start..tok.loc.end] });
        try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
    }
    try stdout.print("{s:<15}| {s}\n", .{ @tagName(tok.type), "" });
    try stdout.print("{s:<15}*{s}\n", .{ "-" ** 15, "-" ** 15 });
    self.reset_lexer();
}

/// reset the lexer to its default state ready to retokenize the input
pub fn reset_lexer(self: *Lexer) void {
    std.debug.assert(self.input.len > 0);
    self.position = 0;
    self.read_head_position = 0;
    self.ch_current = 0;
    self.read_next_character();
}

/// Private function that moves along the input consuming characters
/// until it encounters a given string or the end of the file
fn read_until(self: *Lexer, ch: u8) void {
    while (self.ch_current != ch and self.ch_current != 0) {
        self.read_next_character();
    }
}

/// Read the next character and move our position pointers forward 1
fn read_next_character(self: *Lexer) void {
    if (self.read_head_position >= self.input.len) {
        self.ch_current = 0;
    } else {
        self.ch_current = self.input[self.read_head_position];
    }
    self.position = self.read_head_position;
    self.read_head_position += 1;
}

/// Read the next character but dont move the position pointers
fn peak_next_character(self: *Lexer) u8 {
    if (self.read_head_position >= self.input.len) {
        return 0;
    } else {
        return self.input[self.read_head_position];
    }
}

/// Utility to check if a given character is a valid letter for indentifiers
fn is_letter(ch: u8) bool {
    return (('a' <= ch) and (ch <= 'z')) or (('A' <= ch) and (ch <= 'Z')) or ch == '_';
}

/// Utility to check if a given charachcter is a valid digit for integer literals
fn is_digit(ch: u8) bool {
    return ('0' <= ch) and (ch <= '9');
}

/// Moves our read pointers forward till we have consumed and identifier
fn read_identifier(self: *Lexer) void {
    while (is_letter(self.ch_current) or is_digit(self.ch_current)) {
        self.read_next_character();
    }
}

/// Moves our read pointer forward untill we have consumed a numerical literal
fn read_number_literal(self: *Lexer) bool {
    var is_float = false;
    while (is_digit(self.ch_current) or self.ch_current == '.') {
        if (self.ch_current == '.') {
            is_float = true;
        }
        self.read_next_character();
    }
    return is_float;
}

fn read_string_literal(self: *Lexer) void {
    while (true) {
        if (self.ch_current == '"') {
            break;
        }
        if (self.ch_current == 0) {
            break;
        }
        self.read_next_character();
    }
}

fn skip_white_spaces(self: *Lexer) void {
    while (self.ch_current == ' ' or self.ch_current == '\t' or self.ch_current == '\n' or self.ch_current == '\r') {
        self.read_next_character();
    }
}

// Test every aspect of the lexer and all edge cases
test "test_lexing" {
    const input =
        \\ const five = 5;
        \\ var ten = 10.5;
        \\
        \\ const add = fn(x, y) {
        \\     x + y;
        \\ };
        \\ 
        \\ const result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\
        \\ if ( 5 < 10.5 ) {
        \\     return true;
        \\ } else {
        \\     return false; // This is also a comment that should be skipped
        \\ }
        \\
        \\ // This is a comment that should be skipped
        \\ // More comments to be skipped
        \\
        \\ 10 == 10;
        \\ 10 != 9;
        \\ 10 >= 10;
        \\ 9 <= 10;// Comments close to the previous character should be skipped
        \\
        \\ // Comment at the end of the file should also be skipped correctly
        \\ "foobar"
        \\ "foo bar"
        \\ [1, "two", three, 4]
        \\ while (true) {
        \\   i = i + 1;
        \\   break;
        \\   continue;
        \\ }
        \\ {"one": 1}
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
        .{ .expectedType = .FLOAT, .expectedLiteral = "10.5" },
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
        .{ .expectedType = .FLOAT, .expectedLiteral = "10.5" },
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
        .{ .expectedType = .STRING, .expectedLiteral = "foobar" },
        .{ .expectedType = .STRING, .expectedLiteral = "foo bar" },
        .{ .expectedType = .LBRACKET, .expectedLiteral = "[" },
        .{ .expectedType = .INT, .expectedLiteral = "1" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .STRING, .expectedLiteral = "two" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .IDENT, .expectedLiteral = "three" },
        .{ .expectedType = .COMMA, .expectedLiteral = "," },
        .{ .expectedType = .INT, .expectedLiteral = "4" },
        .{ .expectedType = .RBRACKET, .expectedLiteral = "]" },
        .{ .expectedType = .WHILE, .expectedLiteral = "while" },
        .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = .TRUE, .expectedLiteral = "true" },
        .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .IDENT, .expectedLiteral = "i" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .IDENT, .expectedLiteral = "i" },
        .{ .expectedType = .PLUS, .expectedLiteral = "+" },
        .{ .expectedType = .INT, .expectedLiteral = "1" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .BREAK, .expectedLiteral = "break" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .CONTINUE, .expectedLiteral = "continue" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = .STRING, .expectedLiteral = "one" },
        .{ .expectedType = .COLON, .expectedLiteral = ":" },
        .{ .expectedType = .INT, .expectedLiteral = "1" },
        .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    };
    var lex = Lexer.init(input);

    for (tests) |t| {
        const tok = lex.next_token();
        // lex.print_token(tok, "lexer test");
        try testing.expectEqual(t.expectedType, tok.type);
        try testing.expectEqualStrings(t.expectedLiteral, lex.input[tok.loc.start..tok.loc.end]);
    }
}

const std = @import("std");
const print = std.debug.print;
const testing = std.testing;

pub const token = @import("token.zig");
