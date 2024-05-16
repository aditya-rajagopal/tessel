const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const PROMT = ">> ";
const EXIT = "exit";

pub fn start() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stdin_file = std.io.getStdIn().reader();
    var buf_reader = std.io.bufferedReader(stdin_file);
    const stdin = buf_reader.reader();

    try stdout.print("Welcome to tessel.\n", .{});
    try stdout.print("Feel free to type commands here at your own risk\n", .{});
    try stdout.print("Type \"exit\" without the quotes to quit or Ctrl+c \n", .{});
    try bw.flush();

    while (true) {
        try stdout.print("{s}", .{PROMT});
        try bw.flush();

        var msg_buf: [10240]u8 = undefined;
        const msg = try stdin.readUntilDelimiterOrEof(&msg_buf, '\n');

        if (msg) |m| {
            const cmp = std.mem.eql(u8, EXIT, m[0 .. m.len - 1]);
            if (cmp) {
                try stdout.print("Tessel is exiting", .{});
                break;
            }
            var lex = lexer.Lexer.init(m);
            try lex.print_debug_tokens(stdout);
            try bw.flush();
        }
    }
}
