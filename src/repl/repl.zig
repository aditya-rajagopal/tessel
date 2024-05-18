const std = @import("std");
const lexer = @import("../tessel/lexer.zig");

pub const REPL = @This();
/// The text to be displayed when accepting a new statement
const PROMT = ">> ";

/// The command that will trigger the end of the REPL
const EXIT = "exit";

/// Function that starts the REPL. It creates a stdout/in reader/writer and connects the the lexer
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            @panic("memory leak");
        }
    }

    while (true) {
        try stdout.print("{s}", .{PROMT});
        try bw.flush();

        var msg_buf: [10240]u8 = undefined;
        const msg = try stdin.readUntilDelimiterOrEof(&msg_buf, '\n');
        if (msg) |m| {
            if (m.len == 0) {
                continue;
            }
            const cmp = std.mem.eql(u8, EXIT, m[0 .. m.len - 1]);
            if (cmp) {
                try stdout.print("Tessel is exiting", .{});
                break;
            }
            var source_code = try allocator.alloc(u8, m.len + 1);
            defer allocator.free(source_code);
            @memcpy(source_code, m.ptr);
            source_code[m.len] = 0;

            var lex = lexer.init(source_code[0..m.len :0]);
            try lex.print_debug_tokens(stdout);
            try bw.flush();
        }
    }
}
