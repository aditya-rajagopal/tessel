pub const REPL = @This();
/// The text to be displayed when accepting a new statement
const PROMT = ">> ";

/// The command that will trigger the end of the REPL
const EXIT = "exit()";
// const CLEAR = "clear()";

/// Function that starts the REPL. It creates a stdout/in reader/writer and connects the the lexer
pub fn start() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stdin_file = std.io.getStdIn().reader();
    var buf_reader = std.io.bufferedReader(stdin_file);
    const stdin = buf_reader.reader();

    try print_header(stdout);
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
            if (m.len == 1 and m[0] == '\r') {
                continue;
            }
            const exit_cmp = std.mem.eql(u8, EXIT, m[0 .. m.len - 1]);
            if (exit_cmp) {
                try stdout.print("Tessel is exiting", .{});
                try bw.flush();
                break;
            }
            var source_code = try allocator.alloc(u8, m.len + 1);
            defer allocator.free(source_code);
            @memcpy(source_code, m.ptr);
            source_code[m.len] = 0;

            var ast = try Parser.parse_program(source_code[0..m.len :0], allocator);
            defer ast.deinit(allocator);

            var outlist = std.ArrayList(u8).init(allocator);
            defer outlist.deinit();
            try Parser.convert_ast_to_string(&ast, 1, &outlist);
            outlist.shrinkRetainingCapacity(outlist.items.len);
            try stdout.print("{s}\r\n", .{outlist.allocatedSlice()[0..outlist.items.len]});

            // for (0..ast.nodes.len) |i| {
            //     const n = ast.nodes.get(i);
            //     try stdout.print("Nodes({d}): {any}\r\n", .{ i, n });
            // }
            //
            // try stdout.print("Extra Data: ", .{});
            // for (0..ast.extra_data.len) |i| {
            //     const n = ast.extra_data[i];
            //     try stdout.print("{}, ", .{n});
            // }
            //
            // try stdout.print("\n", .{});

            try Parser.print_parser_errors_to_stdout(&ast, stdout);

            // var lex = lexer.init(source_code[0..m.len :0]);
            // try lex.print_debug_tokens(stdout);
            try bw.flush();
        }
    }
}

fn print_header(stdout: anytype) !void {
    // try stdout.print("\x1b[2J", .{});
    try stdout.print("Welcome to tessel.\n", .{});
    try stdout.print("Feel free to type commands here at your own risk\n", .{});
    try stdout.print("Type \"exit()\" without the quotes to quit or Ctrl+c \n", .{});
    // try stdout.print("Type \"clear()\" without the quotes to clear the screen \n", .{});
}

const std = @import("std");
const lexer = @import("../tessel/lexer.zig");
const Parser = @import("../tessel/parser.zig");
