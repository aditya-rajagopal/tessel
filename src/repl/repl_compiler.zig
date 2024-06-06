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
        if (deinit_status == .leak) @panic("MEMORY LEAK");
    }

    var buffer: [4096]u8 = undefined;
    var msg_buf: [10240]u8 = undefined;
    var source_buffer = std.ArrayList(u8).init(allocator);
    defer source_buffer.deinit();
    try source_buffer.append(0);
    var start_statement: u32 = 0;

    var identifier_map = IdentifierMap.init();
    defer identifier_map.deinit(allocator);
    var compiler = try Compiler.init(allocator, &identifier_map);
    defer compiler.deinit();
    var vm = try VM.init(allocator);
    defer vm.deinit();

    while (true) {
        try stdout.print("{s}", .{PROMT});
        try bw.flush();

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
            _ = source_buffer.popOrNull();
            try source_buffer.appendSlice(m);
            try source_buffer.append('\n');
            try source_buffer.append(0);
            defer source_buffer.shrinkRetainingCapacity(0);

            try bw.flush();
            try stdout.print("\n", .{});
            try bw.flush();
            var ast = try Parser.parse_program(source_buffer.items[0 .. source_buffer.items.len - 1 :0], allocator, &identifier_map);
            defer ast.deinit(allocator);

            if (ast.errors.len > 0) {
                try Parser.print_parser_errors_to_stdout(&ast, stdout);
                try bw.flush();
                continue;
            }

            try compiler.compile(&ast, 0);
            const byte_code = try compiler.get_byte_code();

            const out = try Code.code_to_str(allocator, byte_code.instructions);
            defer allocator.free(out);
            try stdout.print("{s}\n", .{out});
            try bw.flush();

            start_statement = @intCast(try vm.run(byte_code, start_statement));

            try stdout.print("{d}\n", .{byte_code.instructions});
            _ = vm.stack_top() orelse {
                // try stdout.print("Error running code. Stack empty", .{});
                try bw.flush();
                continue;
            };
            var obj = vm.stack_pop();
            const outstr = try object.ObjectToString(obj, &buffer);
            obj.deinit(allocator, false);
            try stdout.print("Output >> {s}\n", .{outstr});
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
const Allocator = std.mem.Allocator;
const lexer = @import("../tessel/lexer.zig");
const Parser = @import("../tessel/parser.zig");
const Evaluator = @import("../tessel/evaluator.zig");
const object = @import("../tessel/object.zig");
const Environment = @import("../tessel/environment.zig");
const IdentifierMap = @import("../tessel/symbol_table.zig");
const global_env = @import("../tessel/environment_pool.zig").global_env;
const Compiler = @import("../tessel/compiler.zig");
const Code = @import("../tessel/code.zig");
const VM = @import("../tessel/vm.zig");
