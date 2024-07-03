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
    // var start_statement: u32 = 0;

    var symbol_tree = SymbolTree.init(allocator);
    defer symbol_tree.deinit();
    var vm = try VM.init(allocator, false);
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
            var ast = try Parser.parse_program(source_buffer.items[0 .. source_buffer.items.len - 1 :0], allocator, &symbol_tree);
            defer ast.deinit(allocator);

            if (ast.errors.len > 0) {
                try Parser.print_parser_errors_to_stdout(&ast, stdout);
                try bw.flush();
                continue;
            }

            ast.print_to_stderr();

            var compiler = try Compiler.create(allocator, &vm.memory);

            try compiler.compile(&ast, 0);

            const out = try Code.code_to_str(allocator, vm.memory.instructions.items);
            defer allocator.free(out);
            const out2 = try Code.code_to_str(allocator, vm.memory.function_storage.items);
            defer allocator.free(out2);

            try stdout.print("Instructions:\n", .{});
            try stdout.print("\t{s}\n", .{out});
            try stdout.print("Functions:\n", .{});
            try stdout.print("\t{s}\n", .{out2});
            try bw.flush();

            try vm.run();

            try stdout.print("{d}\n", .{vm.memory.instructions.items});

            const sptr = vm.memory.stack_top() orelse 0;
            const object = vm.memory.memory.get(sptr);
            std.debug.print("Instruction pointer: {d}\n", .{vm.memory.ins_ptr});
            std.debug.print("Stack Ptr: {d}\n", .{sptr});

            const outstr = try vm.memory.ObjectToString(object, &buffer);
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
const Environment = @import("../tessel/environment.zig");
const IdentifierMap = @import("../tessel/symbol_table.zig");
const global_env = @import("../tessel/environment_pool.zig").global_env;
const Compiler = @import("../tessel/compiler.zig");
const Code = @import("../tessel/code.zig");
const VM = @import("../tessel/vm.zig");
const Memory = @import("../tessel/memory.zig");
const SymbolTree = @import("../tessel/symbol_tree.zig");
