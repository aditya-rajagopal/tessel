const std = @import("std");
const repl = @import("repl/repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();
    const file_name = args.next() orelse return try repl.start(allocator);
    if (file_name.len < 4) {
        std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
        return;
    }
    if (!std.mem.eql(u8, ".tes", file_name[file_name.len - 4 .. file_name.len])) {
        std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
        return;
    }

    var file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();
    var buffer: [10240]u8 = undefined;
    const out = try file.reader().readAll(&buffer);
    var out_buffer: [1024]u8 = undefined;
    buffer[out] = 0;
    var timer = try std.time.Timer.start();

    var env = try Environment.Create(allocator);
    defer env.deinit(allocator);
    var eval = try Evaluator.init(allocator, env);
    defer eval.deinit(allocator);

    var ast = try Parser.parse_program(buffer[0..out :0], allocator);
    defer ast.deinit(allocator);

    try Parser.print_parser_errors_to_stderr(&ast);
    const output = try eval.evaluate_program(&ast, allocator, env);

    defer eval.object_pool.free(allocator, output);
    const outstr = try eval.object_pool.ToString(&out_buffer, output);
    const end_time = timer.read();
    std.debug.print("{s}\n", .{outstr});
    std.debug.print("Program runtime: {d}\n", .{std.fmt.fmtDuration(end_time)});
}

const lexer = @import("tessel/lexer.zig");
const Parser = @import("tessel/parser.zig");
const Evaluator = @import("tessel/evaluator.zig");
const object = @import("tessel/object.zig");
const Environment = @import("tessel/environment.zig");
