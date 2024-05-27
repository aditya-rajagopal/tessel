const std = @import("std");
const repl = @import("repl/repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var pool = try object.CreateCapacity(allocator, 5);

    std.debug.print("Freelist: {any}\n", .{pool.free_list.items});
    std.debug.print("0th Element: {s} {any}\n", .{ @tagName(pool.object_pool.items(.tag)[0]), pool.object_pool.items(.data)[1].boolean });
    std.debug.print("1st Element: {s} {any}\n", .{ @tagName(pool.object_pool.items(.tag)[1]), pool.object_pool.items(.data)[1].boolean });
    std.debug.print("2nd Element: {s} {any}\n", .{ @tagName(pool.object_pool.items(.tag)[2]), pool.object_pool.items(.data)[2].boolean });

    const integer: i64 = 32;
    const position = try pool.create_object(.integer, @ptrCast(&integer));
    std.debug.print("Stored_position: {d}\n", .{position});
    std.debug.print("Value: {any}\n", .{pool.get(position).data.integer});
    std.debug.print("Free List after: {any}\n", .{pool.free_list.items});

    const boolean: bool = true;
    const bool_pos = try pool.create_object(.boolean, @ptrCast(&boolean));
    std.debug.print("Stored_position: {d}\n", .{bool_pos});
    std.debug.print("Value: {any}\n", .{pool.get(bool_pos).data.boolean});
    std.debug.print("Free List after: {any}\n", .{pool.free_list.items});
    // var args = try std.process.argsWithAllocator(allocator);
    // defer args.deinit();
    // _ = args.skip();
    // const file_name = args.next() orelse return try repl.start(allocator);
    // if (file_name.len < 4) {
    //     std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
    //     return;
    // }
    // if (!std.mem.eql(u8, ".tes", file_name[file_name.len - 4 .. file_name.len])) {
    //     std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
    //     return;
    // }
    //
    // var file = try std.fs.cwd().openFile(file_name, .{});
    // // var file = std.fs.openFileAbsolute(file_name, .{}) catch return;
    // defer file.close();
    // var buffer: [10240]u8 = undefined;
    // const out = try file.reader().readAll(&buffer);
    // // std.debug.print("File: {s}", .{buffer[0..out]});
    // var out_buffer: [1024]u8 = undefined;
    // var timer = try std.time.Timer.start();
    // var env = try Environment.Create(allocator);
    // defer env.deinit(allocator);
    // var ast = try Parser.parse_program(buffer[0..out :0], allocator);
    // defer ast.deinit(allocator);
    //
    // try Parser.print_parser_errors_to_stderr(&ast);
    // const output = try Evaluator.evaluate_program(&ast, allocator, env);
    //
    // defer output.deinit(allocator);
    // const outstr = try output.ToString(&out_buffer);
    // const end_time = timer.read();
    // std.debug.print("Fibonacci in Tessel: result: {s} time: {d}", .{ outstr, std.fmt.fmtDuration(end_time) });
}

const lexer = @import("tessel/lexer.zig");
const Parser = @import("tessel/parser.zig");
const Evaluator = @import("tessel/evaluator.zig");
const object = @import("tessel/object.zig");
const Environment = @import("tessel/environment.zig");
