const std = @import("std");
const repl = @import("repl/repl.zig");

pub fn main() !void {
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    // const allocator = arena.allocator();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            @panic("memory leak");
        }
    }
    var eval = try Evaluator.init(allocator);
    // var pool = try object.initCapacity(allocator, 0);
    defer eval.deinit(allocator);

    std.debug.print("Freelist: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("0th Element: {s} {any}\n", .{ @tagName(eval.object_pool.object_pool.items(.tag)[0]), eval.object_pool.object_pool.items(.data)[1].boolean });
    std.debug.print("1st Element: {s} {any}\n", .{ @tagName(eval.object_pool.object_pool.items(.tag)[1]), eval.object_pool.object_pool.items(.data)[1].boolean });
    std.debug.print("2nd Element: {s} {any}\n", .{ @tagName(eval.object_pool.object_pool.items(.tag)[2]), eval.object_pool.object_pool.items(.data)[2].boolean });

    const integer: i64 = 32;
    const position = try eval.object_pool.create(allocator, .integer, @ptrCast(&integer));
    std.debug.print("Stored_position: {d}\n", .{position});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(position).tag), eval.object_pool.get(position).data.integer });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});

    // std.debug.print("Freeing the integer value\n", .{});
    // std.debug.print("Length of object pool: {d}\n", .{eval.object_pool.object_pool.len});
    // try eval.object_pool.free(allocator, position);
    // std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    // std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(position).tag), eval.object_pool.get(position).data.integer });

    const boolean: bool = true;
    const bool_pos = try eval.object_pool.create(allocator, .boolean, @ptrCast(&boolean));
    std.debug.print("Stored_position: {d}\n", .{bool_pos});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(bool_pos).tag), eval.object_pool.get(bool_pos).data.boolean });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});

    const buf = try std.fmt.allocPrint(allocator, "This is a sample string {d}\n", .{integer});
    defer allocator.free(buf);
    const str_pos = try eval.object_pool.create(allocator, .string, @ptrCast(&buf));
    std.debug.print("Stored_position of string: {d}\n", .{str_pos});
    const str = eval.object_pool.get(str_pos).data.string_type;
    std.debug.print("Value: tag {s} \"{s}\"\n", .{ @tagName(eval.object_pool.get(str_pos).tag), str.ptr[0..str.len] });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Freeing the string value\n", .{});
    std.debug.print("Length of object pool: {d}\n", .{eval.object_pool.object_pool.len});
    try eval.object_pool.free(allocator, str_pos);
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(str_pos).tag), eval.object_pool.get(str_pos).data.string_type });

    const err_buf = try std.fmt.allocPrint(allocator, "This is a sample Error {d}\n", .{integer});
    defer allocator.free(err_buf);
    const err_pos = try eval.object_pool.create(allocator, .runtime_error, @ptrCast(&err_buf));
    std.debug.print("Stored_position of Runtime Error: {d}\n", .{err_pos});
    const str_err = eval.object_pool.get(str_pos).data.string_type;
    std.debug.print("Value: tag {s} \"{s}\"\n", .{ @tagName(eval.object_pool.get(err_pos).tag), str_err.ptr[0..str_err.len] });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Freeing the string value\n", .{});
    std.debug.print("Length of object pool: {d}\n", .{eval.object_pool.object_pool.len});
    try eval.object_pool.free(allocator, err_pos);
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(err_pos).tag), eval.object_pool.get(err_pos).data.string_type });

    const return_expr = try eval.object_pool.create(allocator, .return_expression, @ptrCast(&position));
    std.debug.print("Stored_position of Runtime Error: {d}\n", .{return_expr});
    std.debug.print("Value: tag {s} \"{any}\"\n", .{ @tagName(eval.object_pool.get(return_expr).tag), eval.object_pool.get(return_expr).data.return_value });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Freeing the string value\n", .{});
    std.debug.print("Length of object pool: {d}\n", .{eval.object_pool.object_pool.len});
    try eval.object_pool.free(allocator, return_expr);
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(return_expr).tag), eval.object_pool.get(return_expr).data.return_value });

    var blocks = [_]u32{ 1, 2, 3, 5 };
    var params = [_]u32{ 1, 2 };
    const env = try Environment.Create(allocator);
    defer env.deinit(allocator);
    const func_data = object.InternalObject.FunctionData{
        .block = blocks[0..4],
        .parameters = params[0..2],
        .env = env,
    };
    const func_loc = try eval.object_pool.create(allocator, .function_expression, @ptrCast(&func_data));

    std.debug.print("Stored_position of Runtime Error: {d}\n", .{func_loc});
    std.debug.print("Value: tag {s} \"{any}\"\n", .{ @tagName(eval.object_pool.get(func_loc).tag), eval.object_pool.get(func_loc).data.function });
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Freeing the string value\n", .{});
    std.debug.print("Length of object pool: {d}\n", .{eval.object_pool.object_pool.len});
    try eval.object_pool.free(allocator, func_loc);
    std.debug.print("Free List after: {any}\n", .{eval.object_pool.free_list.items});
    std.debug.print("Value: tag {s} {any}\n", .{ @tagName(eval.object_pool.get(func_loc).tag), eval.object_pool.get(func_loc).data.integer });

    try env.create_variable(allocator, "a", position, .constant);
    const var_pos = env.get_object("a");
    std.debug.print("Got variable a in position: {d}, should be: {d}\n", .{ var_pos, position });
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
