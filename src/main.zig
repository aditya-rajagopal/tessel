const std = @import("std");
const repl = @import("repl/repl.zig");

const tessel_fibonacci_35 =
    \\  const fibonacci = fn(x) {
    \\      if ( x == 0 ) {
    \\          return 0;
    \\      } else {
    \\          if ( x == 1 ) {
    \\              return 1;
    \\          } else {
    \\              return fibonacci(x - 1) + fibonacci(x - 2);
    \\          }
    \\      }
    \\  }
    \\  fibonacci(25);
;

fn fibonacci(x: u32) u32 {
    if (x == 0) {
        return 0;
    } else {
        if (x == 1) {
            return 1;
        } else {
            return fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
}

pub fn main() !void {
    // std.debug.print("Testing Fibonacci(35): {s}", .{tessel_fibonacci_35});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            @panic("memory leak");
        }
    }
    var buffer: [1024]u8 = undefined;
    // var args = try std.process.argsWithAllocator(allocator);
    // defer args.deinit();
    // _ = args.skip();
    // const arg = args.next() orelse return;
    // const num = try std.fmt.parseInt(u32, arg, 10);
    var timer = try std.time.Timer.start();
    // const start_time = timer.read();
    var env = try Environment.Create(allocator);
    defer env.deinit(allocator);
    var ast = try Parser.parse_program(tessel_fibonacci_35, allocator);
    defer ast.deinit(allocator);

    try Parser.print_parser_errors_to_stderr(&ast);
    const output = try Evaluator.evaluate_program(&ast, allocator, env);

    defer output.deinit(allocator);
    const outstr = try output.ToString(&buffer);
    // const output = fibonacci(num);
    const end_time = timer.read();
    std.debug.print("Fibonacci in Tessel: result: {s} time: {d}", .{ outstr, std.fmt.fmtDuration(end_time) });
    // std.debug.print("Fibonacci in Tessel: result: {d} time: {d}", .{ output, std.fmt.fmtDuration(end_time) });
    // try repl.start();
}

const lexer = @import("tessel/lexer.zig");
const Parser = @import("tessel/parser.zig");
const Evaluator = @import("tessel/evaluator.zig");
const object = @import("tessel/object.zig");
const Environment = @import("tessel/environment.zig");
