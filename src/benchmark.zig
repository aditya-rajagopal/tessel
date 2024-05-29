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
    \\  fibonacci(35);
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
    std.debug.print("Testing Fibonacci(35) Using tessel: {s}\n", .{tessel_fibonacci_35});
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var buffer: [1024]u8 = undefined;
    var timer = try std.time.Timer.start();
    var identifier_map = IdentifierMap.init();
    defer identifier_map.deinit(allocator);
    var env = try Environment.Create(allocator);
    var eval = try Evaluator.init(allocator, env, &identifier_map);
    std.debug.print("Object Pool Capacity start {d}\n", .{eval.object_pool.object_pool.capacity});
    var ast = try Parser.parse_program(tessel_fibonacci_35, allocator, &identifier_map);
    defer ast.deinit(allocator);

    try Parser.print_parser_errors_to_stderr(&ast);
    const output = try eval.evaluate_program(&ast, allocator, env);

    const outstr = try eval.object_pool.ToString(&buffer, output);
    const end_time = timer.read();
    std.debug.print("Object Pool Capacity End {d}\n", .{eval.object_pool.object_pool.capacity});
    // env.print_env_hashmap_stderr();
    // try eval.object_pool.print_object_pool_to_stderr();
    std.debug.print("Fibonacci in Tessel: result: {s} time: {d}\n", .{ outstr, std.fmt.fmtDuration(end_time) });
    eval.object_pool.free(allocator, output);
    env.deinit(allocator, &eval.object_pool);
    eval.deinit(allocator);
}

const lexer = @import("tessel/lexer.zig");
const Parser = @import("tessel/parser.zig");
const Evaluator = @import("tessel/evaluator.zig");
const object = @import("tessel/object.zig");
const Environment = @import("tessel/environment.zig");
const IdentifierMap = @import("tessel/identifier_map.zig");
