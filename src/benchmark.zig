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
    std.debug.print("Testing ./test.tes Using tessel:\n", .{});
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buffer: [1024]u8 = undefined;
    var len: usize = 0;
    var timer = try std.time.Timer.start();
    {
        var file = try std.fs.cwd().openFile("./test.tes", .{});
        defer file.close();

        const source = try file.readToEndAllocOptions(allocator, 4096 * 1024, 4096 * 8, 1, 0);
        var symbol_tree = SymbolTree.init(allocator);
        defer symbol_tree.deinit();
        var vm = try VM.init(allocator, false);
        defer vm.deinit();

        var ast = try Parser.parse_program(source, allocator, &symbol_tree);
        defer ast.deinit(allocator);

        if (ast.errors.len > 0) {
            try Parser.print_parser_errors_to_stderr(&ast);
            return;
        }

        var compiler = try Compiler.create(allocator, &vm.memory);
        try compiler.compile(&ast, 0);

        try vm.run();

        const sptr = vm.memory.stack_top() orelse 0;
        const obj = vm.memory.memory.get(sptr);

        const outstr = try vm.memory.ObjectToString(obj, &buffer);
        len = outstr.len;
    }
    const end_time = timer.read();
    std.debug.print("Fibonacci in Tessel: result: {s} time: {d}\n", .{ buffer[0..len], std.fmt.fmtDuration(end_time) });

    // Run python code
    var argv = std.ArrayListUnmanaged([]const u8){};
    defer argv.deinit(allocator);
    try argv.appendSlice(allocator, &[_][]const u8{ "python", "./test.py" });

    var process = Child.init(argv.items, allocator);
    var python_timer = try std.time.Timer.start();
    {
        _ = try process.spawnAndWait();
    }
    const python_end_time = python_timer.read();
    std.debug.print("Time to run python: {s}\n", .{std.fmt.fmtDuration(python_end_time)});

    return;
}

const lexer = @import("tessel/lexer.zig");
const Parser = @import("tessel/parser.zig");
const Compiler = @import("tessel/compiler.zig");
const ByteCode = @import("tessel/byte_code.zig");
const VM = @import("tessel/vm.zig");
const Memory = @import("tessel/memory.zig");
const SymbolTree = @import("tessel/symbol_tree.zig");
const Child = @import("std").process.Child;
