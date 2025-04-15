const std = @import("std");
const repl = @import("repl/repl_compiler.zig");

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer {
    //     const deinit_status = gpa.deinit();
    //     if (deinit_status == .leak) @panic("MEMORY LEAK");
    // }
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    _ = args.skip();

    const file_name = args.next() orelse return try repl.start();
    if (file_name.len < 4) {
        std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
        return;
    }
    if (!std.mem.eql(u8, ".tes", file_name[file_name.len - 4 .. file_name.len])) {
        std.debug.print("Unkown parameter provided: \"{s}\". Expected a .tes file\n", .{file_name});
        return;
    }

    var buffer: [10240]u8 = undefined;
    var len: usize = 0;

    var file = try std.fs.cwd().openFile(file_name, .{});
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

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("{s}\n", .{buffer[0..len]});
    try bw.flush();
}

const Parser = @import("tessel/parser.zig");
const Evaluator = @import("tessel/evaluator.zig");
const Compiler = @import("tessel/compiler.zig");
const ByteCode = @import("tessel/byte_code.zig");
const VM = @import("tessel/vm.zig");
const Memory = @import("tessel/memory.zig");
const SymbolTree = @import("tessel/symbol_tree.zig");
