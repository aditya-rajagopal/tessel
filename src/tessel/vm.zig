pub const VM = @This();

// instructions: []const u8,
// constants: std.MultiArrayList(ObjectPool.InternalObject).Slice,
allocator: Allocator,

stack: std.MultiArrayList(ObjectPool.InternalObject),
globals: []ObjectPool.InternalObject,
stack_ptr: usize = 0,

pub const VMError = error{ StackOverflow, AccessingEmptyStack, InsufficientOperandsOnStack, TypeMismatch };
pub const Error = VMError || Allocator.Error;

pub const stack_limit = 2048;
pub const globals_size = 65536;

pub fn init(allocator: Allocator) !VM {
    var vm = VM{
        // .instructions = byte_code.instructions,
        // .constants = byte_code.constants,
        .allocator = allocator,
        .stack = .{},
        .globals = try allocator.alloc(ObjectPool.InternalObject, globals_size),
    };
    try vm.stack.ensureTotalCapacity(allocator, stack_limit);
    return vm;
}

pub fn deinit(self: *VM) void {
    // self.allocator.free(self.instructions);
    // self.constants.deinit(self.allocator);
    self.stack.deinit(self.allocator);
    self.allocator.free(self.globals);
}

pub fn run(self: *VM, byte_code: ByteCode, start_index: u32) !usize {
    var index: usize = @intCast(start_index);
    while (index < byte_code.instructions.len) {
        const op: Code.Opcode = @enumFromInt(byte_code.instructions[index]);
        // std.debug.print("Index: {d} op:{s}\n", .{ index, @tagName(op) });

        switch (op) {
            .load_const => {
                const obj_index = std.mem.bytesToValue(u16, byte_code.instructions[index + 1 ..]);
                index += 2;
                try self.stack_push(byte_code.constants.get(obj_index));
            },
            .ltrue => {
                try self.stack_push(byte_code.constants.get(ObjectPool.true_object));
            },
            .lfalse => {
                try self.stack_push(byte_code.constants.get(ObjectPool.false_object));
            },
            .lnull => {
                try self.stack_push(byte_code.constants.get(ObjectPool.null_object));
            },
            .pop => {
                if (self.stack_ptr == 0) {
                    index += 1;
                    continue;
                }
                _ = self.stack_pop();
            },
            .add,
            .sub,
            .mul,
            .div,
            .geq,
            .gt,
            => {
                const right = self.stack_pop();
                const right_tag = right.tag;
                const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                const left_tag = self.stack.items(.tag)[sptr - 1];
                if (left_tag != right_tag) {
                    return VMError.TypeMismatch;
                }
                switch (left_tag) {
                    .integer => {
                        try self.eval_int_infix(op, right.data.integer);
                    },
                    else => return VMError.TypeMismatch,
                }
            },
            .neq, .eq => {
                try self.eval_eq(op);
            },
            .neg => {
                const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                const left_tag = self.stack.items(.tag)[sptr - 1];
                if (left_tag != .integer) {
                    return VMError.TypeMismatch;
                }
                self.stack.items(.data)[sptr - 1].integer *= -1;
            },
            .not => {
                const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                const left_tag = self.stack.items(.tag)[sptr - 1];
                switch (left_tag) {
                    .integer => {
                        const lhs = self.stack.items(.data)[sptr - 1].integer;
                        self.stack.items(.tag)[sptr - 1] = .boolean;
                        self.stack.items(.data)[sptr - 1].boolean = lhs == 0;
                    },
                    .boolean => {
                        self.stack.items(.data)[sptr - 1].boolean = !self.stack.items(.data)[sptr - 1].boolean;
                    },
                    else => return VMError.TypeMismatch,
                }
            },
            .jmp => {
                const destination = std.mem.bytesToValue(u16, byte_code.instructions[index + 1 ..]);
                index = destination;
                continue;
            },
            .jn => {
                const right = self.stack_pop();
                const right_tag = right.tag;
                if (right_tag != .integer and right_tag != .boolean) {
                    return VMError.TypeMismatch;
                }

                const condition = switch (right_tag) {
                    .integer => right.data.integer != 0,
                    .boolean => right.data.boolean,
                    else => unreachable,
                };

                if (condition) {
                    index += 5;
                    continue;
                } else {
                    const destination = std.mem.bytesToValue(u16, byte_code.instructions[index + 1 ..]);
                    index = destination;
                    continue;
                }
            },
            .set_global => {
                const var_index = std.mem.bytesToValue(u16, byte_code.instructions[index + 1 ..]);
                index += 2;

                const expr = self.stack_pop();
                self.globals[var_index] = expr;
            },
            .get_global => {
                const var_index = std.mem.bytesToValue(u16, byte_code.instructions[index + 1 ..]);
                index += 2;

                try self.stack_push(self.globals[var_index]);
            },
        }
        index += 1;
    }
    return index;
}

fn eval_int_infix(self: *VM, op: Code.Opcode, rhs: i64) !void {
    const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
    switch (op) {
        .add => {
            self.stack.items(.data)[sptr - 1].integer += rhs;
        },
        .sub => {
            self.stack.items(.data)[sptr - 1].integer -= rhs;
        },
        .mul => {
            self.stack.items(.data)[sptr - 1].integer *= rhs;
        },
        .div => {
            const lhs = self.stack.items(.data)[sptr - 1].integer;
            self.stack.items(.data)[sptr - 1].integer = @divFloor(lhs, rhs);
        },
        .gt => {
            const lhs = self.stack.items(.data)[sptr - 1].integer;
            self.stack.items(.tag)[sptr - 1] = .boolean;
            self.stack.items(.data)[sptr - 1].boolean = lhs > rhs;
        },
        .geq => {
            const lhs = self.stack.items(.data)[sptr - 1].integer;
            self.stack.items(.tag)[sptr - 1] = .boolean;
            self.stack.items(.data)[sptr - 1].boolean = lhs >= rhs;
        },
        else => unreachable,
    }
}

fn eval_eq(self: *VM, op: Code.Opcode) !void {
    const right = self.stack_pop();
    const right_tag = right.tag;
    const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
    const left_tag = self.stack.items(.tag)[sptr - 1];
    if (left_tag == .integer and right_tag == .integer) {
        const lhs = self.stack.items(.data)[sptr - 1].integer;
        self.stack.items(.tag)[sptr - 1] = .boolean;
        switch (op) {
            .eq => self.stack.items(.data)[sptr - 1].boolean = lhs == right.data.integer,
            .neq => self.stack.items(.data)[sptr - 1].boolean = lhs != right.data.integer,
            else => unreachable,
        }
        return;
    }

    const left_bool = switch (left_tag) {
        .integer => self.stack.items(.data)[sptr - 1].integer != 0,
        .boolean => self.stack.items(.data)[sptr - 1].boolean,
        else => {
            // const outstr = try std.fmt.allocPrint(
            //     allocator,
            //     "Unknown Operation: <{s}> {s} <{s}>",
            //     .{
            //         self.object_pool.get_tag_string(left),
            //         get_token_literal(ast, node.main_token),
            //         self.object_pool.get_tag_string(right),
            //     },
            // );
            // return self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
            return VMError.TypeMismatch;
        },
    };
    const right_bool = switch (right_tag) {
        .integer => right.data.integer != 0,
        .boolean => right.data.boolean,
        else => {
            // const outstr = try std.fmt.allocPrint(
            //     allocator,
            //     "Unknown Operation: <{s}> {s} <{s}>",
            //     .{
            //         self.object_pool.get_tag_string(left),
            //         get_token_literal(ast, node.main_token),
            //         self.object_pool.get_tag_string(right),
            //     },
            // );
            // return self.object_pool.create(allocator, .runtime_error, @ptrCast(&outstr));
            return VMError.TypeMismatch;
        },
    };
    self.stack.items(.tag)[sptr - 1] = .boolean;
    switch (op) {
        .eq => self.stack.items(.data)[sptr - 1].boolean = left_bool == right_bool,
        .neq => self.stack.items(.data)[sptr - 1].boolean = left_bool != right_bool,
        else => unreachable,
    }
}

pub fn stack_pop(self: *VM) ObjectPool.InternalObject {
    self.stack_ptr -= 1;
    return self.stack.pop();
}

fn stack_push(self: *VM, obj: ObjectPool.InternalObject) VMError!void {
    if (self.stack_ptr >= stack_limit) {
        return Error.StackOverflow;
    }
    self.stack.appendAssumeCapacity(obj);
    self.stack_ptr += 1;
}

pub fn stack_top(self: *VM) ?usize {
    if (self.stack_ptr == 0) {
        return null;
    }
    return self.stack_ptr;
}

test "vm_init" {
    const source: [:0]const u8 = "1 + 2";

    var symbol_table = SymbolTable.init();
    defer symbol_table.deinit(testing.allocator);

    var compiler = try Compiler.init(testing.allocator, &symbol_table);
    defer compiler.deinit();

    var ast = try Parser.parse_program(source, testing.allocator, &symbol_table);
    defer ast.deinit(testing.allocator);

    try compiler.compile(&ast, 0);
    // const byte_code: ByteCode = try compiler.get_byte_code();
    var vm = try VM.init(testing.allocator);
    vm.deinit();
    // byte_code.deinit(testing.allocator);
}

const VMTestCase = struct {
    source: [:0]const u8,
    expected: []const u8,
};

test "vm_test_arithmetic" {
    const tests = [_]VMTestCase{
        .{ .source = "5", .expected = "5" },
        .{ .source = "10", .expected = "10" },
        .{ .source = "-5", .expected = "-5" },
        .{ .source = "-10", .expected = "-10" },
        .{ .source = "5 + 5 + 5 + 5 - 10", .expected = "10" },
        .{ .source = "2 * 2 * 2 * 2 * 2", .expected = "32" },
        .{ .source = "-50 + 100 + -50", .expected = "0" },
        .{ .source = "5 * 2 + 10", .expected = "20" },
        .{ .source = "5 + 2 * 10", .expected = "25" },
        .{ .source = "20 + 2 * -10", .expected = "0" },
        .{ .source = "50 / 2 * 2 + 10", .expected = "60" },
        .{ .source = "2 * (5 + 10)", .expected = "30" },
        .{ .source = "3 * 3 * 3 + 10", .expected = "37" },
        .{ .source = "3 * (3 * 3) + 10", .expected = "37" },
        .{ .source = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = "50" },
    };

    try run_vm_tests(&tests);
}

test "run_bool_tests" {
    const tests = [_]VMTestCase{
        .{ .source = "true", .expected = "true" },
        .{ .source = "false", .expected = "false" },
        .{ .source = "1 < 2", .expected = "true" },
        .{ .source = "1 > 2", .expected = "false" },
        .{ .source = "1 < 1", .expected = "false" },
        .{ .source = "1 > 1", .expected = "false" },
        .{ .source = "1 == 1", .expected = "true" },
        .{ .source = "1 != 1", .expected = "false" },
        .{ .source = "1 == 2", .expected = "false" },
        .{ .source = "1 != 2", .expected = "true" },
        .{ .source = "true == true", .expected = "true" },
        .{ .source = "false == false", .expected = "true" },
        .{ .source = "true == false", .expected = "false" },
        .{ .source = "true != false", .expected = "true" },
        .{ .source = "false != true", .expected = "true" },
        .{ .source = "(1 < 2) == true", .expected = "true" },
        .{ .source = "(1 < 2) == false", .expected = "false" },
        .{ .source = "(1 > 2) == true", .expected = "false" },
        .{ .source = "(1 > 2) == false", .expected = "true" },
    };
    try run_vm_tests(&tests);
}

test "run_if_expressions" {
    const tests = [_]VMTestCase{
        .{ .source = "if (true) { 10 }", .expected = "10" },
        .{ .source = "if (false) { 10 }", .expected = "null" },
        .{ .source = "if (1) { 10 }", .expected = "10" },
        .{ .source = "if (1 < 2) { 10 }", .expected = "10" },
        .{ .source = "if (1 > 2) { 10 }", .expected = "null" },
        .{ .source = "if (1 > 2) { 10 } else { 20 }", .expected = "20" },
        .{ .source = "if (1 < 2) { 10 } else { 20 }", .expected = "10" },
        // .{ .source = "if (1 < 2) { if(1 < 2) { return 10; } return 1; } else { 20 }", .expected = "10" },
        .{ .source = "if (1 < 2) { if ( 3 < 2 ) { 30 } else{ if (1 < 2 * 5 + 3) { 10 } }} else { 20 }", .expected = "10" },
    };

    try run_vm_tests(&tests);
}

test "evaluate_while_loops" {
    const tests = [_]VMTestCase{
        .{ .source = "while (false) { 10; }", .expected = "null" },
        .{ .source = "var a = 0; while (a < 10) { a = a + 1; } a", .expected = "10" },
        // .{
        //     .source =
        //     \\  const fn_call = fn(x) {
        //     \\      const b = fn(y) {
        //     \\          var a = y;
        //     \\          while (a < x ) {
        //     \\              a = a + 1;
        //     \\              if ( a >= 10 ) {
        //     \\                  break;
        //     \\              }
        //     \\          }
        //     \\          return a;
        //     \\      };
        //     \\      return b;
        //     \\  };
        //     \\  const t = fn_call(20);
        //     \\  t(10);
        //     ,
        //     .expected = "11",
        // },
    };

    try run_vm_tests(&tests);
}

test "run_prefix_not" {
    const tests = [_]VMTestCase{
        .{
            .source = "!5",
            .expected = "false",
        },
        .{
            .source = "!false",
            .expected = "true",
        },
        .{
            .source = "!!true",
            .expected = "true",
        },
        .{
            .source = "!!5",
            .expected = "true",
        },
        .{
            .source = "!!!5",
            .expected = "false",
        },
    };

    try run_vm_tests(&tests);
}

test "evaluate_identifiers" {
    const tests = [_]VMTestCase{
        .{ .source = "const a = 10; a;", .expected = "10" },
        .{ .source = "const a = 10; const b = 10; a;", .expected = "10" },
        .{ .source = "const a = 10; const b = 11; a; b;", .expected = "11" },
        .{ .source = "const a = 10; const b = 11; const c = a * b; b + c;", .expected = "121" },
        .{ .source = "const a = 2 * 2; const b = a + 3; if ( a < b ) { a; } else { b; } ", .expected = "4" },
        .{
            .source = "const a = 2 * 2; const b = a + 3; const c = if ( a < b ) { a + 3; } else { b; }; c; ",
            .expected = "7",
        },
        .{
            .source = "var a = 2 * 2; const b = a + 3; if ( a < b ) { a = 5; } else { a = 2; }; a; ",
            .expected = "5",
        },
        // .{
        //     .source = "var a = 2 * 2; const b = a + 3; const c = if ( a < b ) { return a + 5; } else { return true; }; c; ",
        //     .expected = "9",
        // },
    };

    try run_vm_tests(&tests);
}

fn run_vm_tests(tests: []const VMTestCase) !void {
    var buffer: [2048]u8 = undefined;
    for (tests) |t| {
        var symbol_table = SymbolTable.init();
        defer symbol_table.deinit(testing.allocator);
        var compiler = try Compiler.init(testing.allocator, &symbol_table);
        defer compiler.deinit();
        var ast = try Parser.parse_program(t.source, testing.allocator, &symbol_table);
        defer ast.deinit(testing.allocator);
        try compiler.compile(&ast, 0);
        const byte_code: ByteCode = try compiler.get_byte_code();
        // defer byte_code.deinit(testing.allocator);
        // var vm = try VM.init(byte_code, testing.allocator);
        var vm = try VM.init(testing.allocator);
        defer vm.deinit();
        _ = try vm.run(byte_code, 0);

        if (vm.stack_top()) |sptr| {
            const object = vm.stack.get(sptr - 1);

            const outstr = try ObjectPool.ObjectToString(object, &buffer);
            try testing.expectEqualSlices(u8, t.expected, outstr);
        } else {
            try testing.expectEqualSlices(u8, t.expected, "null");
        }
    }
}

const ObjectPool = @import("object.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
const Ast = @import("ast.zig");
const Code = @import("code.zig");
const Parser = @import("parser.zig");
const SymbolTable = @import("symbol_table.zig");
const ObjectTypes = ObjectPool.ObjectTypes;
const ObjectIndex = ObjectPool.ObjectIndex;
const ByteCode = @import("byte_code.zig");
const Compiler = @import("compiler.zig");
