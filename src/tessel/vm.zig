pub const VM = @This();

instructions: []const u8,
constants: std.MultiArrayList(ObjectPool.InternalObject).Slice,
allocator: Allocator,

stack: std.MultiArrayList(ObjectPool.InternalObject),
stack_ptr: usize = 0,

pub const VMError = error{ StackOverflow, AccessingEmptyStack, InsufficientOperandsOnStack, TypeMismatch };
pub const Error = VMError || Allocator.Error;

pub const stack_limit = 2048;

pub fn init(byte_code: ByteCode, allocator: Allocator) !VM {
    var vm = VM{
        .instructions = byte_code.instructions,
        .constants = byte_code.constants,
        .allocator = allocator,
        .stack = .{},
    };
    try vm.stack.ensureTotalCapacity(allocator, stack_limit);
    return vm;
}

pub fn deinit(self: *VM) void {
    self.allocator.free(self.instructions);
    self.constants.deinit(self.allocator);
    self.stack.deinit(self.allocator);
}

pub fn run(self: *VM) !void {
    var index: usize = 0;
    while (index < self.instructions.len) {
        const op: Code.Opcode = @enumFromInt(self.instructions[index]);

        switch (op) {
            .load_const => {
                const obj_index = std.mem.bytesToValue(u16, self.instructions[index + 1 ..]);
                index += 2;
                try self.stack_push(self.constants.get(obj_index));
            },
            .add => {
                const right = self.stack_pop();
                const right_tag = right.tag;
                const sptr = self.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                const left_tag = self.stack.items(.tag)[sptr - 1];
                if (left_tag != right_tag) {
                    return VMError.TypeMismatch;
                }
                switch (left_tag) {
                    .integer => {
                        self.stack.items(.data)[sptr - 1].integer += right.data.integer;
                    },
                    else => return VMError.TypeMismatch,
                }
            },
        }
        index += 1;
    }
}

fn stack_pop(self: *VM) ObjectPool.InternalObject {
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

    var identifier_map = IdentifierMap.init();
    defer identifier_map.deinit(testing.allocator);

    var compiler = try Compiler.init(testing.allocator);
    defer compiler.deinit(testing.allocator);

    var ast = try Parser.parse_program(source, testing.allocator, &identifier_map);
    defer ast.deinit(testing.allocator);

    try compiler.compile(&ast, 0);
    const byte_code: ByteCode = try compiler.get_byte_code();
    var vm = try VM.init(byte_code, testing.allocator);
    vm.deinit();
    // byte_code.deinit(testing.allocator);
}

const VMTestCase = struct {
    source: [:0]const u8,
    expected: ObjectPool.InternalObject,
};

test "vm_test_arithmetic" {
    const tests = [_]VMTestCase{
        .{
            .source = "1",
            .expected = ObjectPool.InternalObject{
                .tag = .integer,
                .data = .{
                    .integer = 1,
                },
                .refs = 0,
            },
        },
        .{
            .source = "2",
            .expected = ObjectPool.InternalObject{
                .tag = .integer,
                .data = .{
                    .integer = 2,
                },
                .refs = 0,
            },
        },
        .{
            .source = "1 + 2",
            .expected = ObjectPool.InternalObject{
                .tag = .integer,
                .data = .{
                    .integer = 3,
                },
                .refs = 0,
            },
        },
    };

    try run_vm_tests(&tests);
}

fn run_vm_tests(tests: []const VMTestCase) !void {
    for (tests) |t| {
        var identifier_map = IdentifierMap.init();
        defer identifier_map.deinit(testing.allocator);
        var compiler = try Compiler.init(testing.allocator);
        defer compiler.deinit(testing.allocator);
        var ast = try Parser.parse_program(t.source, testing.allocator, &identifier_map);
        defer ast.deinit(testing.allocator);
        try compiler.compile(&ast, 0);
        const byte_code: ByteCode = try compiler.get_byte_code();
        var vm = try VM.init(byte_code, testing.allocator);
        defer vm.deinit();
        try vm.run();

        const sptr = vm.stack_top() orelse return VMError.AccessingEmptyStack;
        const object = vm.stack.get(sptr - 1);
        try testing.expectEqual(t.expected.tag, object.tag);
        switch (t.expected.tag) {
            .integer => {
                try testing.expectEqual(t.expected.data.integer, object.data.integer);
            },
            else => unreachable,
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
const IdentifierMap = @import("identifier_map.zig");
const ObjectTypes = ObjectPool.ObjectTypes;
const ObjectIndex = ObjectPool.ObjectIndex;
const ByteCode = @import("byte_code.zig");
const Compiler = @import("compiler.zig");
