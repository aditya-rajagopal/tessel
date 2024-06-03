pub const Code = @This();

pub const CodeError = error{};

const Error = CodeError || Allocator.Error;

pub const Instructions = []const u8;

pub const ReadReturn = struct {
    operands: []u32,
    offset: usize,
};

pub fn make(
    instructions: *std.ArrayList(u8),
    op: Opcode,
    operands: []const u32,
) Error!void {
    try instructions.append(@as(u8, @intCast(@intFromEnum(op))));
    const def = Definitions[@as(usize, @intCast(@intFromEnum(op)))];
    if (def[0] == 0) {
        return;
    }
    assert(def.len == operands.len);
    var inst_len: u8 = 0;
    for (def) |v| {
        inst_len += v;
    }

    try instructions.ensureUnusedCapacity(inst_len);

    for (0..operands.len) |i| {
        const width = def[i];
        switch (width) {
            2 => {
                instructions.appendSliceAssumeCapacity(std.mem.asBytes(&@as(u16, @intCast(operands[i]))));
            },
            else => unreachable,
        }
    }
}

pub fn read_operands(allocator: Allocator, definition: []const u8, ins: Instructions) !ReadReturn {
    var operands = std.ArrayListUnmanaged(u32){};
    defer operands.deinit(allocator);
    var offset: usize = 0;

    for (definition) |d| {
        switch (d) {
            2 => {
                const value = @as(u32, @intCast(std.mem.bytesToValue(u16, ins[offset..])));
                try operands.append(allocator, value);
            },
            0 => {
                continue;
            },
            else => unreachable,
        }
        offset += @as(usize, @intCast(d));
    }

    return ReadReturn{
        .operands = try operands.toOwnedSlice(allocator),
        .offset = offset,
    };
}

pub fn code_to_str(allocator: Allocator, instructions: []const u8) ![]u8 {
    var offset: usize = 0;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();
    var local_buffer: [1024]u8 = undefined;
    while (offset < instructions.len) {
        const def = Definitions[@as(usize, @intCast(instructions[offset]))];
        const output = try read_operands(allocator, def, instructions[offset + 1 ..]);
        const op: Opcode = @enumFromInt(instructions[offset]);
        try buffer.appendSlice(try std.fmt.bufPrint(
            &local_buffer,
            "{d:0>4} {s}",
            .{ offset, @tagName(op) },
        ));
        try format_instruction(def, output.operands, &buffer);
        try buffer.appendSlice("\n");
        offset += 1 + output.offset;
        allocator.free(output.operands);
    }
    return buffer.toOwnedSlice();
}

fn format_instruction(def: []const u8, operands: []u32, buffer: *std.ArrayList(u8)) !void {
    const num_operands = def.len;
    if (def[0] == 0) {
        return;
    }
    assert(operands.len == def.len);
    var local_buffer: [1024]u8 = undefined;
    switch (num_operands) {
        1 => {
            try buffer.appendSlice(try std.fmt.bufPrint(
                &local_buffer,
                " {d}",
                .{
                    operands[0],
                },
            ));
        },
        0 => {},
        else => unreachable,
    }
}

pub const Opcode = enum(u8) {
    load_const,
    add,
};

pub const Definitions = std.enums.directEnumArrayDefault(Opcode, []const u8, null, 0, .{
    // We are limiting the number of possible constants to be around 2^16 - 1.
    .load_const = &[_]u8{2},
    .add = &[_]u8{0},
});

test "test_definitions" {
    const op: Opcode = .load_const;
    const value = Definitions[@as(usize, @intCast(@intFromEnum(op)))];
    try testing.expectEqualSlices(u8, &[_]u8{2}, value);
}

test "make code" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const u32,
        expected: []const u8,
    }{
        .{
            .op = .load_const,
            .operands = &[_]u32{65534},
            .expected = &[_]u8{ @as(u8, @intCast(@intFromEnum(Opcode.load_const))), 254, 255 },
        },
        .{
            .op = .add,
            .operands = &[_]u32{},
            .expected = &[_]u8{@as(u8, @intCast(@intFromEnum(Opcode.add)))},
        },
    };

    var insts = std.ArrayList(u8).init(testing.allocator);
    defer insts.deinit();
    for (tests) |t| {
        try make(&insts, t.op, t.operands);
        try testing.expectEqualSlices(u8, t.expected, insts.items);
        insts.shrinkRetainingCapacity(0);
    }
}

test "make instructions string" {
    var insts = std.ArrayList(u8).init(testing.allocator);
    defer insts.deinit();
    try make(&insts, .load_const, &[_]u32{1});
    try make(&insts, .load_const, &[_]u32{2});
    try make(&insts, .load_const, &[_]u32{65534});
    try make(&insts, .add, &[_]u32{});

    const expected_str =
        \\0000 load_const 1
        \\0003 load_const 2
        \\0006 load_const 65534
        \\0009 add
        \\
    ;

    const output = try code_to_str(testing.allocator, insts.items);
    try testing.expectEqualSlices(u8, expected_str, output);
    testing.allocator.free(output);
}

test "code_read_ops" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const u32,
        bytes_to_read: u32,
    }{
        .{
            .op = .load_const,
            .operands = &[_]u32{65534},
            .bytes_to_read = 2,
        },
        .{
            .op = .add,
            .operands = &[_]u32{},
            .bytes_to_read = 0,
        },
    };

    var insts = std.ArrayList(u8).init(testing.allocator);
    defer insts.deinit();
    for (tests) |t| {
        try make(&insts, t.op, t.operands);
        const value = Definitions[@as(usize, @intCast(@intFromEnum(t.op)))];
        const output = try read_operands(testing.allocator, value, insts.items[1..]);
        try testing.expectEqual(t.bytes_to_read, output.offset);
        try testing.expectEqualSlices(u32, t.operands, output.operands);
        testing.allocator.free(output.operands);
        insts.shrinkRetainingCapacity(0);
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
