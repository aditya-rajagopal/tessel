pub const VM = @This();

memory: Memory,
allocator: Allocator,

pub const VMError = error{ InsufficientOperandsOnStack, TypeMismatch, IndexOutOfBounds };
pub const Error = VMError || Allocator.Error || Memory.MemoryError;

pub const memory_reservation = 65536;

pub fn init(allocator: Allocator, reserve_memory: bool) !VM {
    return VM{
        .memory = try Memory.initCapacity(allocator, if (reserve_memory) memory_reservation else 0),
        .allocator = allocator,
    };
}

pub fn deinit(self: *VM) void {
    self.memory.deinit();
}

pub fn run(self: *VM) !void {
    // var self.memory.ins_ptr: usize = @intCast(start_index);
    while (self.memory.ins_ptr < self.memory.instructions.items.len) {
        const op: Code.Opcode = @enumFromInt(self.memory.instructions.items[self.memory.ins_ptr]);
        // std.debug.print("Index: {d} op:{s}\n", .{ self.memory.ins_ptr, @tagName(op) });

        switch (op) {
            .load_const => {
                const obj_index = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                self.memory.ins_ptr += 2;
                try self.memory.stack_push(self.memory.get_constant(obj_index));
            },
            .ltrue => {
                try self.memory.stack_push(self.memory.get(Memory.true_object));
            },
            .lfalse => {
                try self.memory.stack_push(self.memory.get(Memory.false_object));
            },
            .lnull => {
                try self.memory.stack_push(self.memory.get(Memory.null_object));
            },
            .array => {
                const num_objects = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                self.memory.ins_ptr += 2;
                var data = try std.ArrayList(Memory.MemoryAddress).initCapacity(self.allocator, num_objects);
                defer data.deinit();
                for (0..num_objects) |_| {
                    data.appendAssumeCapacity(try self.memory.move_stack_top_to_heap());
                }
                try self.memory.stack_push_create(.array, @ptrCast(&(try data.toOwnedSlice())));
            },
            .index_into => try self.eval_index_into(),
            .index_range => try self.eval_index_range(),
            .pop => {
                if (self.memory.stack_ptr == 0) {
                    self.memory.ins_ptr += 1;
                    continue;
                }
                _ = try self.memory.stack_pop();
            },
            .add,
            .sub,
            .mul,
            .div,
            .geq,
            .gt,
            => {
                const right = try self.memory.stack_pop();
                const right_tag = right.dtype;
                switch (right_tag) {
                    .integer => try self.eval_int_infix(op, right.data.integer),
                    .string => try self.eval_string_infix(op, right),
                    else => {
                        return VMError.TypeMismatch;
                    },
                }
            },
            .neq, .eq => {
                try self.eval_eq(op);
            },
            .neg => {
                const sptr = self.memory.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                var memory_slice = self.memory.memory.slice();
                const left_tag = memory_slice.items(.dtype)[sptr - 1];
                if (left_tag != .integer) {
                    return VMError.TypeMismatch;
                }
                memory_slice.items(.data)[sptr - 1].integer *= -1;
            },
            .not => {
                const sptr = self.memory.stack_top() orelse return VMError.InsufficientOperandsOnStack;
                var memory_slice = self.memory.memory.slice();
                const data_slice = memory_slice.items(.data);
                const type_slice = memory_slice.items(.dtype);
                const left_tag = type_slice[sptr - 1];
                switch (left_tag) {
                    .integer => {
                        const lhs = data_slice[sptr - 1].integer;
                        type_slice[sptr - 1] = .boolean;
                        data_slice[sptr - 1].boolean = lhs == 0;
                    },
                    .boolean => {
                        data_slice[sptr - 1].boolean = !data_slice[sptr - 1].boolean;
                    },
                    else => return VMError.TypeMismatch,
                }
            },
            .jmp => {
                const destination = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                self.memory.ins_ptr = destination;
                continue;
            },
            .jn => {
                const right = try self.memory.stack_pop();
                const right_tag = right.dtype;
                if (right_tag != .integer and right_tag != .boolean) {
                    return VMError.TypeMismatch;
                }

                const condition = switch (right_tag) {
                    .integer => right.data.integer != 0,
                    .boolean => right.data.boolean,
                    else => unreachable,
                };

                if (condition) {
                    self.memory.ins_ptr += 5;
                    continue;
                } else {
                    const destination = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                    self.memory.ins_ptr = destination;
                    continue;
                }
            },
            .set_global => {
                const gid = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                self.memory.ins_ptr += 2;

                try self.memory.set_global(gid);
                // const expr = self.memory.stack_pop();
                // self.globals[var_self.memory.ins_ptr] = expr;
            },
            .get_global => {
                const gid = std.mem.bytesToValue(u16, self.memory.instructions.items[self.memory.ins_ptr + 1 ..]);
                self.memory.ins_ptr += 2;

                try self.memory.get_global(gid);
            },
        }
        self.memory.ins_ptr += 1;
    }
}

fn eval_index_range(self: *VM) !void {
    const right_range = try self.memory.stack_pop();
    if (right_range.dtype != .integer) {
        return Error.TypeMismatch;
    }
    const left_range = try self.memory.stack_pop();
    if (left_range.dtype != .integer) {
        return Error.TypeMismatch;
    }
    const literal = try self.memory.stack_pop();
    // var memory_slice = self.memory.memory.slice();
    const len = switch (literal.dtype) {
        .array => @as(i64, @intCast(literal.data.array.items.len)),
        .string => @as(i64, @intCast(literal.data.string_type.items.len)),
        else => unreachable,
    };

    const left_index = left_range.data.integer;
    const right_index = right_range.data.integer;
    if (left_index >= len or left_index < -len) {
        return Error.IndexOutOfBounds;
    }

    if (right_index > len or right_index < -len) {
        return Error.IndexOutOfBounds;
    }

    var left: usize = undefined;
    var right: usize = undefined;
    if (left_index >= 0) {
        left = @as(usize, @intCast(left_index));
    } else {
        left = @as(usize, @intCast(len + left_index));
    }
    if (right_index >= 0) {
        right = @as(usize, @intCast(right_index));
    } else {
        right = @as(usize, @intCast(len + right_index));
    }

    std.debug.print("left: {d}, right: {d}\n", .{ left, right });
    if (left > right) {
        return Error.IndexOutOfBounds;
    }

    switch (literal.dtype) {
        .array => {
            const locations = literal.data.array.items[left..right];
            var new_locations = try std.ArrayList(u32).initCapacity(self.allocator, locations.len);
            for (0..locations.len) |i| {
                new_locations.appendAssumeCapacity(try self.memory.dupe(locations[i]));
            }
            try self.memory.stack_push_create(.array, @ptrCast(&(try new_locations.toOwnedSlice())));
        },
        .string => {
            const slice = literal.data.string_type.items[left..right];
            const str = try std.fmt.allocPrint(self.allocator, "{s}", .{slice});
            try self.memory.stack_push_create(.string, @ptrCast(&str));
        },
        else => return Error.TypeMismatch,
    }
}

fn eval_index_into(self: *VM) !void {
    const index = try self.memory.stack_pop();
    if (index.dtype != .integer) {
        return Error.TypeMismatch;
    }
    const literal = try self.memory.stack_pop();

    const len = switch (literal.dtype) {
        .array => @as(i64, @intCast(literal.data.array.items.len)),
        .string => @as(i64, @intCast(literal.data.string_type.items.len)),
        else => unreachable,
    };

    const value = index.data.integer;

    if (value >= len or value < -(len)) {
        std.debug.print("value: {d}, len: {d}\n", .{ value, len });
        return Error.IndexOutOfBounds;
    }

    var i: usize = undefined;
    if (value >= 0) {
        i = @as(usize, @intCast(value));
    } else {
        i = @as(usize, @intCast(len + value));
    }

    switch (literal.dtype) {
        .array => {
            const location = literal.data.array.items[i];
            try self.memory.dupe_onto_stack(location);
            // try self.memory.stack_push(self.memory.get(location));
        },
        .string => {
            const character = literal.data.string_type.items[i];
            const str = try std.fmt.allocPrint(self.allocator, "{c}", .{character});
            try self.memory.stack_push_create(.string, @ptrCast(&str));
        },
        else => return Error.TypeMismatch,
    }
}

fn eval_string_infix(self: *VM, op: Code.Opcode, rhs: Memory.MemoryObject) !void {
    // const memory_slice = self.memory.memory.slice();
    // const dtype_slice = memory_slice.items(.dtype);
    // const data_slice = memory_slice.items(.data);
    const lhs = try self.memory.stack_pop();

    switch (op) {
        .add => {
            const outstr = try std.fmt.allocPrint(
                self.allocator,
                "{s}{s}",
                .{
                    lhs.data.string_type.items,
                    rhs.data.string_type.items,
                },
            );
            try self.memory.stack_push_create(.string, @ptrCast(&outstr));
        },
        .eq => {
            const result = std.mem.eql(u8, lhs.data.string_type.items, rhs.data.string_type.items);
            if (result) {
                try self.memory.stack_push(self.memory.get(Memory.true_object));
            } else {
                try self.memory.stack_push(self.memory.get(Memory.false_object));
            }
        },
        .neq => {
            const result = !std.mem.eql(u8, lhs.data.string_type.items, rhs.data.string_type.items);
            if (result) {
                try self.memory.stack_push(self.memory.get(Memory.true_object));
            } else {
                try self.memory.stack_push(self.memory.get(Memory.false_object));
            }
        },
        else => {
            return VMError.TypeMismatch;
        },
    }
}

fn eval_int_infix(self: *VM, op: Code.Opcode, rhs: i64) !void {
    const sptr = self.memory.stack_top() orelse return VMError.InsufficientOperandsOnStack;
    const memory_slice = self.memory.memory.slice();
    const dtype_slice = memory_slice.items(.dtype);
    const data_slice = memory_slice.items(.data);
    const left_tag = dtype_slice[sptr - 1];
    if (left_tag != .integer) {
        return VMError.TypeMismatch;
    }
    switch (op) {
        .add => {
            data_slice[sptr - 1].integer += rhs;
        },
        .sub => {
            data_slice[sptr - 1].integer -= rhs;
        },
        .mul => {
            data_slice[sptr - 1].integer *= rhs;
        },
        .div => {
            const lhs = data_slice[sptr - 1].integer;
            data_slice[sptr - 1].integer = @divFloor(lhs, rhs);
        },
        .gt => {
            const lhs = data_slice[sptr - 1].integer;
            dtype_slice[sptr - 1] = .boolean;
            data_slice[sptr - 1].boolean = lhs > rhs;
        },
        .geq => {
            const lhs = data_slice[sptr - 1].integer;
            dtype_slice[sptr - 1] = .boolean;
            data_slice[sptr - 1].boolean = lhs >= rhs;
        },
        else => unreachable,
    }
}

fn eval_eq(self: *VM, op: Code.Opcode) !void {
    const right = try self.memory.stack_pop();
    const right_tag = right.dtype;
    if (right_tag == .string) {
        try self.eval_string_infix(op, right);
        return;
    }
    const sptr = self.memory.stack_top() orelse return VMError.InsufficientOperandsOnStack;

    const memory_slice = self.memory.memory.slice();
    const dtype_slice = memory_slice.items(.dtype);
    const data_slice = memory_slice.items(.data);
    const left_tag = dtype_slice[sptr - 1];

    if (left_tag == .integer and right_tag == .integer) {
        const lhs = data_slice[sptr - 1].integer;
        dtype_slice[sptr - 1] = .boolean;
        switch (op) {
            .eq => data_slice[sptr - 1].boolean = lhs == right.data.integer,
            .neq => data_slice[sptr - 1].boolean = lhs != right.data.integer,
            else => unreachable,
        }
        return;
    }

    const left_bool = switch (left_tag) {
        .integer => data_slice[sptr - 1].integer != 0,
        .boolean => data_slice[sptr - 1].boolean,
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
    dtype_slice[sptr - 1] = .boolean;
    switch (op) {
        .eq => data_slice[sptr - 1].boolean = left_bool == right_bool,
        .neq => data_slice[sptr - 1].boolean = left_bool != right_bool,
        else => unreachable,
    }
}

test "vm_init" {
    const source: [:0]const u8 = "1 + 2";

    var symbol_table = SymbolTable.init();
    defer symbol_table.deinit(testing.allocator);
    var vm = try VM.init(testing.allocator, true);
    defer vm.deinit();

    var compiler = try Compiler.create(testing.allocator, &symbol_table, &vm.memory);

    var ast = try Parser.parse_program(source, testing.allocator, &symbol_table);
    defer ast.deinit(testing.allocator);

    try compiler.compile(&ast, 0);
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
test "evaluate_string_expressions" {
    const tests = [_]VMTestCase{
        .{ .source = "\"foobar\"", .expected = "foobar" },
        .{ .source = "const a = \"foobar\"; a;", .expected = "foobar" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a + b;", .expected = "foobar" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a + \"\" + b;", .expected = "foobar" },
        .{ .source = "const a = \"foo\"; a == \"foo\";", .expected = "true" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a == b;", .expected = "false" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a != b;", .expected = "true" },
        .{ .source = "const a = \"foo\"; const b = \"bar\"; a + \"\" + b == \"foobar\";", .expected = "true" },
        .{ .source = "const a = \"foobar\"; a[0];", .expected = "f" },
        .{ .source = "const a = \"foobar\"; a[0] + a[-1];", .expected = "fr" },
        .{ .source = "const a = \"foobar\"; a[0:2];", .expected = "fo" },
        .{ .source = "const a = \"foobar\"; a[3:6][0:2];", .expected = "ba" },
        .{ .source = "const a = \"foobar\"; a[0:2][0];", .expected = "f" },
        .{ .source = "const a = \"foobar\"; a[0:3] + a[3:6];", .expected = "foobar" },

        // .{
        //     .source = "const a = \"foo\"; const b = \"bar\"; var c = fn(x) { return x + \"baz\";}; c(a) + \" \" +c(b);",
        //     .expected = "foobaz barbaz",
        // },
        // .{
        //     .source =
        //     \\  const fn_call = fn(x) {
        //     \\      const b = fn(y) {
        //     \\          x + y
        //     \\      };
        //     \\      return b;
        //     \\  };
        //     \\  const add_foo = fn_call("foo");
        //     \\  add_foo("bar");
        //     ,
        //     .expected = "foobar",
        // },
        // .{ .source = "\"foobar\"[-1]", .expected = "r" },
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

test "run_arrays" {
    const tests = [_]VMTestCase{
        .{ .source = "[1, 2, 3]", .expected = "[1, 2, 3, ]" },
        .{ .source = "const a = 20; [1, 2, a]", .expected = "[1, 2, 20, ]" },
        .{ .source = "[1, 2, 3][0]", .expected = "1" },
        .{ .source = "[1, \"two\", 3][1]", .expected = "two" },
        .{ .source = "const a = [1, 2, 3]; a;", .expected = "[1, 2, 3, ]" },
        .{ .source = "const a = [1, \"two\", 3]; a;", .expected = "[1, two, 3, ]" },
        .{ .source = "const a = [1, [1, 2], 3]; a[1];", .expected = "[1, 2, ]" },
        .{ .source = "const a = [1, [1, 2], 3]; a[1][0];", .expected = "1" },
        .{ .source = "const a = [1, [1, 2], [[3, 4], [5, 6]]]; a[2][1][0];", .expected = "5" },
        .{ .source = "const a = [1, [1, 2], [[3, 4], [\"five\", 6]]]; a[2][1][0][1];", .expected = "i" },
        .{ .source = "const a = [1, 2, 3]; a[-1];", .expected = "3" },
        .{ .source = "const a = [1, 2, 3]; a[0:1];", .expected = "[1, ]" },
        .{ .source = "const a = [1, 2, 3]; a[0:2];", .expected = "[1, 2, ]" },
        .{ .source = "const a = [1, 2, 3]; a[1:3][0];", .expected = "2" },
        .{ .source = "const a = [1, [1, 2], 3]; a[0:2];", .expected = "[1, [1, 2, ], ]" },
        .{ .source = "const a = [1, [1, 2], [[1, 3]]]; a[-1][-1][-1];", .expected = "3" },
        .{ .source = "const a = [1, [1, 2], [[1, 3]]]; a[-3:-1];", .expected = "[1, [1, 2, ], ]" },
        .{ .source = "const a = [1, [1, 2], [[1, 3]]]; a[-3:-1][-1][0];", .expected = "1" },
        // .{ .source = "const last = fn(x) { return x[-1] }; last([1, 2, 3]);", .expected = "3" },
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
        // std.debug.print("Source: {s}\n", .{t.source});
        var symbol_table = SymbolTable.init();
        defer symbol_table.deinit(testing.allocator);
        var vm = try VM.init(testing.allocator, false);
        defer vm.deinit();

        var ast = try Parser.parse_program(t.source, testing.allocator, &symbol_table);
        defer ast.deinit(testing.allocator);

        var compiler = try Compiler.create(testing.allocator, &symbol_table, &vm.memory);
        try compiler.compile(&ast, 0);

        // const out = try Code.code_to_str(testing.allocator, vm.memory.instructions.items);
        // defer testing.allocator.free(out);
        // std.debug.print("Instructions:\n{s}\n", .{out});
        try vm.run();

        const sptr = vm.memory.stack_top() orelse 0;
        // std.debug.print("SPTR: {d}\n", .{sptr});
        const object = vm.memory.memory.get(sptr);
        // std.debug.print("OBJECT: {any}\n", .{object});

        const outstr = try vm.memory.ObjectToString(object, &buffer);
        try testing.expectEqualSlices(u8, t.expected, outstr);
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const assert = std.debug.assert;
const Ast = @import("ast.zig");
const Code = @import("code.zig");
const Parser = @import("parser.zig");
const SymbolTable = @import("symbol_table.zig");
// const ByteCode = @import("self.memory.zig");
const Compiler = @import("compiler.zig");
const Memory = @import("memory.zig");
