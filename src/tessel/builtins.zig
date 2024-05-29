pub const Builtins = @This();

len: ObjectPool.BuiltInFn,

pub const default = Builtins{
    .len = Builtins.len,
};

fn len(
    self: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function len. Expected 1 got {d}",
            .{length},
        ) catch unreachable;
        return self.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch unreachable;
    }

    const arg_tag = self.object_pool.get_tag(objects[0]);
    const arg_data = self.object_pool.get_data(objects[0]);
    switch (arg_tag) {
        .string => {
            const str_len: i64 = @as(i64, @intCast(arg_data.string_type.len));
            return self.object_pool.create(allocator.*, .integer, @ptrCast(&str_len)) catch unreachable;
        },
        .array => {
            const array_len: i64 = @as(i64, @intCast(arg_data.array.items.len));
            return self.object_pool.create(allocator.*, .integer, @ptrCast(&array_len)) catch unreachable;
        },
        else => {
            const output = std.fmt.allocPrint(
                allocator.*,
                "Argument of type: {s} is not supported by builtin 'len'",
                .{self.object_pool.get_tag_string(objects[0])},
            ) catch unreachable;
            return self.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch unreachable;
        },
    }
    return null_object;
}

const Evaluator = @import("evaluator.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const ObjectPool = @import("object.zig");
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;
