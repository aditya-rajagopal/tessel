pub const Builtins = @This();

len: ObjectPool.BuiltInFn,
str: ObjectPool.BuiltInFn,
int: ObjectPool.BuiltInFn,
append: ObjectPool.BuiltInFn,
pop: ObjectPool.BuiltInFn,
get_keys: ObjectPool.BuiltInFn,
print: ObjectPool.BuiltInFn,

pub const default = Builtins{
    .len = Builtins.len,
    .str = Builtins.str,
    .int = Builtins.int,
    .append = Builtins.append,
    .pop = Builtins.pop,
    .get_keys = Builtins.get_keys,
    .print = Builtins.print,
};

fn len(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function len. Expected 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    switch (arg_tag) {
        .string => {
            const str_len: i64 = @as(i64, @intCast(arg_data.string_type.items.len));
            return eval.object_pool.create(allocator.*, .integer, @ptrCast(&str_len)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
        .array => {
            const array_len: i64 = @as(i64, @intCast(arg_data.array.items.len));
            return eval.object_pool.create(allocator.*, .integer, @ptrCast(&array_len)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
        else => {
            const output = std.fmt.allocPrint(
                allocator.*,
                "Argument of type: {s} is not supported by builtin 'len'",
                .{eval.object_pool.get_tag_string(objects[0])},
            ) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
            };
            return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
    }
    return null_object;
}

fn str(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function str. Expected 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    switch (arg_tag) {
        .string => {
            eval.object_pool.increase_ref(objects[0]);
            return objects[0];
        },
        .integer => {
            const out_str = std.fmt.allocPrint(allocator.*, "{d}", .{arg_data.integer}) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
            };
            return eval.object_pool.create(allocator.*, .string, @ptrCast(&out_str)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
        else => {
            const output = std.fmt.allocPrint(
                allocator.*,
                "Argument of type: {s} is not supported by builtin 'len'",
                .{eval.object_pool.get_tag_string(objects[0])},
            ) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
            };
            return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
    }
    return null_object;
}

fn int(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function int. Expected 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    switch (arg_tag) {
        .string => {
            const value = std.fmt.parseInt(i64, arg_data.string_type.items, 10) catch {
                const output = std.fmt.allocPrint(
                    allocator.*,
                    "int() got an invalid string: {s}",
                    .{arg_data.string_type.items},
                ) catch {
                    std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
                };
                return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                    std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
                };
            };
            return eval.object_pool.create(allocator.*, .integer, @ptrCast(&value)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
        .integer => {
            eval.object_pool.increase_ref(objects[0]);
            return objects[0];
        },
        else => {
            const output = std.fmt.allocPrint(
                allocator.*,
                "Argument of type: {s} is not supported by builtin 'int'",
                .{eval.object_pool.get_tag_string(objects[0])},
            ) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
            };
            return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
    }
    return null_object;
}

fn append(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length < 2) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function append. Expected atleast 2 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    if (arg_tag != .array) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Expected the first argument to be of type array. Got {s}",
            .{eval.object_pool.get_tag_string(objects[0])},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    for (1..length) |i| {
        eval.object_pool.increase_ref(objects[i]);
        arg_data.array.append(allocator.*, objects[i]) catch {
            std.debug.panic("Something has gone horribly wrong. Ran out of memory", .{});
        };
    }

    return null_object;
}

fn pop(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function pop. Expected 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    switch (arg_tag) {
        .array => {
            const out = arg_data.array.popOrNull() orelse {
                const output = std.fmt.allocPrint(
                    allocator.*,
                    "Attempting to pop from an empty array",
                    .{},
                ) catch {
                    std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
                };
                return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                    std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
                };
            };
            return out;
        },
        else => {
            const output = std.fmt.allocPrint(
                allocator.*,
                "Argument of type: {s} is not supported by builtin 'pop'. Expected ARRAY",
                .{eval.object_pool.get_tag_string(objects[0])},
            ) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
            };
            return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
                std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
            };
        },
    }
    return null_object;
}

fn get_keys(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length != 1) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function hash_keys. Expected 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    const arg_tag = eval.object_pool.get_tag(objects[0]);
    const arg_data = eval.object_pool.get_data(objects[0]);
    if (arg_tag != .hash_map) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Expected the first argument to be of type HASHMAP. Got {s}",
            .{eval.object_pool.get_tag_string(objects[0])},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }

    for (arg_data.hash_map.keys.items) |obj| {
        eval.object_pool.increase_ref(obj);
    }
    const array_storage = ObjectPool.InternalObject.ArrayType{
        .data = arg_data.hash_map.keys.items,
    };
    const return_object = eval.object_pool.create(allocator.*, .array, @ptrCast(&array_storage)) catch {
        std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
    };

    return return_object;
}

fn print(
    eval: *Evaluator,
    allocator: *const Allocator,
    objects: [*]const ObjectIndex,
    length: u32,
) callconv(.C) ObjectIndex {
    if (length == 0) {
        const output = std.fmt.allocPrint(
            allocator.*,
            "Wrong number of arguments to builin function print. Expected atleast 1 got {d}",
            .{length},
        ) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate a string", .{});
        };
        return eval.object_pool.create(allocator.*, .runtime_error, @ptrCast(&output)) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }
    var out = std.ArrayList(u8).init(allocator.*);
    var buffer: [4096]u8 = undefined;
    for (0..length) |i| {
        out.appendSlice(eval.object_pool.ToString(&buffer, objects[i]) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        }) catch {
            std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
        };
    }
    const out_str = out.toOwnedSlice();

    const return_object = eval.object_pool.create(allocator.*, .string, @ptrCast(&out_str)) catch {
        std.debug.panic("Something has gone horribly wrong. Could not allocate an object", .{});
    };
    return return_object;
}

const Evaluator = @import("evaluator.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
const ObjectPool = @import("object.zig");
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;
