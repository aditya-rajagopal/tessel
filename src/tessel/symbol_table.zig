pub const SymbolTable = @This();

map: std.StringHashMapUnmanaged(Symbol),
current_index: u32,

pub const SymbolIndex = u32;

pub const Symbol = struct {
    name: []const u8,
    scope: SymbolScope,
    index: SymbolIndex,
    type: Tag,

    pub const Tag = enum {
        constant,
        variable,
    };
};

pub const SymbolScope = enum {
    global,
    block,
};

pub const SymbolError = error{ IdentifierRedecleration, UnkownIdentifier };
pub const Error = SymbolError || Allocator.Error;

pub fn init() SymbolTable {
    return SymbolTable{
        .map = .{},
        .current_index = 0,
    };
}

pub fn deinit(self: *SymbolTable, allocator: Allocator) void {
    var kit = self.map.keyIterator();
    while (kit.next()) |value_ptr| {
        allocator.free(value_ptr.*);
    }
    self.map.deinit(allocator);
}

pub fn define(
    self: *SymbolTable,
    allocator: Allocator,
    key: []const u8,
    tag: Symbol.Tag,
    scope: SymbolScope,
) !Symbol {
    if (self.map.contains(key)) {
        return Error.IdentifierRedecleration;
    } else {
        var local_key = std.ArrayList(u8).init(allocator);
        try local_key.appendSlice(key);
        const local_key_str = try local_key.toOwnedSlice();
        const data = Symbol{
            .name = local_key_str,
            .index = self.current_index,
            .scope = scope,
            .type = tag,
        };
        try self.map.put(allocator, local_key_str, data);
        self.current_index += 1;
        return data;
    }
}

pub fn resolve(self: *SymbolTable, key: []const u8) !Symbol {
    return self.map.get(key) orelse return Error.UnkownIdentifier;
}

pub fn resolvePtr(self: *SymbolTable, key: []const u8) !*Symbol {
    return self.map.getPtr(key) orelse return Error.UnkownIdentifier;
}

pub fn print_env_hashmap_stderr(self: *SymbolTable) void {
    var it = self.map.iterator();
    std.debug.print("\n", .{});
    std.debug.print("Environment Hash map: \n", .{});
    while (it.next()) |value_ptr| {
        std.debug.print(
            "\tKey: \"{s}\"\t Value: name: {s}\tscope:{s}\tindex: {d}\n",
            .{
                value_ptr.key_ptr.*,
                value_ptr.value_ptr.name,
                @tagName(value_ptr.value_ptr.scope),
                value_ptr.value_ptr.index,
            },
        );
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;