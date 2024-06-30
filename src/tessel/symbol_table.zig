pub const SymbolTable = @This();

map: std.StringHashMapUnmanaged(Symbol),
current_index: u32,

pub const SymbolIndex = u32;

pub const Symbol = struct {
    // name: []const u8,
    scope: SymbolScope,
    index: SymbolIndex,
    type: Tag,
    depth: u16 = 0,

    pub const Tag = enum(u1) {
        constant = 0,
        variable = 1,
    };
};

pub const SymbolScope = enum(u2) {
    global = 0,
    block = 1,
    local = 2,
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
    depth: u16,
) (SymbolError || Allocator.Error)!Symbol {
    if (self.map.contains(key)) {
        return Error.IdentifierRedecleration;
    } else {
        var local_key = std.ArrayList(u8).init(allocator);
        try local_key.appendSlice(key);
        const local_key_str = try local_key.toOwnedSlice();
        const data = Symbol{
            // .name = local_key_str,
            .index = self.current_index,
            .scope = scope,
            .type = tag,
            .depth = depth,
        };
        try self.map.put(allocator, local_key_str, data);
        self.current_index += 1;
        return data;
    }
}

pub fn resolve(self: *SymbolTable, key: []const u8) SymbolError!Symbol {
    return self.map.get(key) orelse return Error.UnkownIdentifier;
}

pub fn resolvePtr(self: *SymbolTable, key: []const u8) SymbolError!*Symbol {
    return self.map.getPtr(key) orelse return Error.UnkownIdentifier;
}

pub fn print_table_to_stderr(self: *SymbolTable) void {
    var it = self.map.iterator();
    std.debug.print("\tSymbol Table: \n", .{});
    while (it.next()) |value_ptr| {
        std.debug.print(
            "\t\tKey: \"{s}\"\ttype:{s}\tscope:{s}\tindex:{d}\tdepth:{d}\n",
            .{
                value_ptr.key_ptr.*,
                @tagName(value_ptr.value_ptr.type),
                @tagName(value_ptr.value_ptr.scope),
                value_ptr.value_ptr.index,
                value_ptr.value_ptr.depth,
            },
        );
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ObjectIndex = @import("object.zig").ObjectIndex;
const null_object = @import("object.zig").null_object;
