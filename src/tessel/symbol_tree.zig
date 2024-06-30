pub const SymbolTree = @This();

tree: std.ArrayList(SymbolNode),
allocator: Allocator,

pub const SymbolTreeError = error{ReinitialisingGlobalTree};
pub const Error = SymbolTreeError || Allocator.Error || SymbolTable.SymbolError;

pub const SymbolIndex = u16;
pub const SymbolNode = struct {
    parent: ?SymbolIndex,
    depth: u16,
    num_locals: u32 = 0,
    map: SymbolTable,
};

pub fn init(allocator: Allocator) SymbolTree {
    const stree = SymbolTree{
        .tree = std.ArrayList(SymbolNode).init(allocator),
        .allocator = allocator,
    };
    return stree;
}

pub fn deinit(self: *SymbolTree) void {
    for (self.tree.items) |*node| {
        node.map.deinit(self.allocator);
    }
    self.tree.deinit();
}

pub fn get_depth(self: *SymbolTree, location: SymbolIndex) u16 {
    std.debug.assert(location < self.tree.items.len);
    return self.tree.items[location].depth;
}

pub fn create_table(self: *SymbolTree, parent: ?SymbolIndex) Error!SymbolIndex {
    if (parent) |p| {
        std.debug.assert(self.tree.items.len > 0);
        const pos: u16 = @intCast(self.tree.items.len);
        const table = self.tree.items[p];
        try self.tree.append(SymbolNode{
            .parent = p,
            .map = SymbolTable.init(),
            .depth = table.depth + 1,
        });
        return pos;
    } else {
        if (self.tree.items.len != 0) {
            return SymbolTreeError.ReinitialisingGlobalTree;
        }
        try self.tree.append(SymbolNode{
            .parent = null,
            .map = SymbolTable.init(),
            .depth = 0,
        });
        return 0;
    }
}

pub fn define(
    self: *SymbolTree,
    location: SymbolIndex,
    key: []const u8,
    tag: SymbolTable.Symbol.Tag,
) !SymbolTable.Symbol {
    std.debug.assert(location < self.tree.items.len);
    const symbol_node = &self.tree.items[location];

    const scope: SymbolTable.SymbolScope = if (symbol_node.parent) |_| .local else .global;
    const symbol = try symbol_node.map.define(self.allocator, key, tag, scope, symbol_node.depth);
    self.tree.items[location].num_locals += 1;
    return symbol;
}

pub fn define_node_data(
    self: *SymbolTree,
    location: SymbolIndex,
    key: []const u8,
    tag: SymbolTable.Symbol.Tag,
) !Ast.Node.NodeData {
    const symbol = try self.define(location, key, tag);
    return symbol_to_identifier(symbol, self.get_depth(location));
}

pub fn resolve(self: *SymbolTree, location: SymbolIndex, key: []const u8) SymbolTable.SymbolError!SymbolTable.Symbol {
    std.debug.assert(location < self.tree.items.len);
    const symbol_node = &self.tree.items[location];
    return symbol_node.map.resolve(key) catch |err| switch (err) {
        SymbolTable.SymbolError.UnkownIdentifier => |e| {
            if (symbol_node.parent) |p| {
                return self.resolve(p, key);
            } else {
                return e;
            }
        },
        SymbolTable.SymbolError.IdentifierRedecleration => unreachable,
    };
}

// pub fn resolvePtr(self: *SymbolTree, location: SymbolIndex, key: []const u8) SymbolTable.SymbolError!*SymbolTable.Symbol {
//     std.debug.assert(location < self.tree.items.len);
//     const symbol_node = &self.tree.items[location];
//     return symbol_node.map.resolvePtr(key) catch |err| switch (err) {
//         SymbolTable.SymbolError.UnkownIdentifier => |e| {
//             if (symbol_node.parent) |p| {
//                 return self.resolvePtr(p, key);
//             } else {
//                 return e;
//             }
//         },
//         SymbolTable.SymbolError.IdentifierRedecleration => unreachable,
//     };
// }

pub fn resolve_node_data(
    self: *SymbolTree,
    location: SymbolIndex,
    key: []const u8,
) SymbolTable.SymbolError!Ast.Node.NodeData {
    const symbol = try self.resolve(location, key);
    return symbol_to_identifier(symbol, self.get_depth(location));
}

pub fn identifier_to_symbol(node_data: Ast.Node.NodeData) SymbolTable.Symbol {
    return SymbolTable.Symbol{
        .type = @enumFromInt((node_data.rhs >> 2) & 1),
        .scope = @enumFromInt(node_data.rhs & 3),
        .index = node_data.lhs,
        .depth = @intCast(node_data.rhs >> 3),
    };
}

pub fn symbol_to_identifier(symbol: SymbolTable.Symbol, scope_depth: u16) Ast.Node.NodeData {
    var relative_depth: u16 = 0;
    if (symbol.depth <= scope_depth) {
        relative_depth = scope_depth - symbol.depth;
    } else {
        unreachable;
    }
    return Ast.Node.NodeData{
        .lhs = symbol.index,
        .rhs = (@as(u32, @intCast(@intFromEnum(symbol.type))) << 2) | @intFromEnum(symbol.scope) | (relative_depth << 3),
    };
}

pub fn print_tree_to_stderr(self: *SymbolTree) void {
    std.debug.print("\n", .{});
    std.debug.print("Symbol Tree\n", .{});
    for (self.tree.items, 0..) |*value, i| {
        var parent: i32 = 0;
        if (value.parent) |p| {
            parent = @intCast(p);
        } else {
            parent = -1;
        }
        std.debug.print("\tTree info: parent:{any}, depth:{d}, num_locals:{d}\n", .{
            parent,
            value.depth,
            value.num_locals,
        });
        const symbol_node = &self.tree.items[i];
        symbol_node.map.print_table_to_stderr();
        std.debug.print("\n", .{});
    }
}

test "Symbol Tree init" {
    var tree = SymbolTree.init(testing.allocator);
    defer tree.deinit();
}

test "Symbol Tree creation" {
    var tree = SymbolTree.init(testing.allocator);
    defer tree.deinit();

    const table_index = tree.create_table(null);
    try testing.expectEqual(0, table_index);
}

test "Symbol Tree Create global variable" {
    var tree = SymbolTree.init(testing.allocator);
    defer tree.deinit();

    const table_index = try tree.create_table(null);
    try testing.expectEqual(0, table_index);
    const output = try tree.define(table_index, "a", .constant);
    // try testing.expectEqualSlices(u8, "a", output.name);
    try testing.expectEqual(.constant, output.type);
    try testing.expectEqual(.global, output.scope);
    try testing.expectEqual(0, output.index);

    const resolved = try tree.resolve(table_index, "a");
    // try testing.expectEqualSlices(u8, "a", resolved.name);
    try testing.expectEqual(.constant, resolved.type);
    try testing.expectEqual(.global, resolved.scope);
    try testing.expectEqual(0, resolved.index);
}

test "Symbol Tree Create local variable" {
    var tree = SymbolTree.init(testing.allocator);
    defer tree.deinit();

    const table_index = try tree.create_table(null);
    try testing.expectEqual(0, table_index);
    const table = tree.tree.items[table_index];
    try testing.expectEqual(null, table.parent);
    _ = try tree.define(table_index, "a", .constant);
    _ = try tree.define(table_index, "b", .constant);

    var resolve_a = try tree.resolve(table_index, "a");
    // try testing.expectEqualSlices(u8, "a", resolve_a.name);
    try testing.expectEqual(.constant, resolve_a.type);
    try testing.expectEqual(.global, resolve_a.scope);
    try testing.expectEqual(0, resolve_a.index);

    var resolve_b = try tree.resolve(table_index, "b");
    // try testing.expectEqualSlices(u8, "b", resolve_b.name);
    try testing.expectEqual(.constant, resolve_b.type);
    try testing.expectEqual(.global, resolve_b.scope);
    try testing.expectEqual(1, resolve_b.index);

    const first_local = try tree.create_table(table_index);
    const first_table = tree.tree.items[first_local];
    try testing.expectEqual(1, first_local);
    try testing.expectEqual(0, first_table.parent);

    resolve_a = try tree.resolve(first_local, "a");
    // try testing.expectEqualSlices(u8, "a", resolve_a.name);
    try testing.expectEqual(.constant, resolve_a.type);
    try testing.expectEqual(.global, resolve_a.scope);
    try testing.expectEqual(0, resolve_a.index);

    resolve_b = try tree.resolve(first_local, "b");
    // try testing.expectEqualSlices(u8, "b", resolve_b.name);
    try testing.expectEqual(.constant, resolve_b.type);
    try testing.expectEqual(.global, resolve_b.scope);
    try testing.expectEqual(1, resolve_b.index);

    _ = try tree.define(first_local, "c", .constant);
    _ = try tree.define(first_local, "d", .constant);

    const resolve_c = try tree.resolve(first_local, "c");
    // try testing.expectEqualSlices(u8, "c", resolve_c.name);
    try testing.expectEqual(.constant, resolve_c.type);
    try testing.expectEqual(.local, resolve_c.scope);
    try testing.expectEqual(0, resolve_c.index);

    const resolve_d = try tree.resolve(first_local, "d");
    // try testing.expectEqualSlices(u8, "d", resolve_d.name);
    try testing.expectEqual(.constant, resolve_d.type);
    try testing.expectEqual(.local, resolve_d.scope);
    try testing.expectEqual(1, resolve_d.index);

    const second_local = try tree.create_table(first_local);
    const second_table = tree.tree.items[second_local];
    try testing.expectEqual(2, second_local);
    try testing.expectEqual(1, second_table.parent);

    resolve_a = try tree.resolve(second_local, "a");
    // try testing.expectEqualSlices(u8, "a", resolve_a.name);
    try testing.expectEqual(.constant, resolve_a.type);
    try testing.expectEqual(.global, resolve_a.scope);
    try testing.expectEqual(0, resolve_a.index);

    resolve_b = try tree.resolve(second_local, "b");
    // try testing.expectEqualSlices(u8, "b", resolve_b.name);
    try testing.expectEqual(.constant, resolve_b.type);
    try testing.expectEqual(.global, resolve_b.scope);
    try testing.expectEqual(1, resolve_b.index);

    _ = try tree.define(second_local, "e", .constant);
    _ = try tree.define(second_local, "f", .constant);

    const resolve_e = try tree.resolve(second_local, "e");
    // try testing.expectEqualSlices(u8, "e", resolve_e.name);
    try testing.expectEqual(.constant, resolve_e.type);
    try testing.expectEqual(.local, resolve_e.scope);
    try testing.expectEqual(0, resolve_e.index);

    const resolve_f = try tree.resolve(second_local, "f");
    // try testing.expectEqualSlices(u8, "f", resolve_f.name);
    try testing.expectEqual(.constant, resolve_f.type);
    try testing.expectEqual(.local, resolve_f.scope);
    try testing.expectEqual(1, resolve_f.index);
}

test "Random things" {
    var tree = SymbolTree.init(testing.allocator);
    defer tree.deinit();

    const table_index = try tree.create_table(null);
    _ = try tree.define(table_index, "e", .constant);

    const second_local = try tree.create_table(table_index);
    const hash2 = try tree.define(second_local, "e", .variable);

    const node = Ast.Node{
        .tag = .IDENTIFIER,
        .main_token = 0,
        .node_data = symbol_to_identifier(hash2, 1),
    };
    try testing.expectEqual(0, node.node_data.lhs);
    try testing.expectEqual(0b110, node.node_data.rhs);

    const symbol = identifier_to_symbol(node.node_data);
    try testing.expectEqual(.variable, symbol.type);
    try testing.expectEqual(.local, symbol.scope);
}

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const SymbolTable = @import("symbol_table.zig");
const Ast = @import("ast.zig");
