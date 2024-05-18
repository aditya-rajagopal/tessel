/// Datastructure to store the AST
pub const Ast = @This();

/// A storage for tokens and we will use a tokenArrayIndex as the handel to link to a specific token
tokens: TokenArrayType.Slice,
/// An array store for all the nodes in the AST
/// we will work with handles to this array intead of creating a linked list
nodes: NodeArrayType.Slice,

pub const TokenArrayIndex = u32;
pub const TokenArray = struct {
    tag: token.TokenType, //
    start: u32,
    end: u32,
};
pub const TokenArrayType = std.MultiArrayList(TokenArray);

pub const NodeArrayType = std.MultiArrayList(Node);

pub const Node = struct {
    /// The tag representing the type of node the instance will be
    tag: Tag,
    main_token: TokenArrayIndex,
    node_data: NodeData,

    pub const NodeIndex = u32;
    pub const Tag = enum {
        /// The root node type
        ROOT,
        /// Variable decleration
        /// of the type const/var .IDENT = EXPRESSION
        VAR_STATEMENT,
    };

    /// The data a node will store pointing to a node in the node data store by index
    pub const NodeData = struct { lhs: NodeIndex, rhs: NodeIndex };
};

const token = @import("token.zig");
const std = @import("std");
