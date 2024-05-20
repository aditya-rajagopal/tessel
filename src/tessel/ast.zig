/// Datastructure to store the AST
pub const Ast = @This();

source_buffer: [:0]const u8,
/// A storage for tokens and we will use a tokenArrayIndex as the handel to link to a specific token
tokens: TokenArrayType.Slice,
/// An array store for all the nodes in the AST
/// we will work with handles to this array intead of creating a linked list
nodes: NodeArrayType.Slice,
/// Some expressions need more than 2 children so we can store the data of the nodes that we need
/// in this array. Depending on the type of node being evaluated we can access the nodes in the same way they were
/// stored
extra_data: []Node.NodeIndex,
errors: []const Error,

pub const TokenArrayIndex = u32;
pub const TokenArray = struct {
    tag: token.TokenType, //
    start: u32,
    end: u32,
};
pub const TokenArrayType = std.MultiArrayList(TokenArray);

pub const NodeArrayType = std.MultiArrayList(Node);

pub fn deinit(self: *Ast, allocator: Allocator) void {
    self.tokens.deinit(allocator);
    self.nodes.deinit(allocator);
    allocator.free(self.errors);
    allocator.free(self.extra_data);
}

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
        /// Return control statment
        /// return <expression>;
        RETURN_STATEMENT,
        /// Expression statement
        /// <expression>;
        EXPRESSION_STATEMENT,
        /// Indicates a block of statments
        /// The statments are stored in teh extra_data array
        /// lhs = starting point in the extra_data array
        /// rhs = ending point in extra data array
        /// each element in the range will be 1 statement that needs to be executed
        BLOCK,
        /// Has no child data nodes it is used in expressions
        IDENTIFIER,
        /// Has no child data nodes
        INTEGER_LITERAL,
        /// Has no child data nodes
        BOOLEAN_LITERAL,
        /// ! lhs. rhs is empty.
        BOOL_NOT,
        /// - lhs. rhs is empty
        NEGATION,
        /// Comparison lhs == rhs. == token is the main_token
        DOUBLE_EQUAL,
        /// Comparison lhs != rhs.  != token is the main_token
        NOT_EQUAL,
        /// Comparison lhs < rhs < token is the main_token
        LESS_THAN,
        /// Comparison lhs > rhs > token is the main_token
        GREATER_THAN,
        /// Comparison lhs <= rhs <= token is the main_token
        LESS_THAN_EQUAL,
        /// Comparison lhs >= rhs >= token is the main_token
        GREATER_THAN_EQUAL,
        /// lhs + rhs + token is the main_token
        ADDITION,
        /// lhs - rhs - token is the main_token
        SUBTRACTION,
        /// lhs * rhs * token is the main_token
        MULTIPLY,
        /// lhs / rhs / token is the main_token
        DIVIDE,
        ///
        IF,
        NAKED_IF,
        IF_ELSE,
        FUNCTION_PARAMETER_BLOCK,
        FUNCTION_EXPRESSION,
        FUNCTION_CALL,

        pub fn get_operator_string(tag: Tag) []const u8 {
            switch (tag) {
                .BOOL_NOT => return "!",
                .NEGATION => return "-",
                .DOUBLE_EQUAL => return "==",
                .NOT_EQUAL => return "!=",
                .LESS_THAN => return "<",
                .GREATER_THAN => return ">",
                .LESS_THAN_EQUAL => return "<=",
                .GREATER_THAN_EQUAL => return ">=",
                .ADDITION => return "+",
                .SUBTRACTION => return "-",
                .MULTIPLY => return "*",
                .DIVIDE => return "/",
                else => return "",
            }
        }
    };

    /// The data a node will store pointing to a node in the node data store by index
    pub const NodeData = struct { lhs: NodeIndex, rhs: NodeIndex };
};

pub const Error = struct {
    tag: Tag,
    token: TokenArrayIndex,
    expected: token.TokenType,

    pub const Tag = enum {
        expected_identifier_after_const, //
        expected_assignent_after_var_decl,
        expected_expression,
        expected_token,
        expected_closing_rparen,
        expected_prefix_expression,
        expected_parens_around_if_condition,
    };
};

const token = @import("token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
