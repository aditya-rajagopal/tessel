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
integer_literals: []const i64,

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
    allocator.free(self.integer_literals);
}

pub fn get_token_literal(ast: *const Ast, tok_loc: Ast.TokenArrayIndex) []const u8 {
    const tok = ast.tokens.get(tok_loc);
    return ast.source_buffer[tok.start..tok.end];
}

pub const Node = struct {
    /// The tag representing the type of node the instance will be
    tag: Tag,
    main_token: TokenArrayIndex,
    node_data: NodeData,

    pub const NodeIndex = u32;
    /// Useful when storing information into the extra_data array
    pub const ExtraDataRange = struct {
        start: NodeIndex,
        end: NodeIndex,
    };
    pub const Tag = enum {
        /// The root node type
        ROOT,
        /// Variable decleration
        /// of the type const/var .IDENT = EXPRESSION
        /// lhs = identifier node
        /// rhs = expression to store into the node
        VAR_STATEMENT,
        /// Return control statment
        /// return <expression>;
        /// lhs = return expression node;
        /// rhs = 0;
        RETURN_STATEMENT,
        /// Assign
        /// main_token = =
        /// lhs = identifier to store expression result into
        /// rhs = expression
        ASSIGNMENT_STATEMENT,
        /// Expression statement
        /// <expression>;
        /// lhs = expression start node;
        /// rhs = 0;
        EXPRESSION_STATEMENT,
        /// Break statemnt
        /// breaks out a loop block
        /// main_token = break
        BREAK_STATEMENT,
        /// continue statemnt
        /// continue to the next loop iteration
        /// main_token = continue
        CONTINUE_STATEMENT,
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
        /// main_token is the location of the litera
        /// lhs = 0 if false literal and 1 if true literal
        BOOLEAN_LITERAL,
        /// main_token is the location of the litera
        /// lhs = start of the string in sourc rhs = end of string
        STRING_LITERAL,
        /// main_token = [
        /// lhs = start in the extra data array of the expressions that make up the array
        /// rhs = end in the extra data array of the expressions that make up the array
        ARRAY_LITERAL,
        /// <int/bool/string literal><colon><expression>
        /// main_token = :
        /// lhs: key
        /// rhs: value
        HASH_ELEMENT,
        /// {<hash_map>, <hash_map>, ...}
        /// main_token = {
        /// lsh = start in the extra data array of the maps that make up the hash
        /// rhs = end in the extra data array of the maps that make up the hash
        HASH_LITERAL,
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
        /// If statement without an else block
        /// lhs = condition expression node
        /// rhs = block to evaulate if lhs is true
        NAKED_IF,
        /// If statement with an else block
        /// lhs = condition expression node
        /// rhs = start location in extra_nodes block to read from
        /// extra_data[rhs] => if block
        /// extra_data[rhs+1] => else block node
        IF_ELSE,
        /// While loop
        /// main_token = while
        /// lhs = condition expression
        /// rhs = block to execute if lhs is true and then loop back to lhs condition
        WHILE_LOOP,
        /// fn <FUNCTION_PARAMETER_BLOCK> <FUNCTION_BLOCK>
        /// function expression main_token = fn
        /// lhs = <FUNCTION_PARAMETER_BLOCK>
        /// rhs = <FUNCTION_BLOCK> of type BLOCK
        FUNCTION_EXPRESSION,
        /// Parameters of a function decleration.
        /// this is a list of identifiers
        /// main_token = (
        /// lhs = node in ast of the first identifier
        /// rhs - 1 = node to search till for all the identifiers
        FUNCTION_PARAMETER_BLOCK,
        /// A function call expression
        /// main_token = (
        /// lhs = node of identifier with function's name
        /// rhs = location in extra_datas array
        /// the argument expressions nodes are in extra_data[rhs..rhs+2]
        FUNCTION_CALL,
        /// <lbracket><integer><colon><integer><rbracket>
        /// main_token = :
        /// lhs = expression/identifier/literal that returns an array or strign
        /// rhs = position into extra data for the 2 expressions that evaluate to the start and end of the range.
        INDEX_RANGE,
        /// Index into an array literal/variable
        /// main_token = [
        /// lhs = expression/identifier/literal that returns an array, string or hashmap
        /// rhs = expression that evaluates to the index.
        INDEX_INTO,

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
        expected_map,
        illegal_break_or_continue,
        expected_token,
        expected_closing_rparen,
        expected_prefix_expression,
        expected_parens_around_if_condition,
        variable_redecleration,
        unkown_variable,
        reassigning_const,
    };
};

const token = @import("token.zig");
const std = @import("std");
const Allocator = std.mem.Allocator;
