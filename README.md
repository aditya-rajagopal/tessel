# tessel

A small interpreted language that is compiled into bytecode and executed by a VM. It was mainly created as a tool to understand how languages work and there is no guarantee that every aspect of the language is formally correct.
Additionally many aspects of the language are not in a finished state as there is a rework in progress that is converting the language from a tree based interpreted language to one that is compiled to bytecode and run in a VM.

There are some choices made for the sake of implementing different types of functionality so things like floats are not in the language! The language cannot represent pi currently and that is a crime.

# Installation

This language is built in [zig](https://ziglang.org/download) and it needs to be downloaded to compile the project.


You may run the following to compile the language.
```
git clone https://github.com/aditya-rajagopal/tessel.git
cd tessle
zig build -Doptimize=ReleaseFast
```

This will generate the required executables in the `zig-out/bin` folder. 

You may use the following to build and immediately run the language
```
zig build run -Doptimize=ReleaseFast
```
This will trigger the REPL and allow you to start exectuing the code.

NOTE: This is not just a REPL but a way to view how the internals of the language work. Example:
```
tessel> zig build run -Doptimize=ReleaseFast
Welcome to tessel.
Type "exit()" without the quotes to quit or Ctrl+c
>> const a = 10;

AST:
        Node[0]         tag: ROOT                main_token: CONST       Childrens: lhs: 0, rhs:1
        Node[1]         tag: VAR_STATEMENT       main_token: CONST       Childrens: lhs: 2, rhs:3
        Node[2]         tag: IDENTIFIER          main_token: IDENT       Childrens: lhs: 0, rhs:0
        Node[3]         tag: INTEGER_LITERAL     main_token: INT         Childrens: lhs: 0, rhs:0
Extra Nodes:
         { 1 }
Instructions:
0000 load_const 0
0003 set_global 0


Functions:


Instructions bytes: { 0, 0, 0, 16, 0, 0 }
Instruction pointer: 6
Stack Ptr: 0
Output >> null
>>
```

You may also provide either the exectutable or the `zig build run` command with a file to execute rather than a REPL.

```
zig build run -Doptimize=ReleaseFast -- ./test.tes
```
OR
```
./zig-out/bin/tessel.exe ./test.tes
```

NOTE: Currently due to the migration to the bytecode-VM system builtin functions like `print` are not available yet. So to print some result it needs to be the last expression of the code. Will be complete soom(TM).

## Testing

The code has extensive test cases to ensure correctness and they can be run with the following.

```
zig build test
```

Most files contain tests but here are some example tests which are also useful to see how the language works!
- [vm.zig](https://github.com/aditya-rajagopal/tessel/blob/136f89dc18661b62f4c748b8fded121616441514/src/tessel/vm.zig#L648)
- [compiler.zig](https://github.com/aditya-rajagopal/tessel/blob/136f89dc18661b62f4c748b8fded121616441514/src/tessel/compiler.zig#L697)
- [parser.zig](https://github.com/aditya-rajagopal/tessel/blob/136f89dc18661b62f4c748b8fded121616441514/src/tessel/parser.zig#L1154)


## Benchmarking

There is also a benchmark that can be run that compares the same recursive fibonacci function to a decently high depth of 35.
This is not an amazing benchmark but does show a good example of the language managing stack frame memory usage and speed of context switching.

```
zig build benchmark -Doptimize=ReleaseFast
>>
Testing ./test.tes Using tessel:
Fibonacci in Tessel: result: 9227465 time: 2.146s
9227465
Time to run python: 902.478ms
```

NOTE: the command python must exist in your machine for this to exist.


# TODO

Currently the language is in a transition state from a AST-evaluator approach to exectuing the code to the AST compiled into bytecode and run in a VM.
This implies that there are many aspects of the language that are in an incomplete state and I will come back to them when i come back to exploring languages

- [ ] Transition builtins to the bytecode interpreter
- [ ] Add supports for floats.
- [ ] Add more builtins especially for range, enumerate and file IO.
- [ ] Add support for structures and appending functions to them. Not classes more akin to GO.
- [ ] Complete transition of all properties of function. Currently currying and closures are not 100% working as intended.

# Language

The language is inspired by python with some interesting things added mostly to explore how they might be implemented in a language

General rules:
    - Lines end in a ';'
    - Variables declared with const cannot be mutated and will raise an error during compilation
    - identifiers cannot be redeclared in the same scope
    - There is only a while loop for now. More can be added later.
        - There are break; and continue; statements to mutate loops
    - Comments start with a '//'
    - Functions using variables from parent scopes will have the value of the variable stored int he stack frame allowing closures
    - Scopes are defined with { }. Brackets must match.
    - Conditions for while and if-else blocks must be enclosed in ( )
    - Arrays are defined within [ ]
    - Hasmaps are defined with { }. These are similar to python dictionaries. 

Please look at the test cases described in the Tests section to see many more example usage. 

## Everything is an expression

The language is fundamentally based on the fact that everything is an expression. Functions are an expression, while loops are expression, and even if-else blocks are expession.

This also means that the result of last expression in a block is the implicit return value.

```
fn() { 5 + 10 }()
// Output: 15
```

```
const a = 2 * 2;
const b = a + 3;
const c = if ( a < b ) { a + 3; } else { b; };
c;
// Output: 7
```

## Variables

Variables are dynamic and do not have type associated with them externally. A vriable must be marked const or var depending on usage.
const variables allow some optimizations if you know they are not going to be modified.

```
var a = \"foo\";
const b = \"bar\";
a = a + b;
a;
// Output: foobar

b = a + b
// Error!
```

Techincal note: Variables have memory slots alloted to them depending on the scope. Global variables in the heap and variables local
to the scope on the stack. Functions, arrays, hashmaps, and strings are always stored in the heap and only references are stored in the stack.
This does mean that passing an array to a function can allow you to mutate the array with append().

## Literals
The language supports the following datatype

- Integers. Stored as int64. Always passed as value.
- booleans: `false` and `true`. All booleans are just references to a global object and not created every time.
- string: array of ascii characters eclosed in "".
- Arrays: list of objects. Can contain a mixture of types. 
- Hashmap: key, value pairs similar to python dictionaries. Keys may be integer, boolean, and string.

## Loops

Currently only while loops are available. You may use break and continue statements

```
var a = "";
var i = 0;
while (i < 5) {
  if (i == 2) {
      a = a + "two ";
      i = i + 1;
      continue;
  };
  const b = "local ";
  a = a + b;
  i = i + 1;
}
a;
// Output: local local two local local 
```

## Functions

Functions are expressions and can be defined and used in place:

```
const a = fn(x, y) { x + y}(1, 2);
a;
// Output: 3
```

You can ofcourse store the function in a variable to use it later

```
var a = fn() {
return 5 + 10
};
a();
// Output: 15
```

Functions are also values so you can return functions or pass functions as arguments. 

```
const call_fn = fn(x, y) {
    x(y); // Note implicit return
};
const func = fn(x) { return 2 * x; };
call_fn(func, 4);
// Output: 8
```
