# Objective-C Compiler

## Requirements

To use this project you must have the following:
* C++ compiler: g++ (or other) with support of C++17;
* Flex utility;
* Bison utility;
* C++ standard library: must include support for std::filesystem (flag -lstdc++fs, relevant for GCC up to version 9.x).

It is recommended to install the latest version of [MSYS2](https://www.msys2.org/) to access all utilities required to use the compiler.

## Build

Before you start using the compiler you must build the project. To do so you need to:
* Navigate to the project directory;
* Use `make` command to build the project;

You can use `make clean` command to delete all the files generated in the building process or `make rebuild` command to rebuild the project.

## Usage

To use this compiler you should have a `.m` file with Objective-C code (address [Supported features](#supported-features)).

Execute `objc_compiler.exe file_name.m` (for cmd) or `./objc_compiler.exe file_name.m` (for MSYS2 environments) command to process it. On completion the compiler will generate several files:
* `file_name_tokens.txt` &mdash; contains the tokens that were made by the lexer;
* `file_name_ast.dot` &mdash; contains the DOT representation of the AST built by the parser.

To visualize the AST use online visualizers [Graphviz Online](https://dreampuf.github.io/GraphvizOnline), [Edotor](https://edotor.net/) or [Webgraphviz](http://www.webgraphviz.com/) or use the Graphviz utility (download [here](https://graphviz.org/download/)).

If you are using the Graphviz utility execute one of these command to generate the AST visualization:
* `dot -Tpng graph.dot -o graph.png` for a PNG image;
* `dot -Tpdf graph.dot -o graph.pdf` for a PDF file;
* `dot -Tsvg graph.dot -o graph.svg` for an SVG image;
* `dot -Tjpg graph.dot -o graph.jpg` for a JPG image.

## Supported Features

Listed below are the features that this compiler supports (at least in some way):

1. **Class Declarations**
* Class interfaces (`@interface`)
* Class implementations (`@implementation`)
* Inheritance (via `: ParentClass`)
* Forward class declarations (`@class Class1, Class2;`)

2. **Access Modifiers**
* `@public`
* `@protected`
* `@private`

3. **Methods**
* Instance methods (start with `-`)
* Class methods (start with `+`)
* Complex selectors with parameters (`methodName:(type)param`)
* Multiple parameters (`methodName1:(type1)param1 methodName2:(type2)param2`)
* Return types (including `void`)

4. **Properties**
* Property declaration (`@property`)
* Property attributes (only `readonly` and `readwrite`)

5. **Data Types**
* Basic types (`int`, `float`, `char`, `BOOL`)
* Class types (`ClassName*`)
* Special type `id` (dynamic typing)
* Arrays (multidimensional)
* `void` type for methods

6. **Variables and Declarations**
* Variable declaration with initialization
* Arrays with specified dimensions
* Initializer lists for arrays

7. **Operators and Expressions**
* Arithmetic (`+`, `-`, `*`, `/`, unary minus)
* Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`)
* Logical (`&&`, `||`, `!`)
* Assignment `=`
* Increment/decrement `++`, `--` (only postfix)
* Access operators (`.`, `->`)
* Method calls (`[receiver method]`)
* Array operations (index access `[]`)

8. **Literals**
* Numeric (`int`, `float`)
* Character (`char`)
* String (`char*`)
* Objective-C literals:
  - `@"string"` (`NSString` strings)
  - `@YES`, `@NO` (object `BOOL`)
  - `@42`, `@3.14` (object numbers)
  - `@[]` (arrays)
  - `@()` (boxed expressions)
* `nil`

9. **Control Flow Structures**
* Conditional (`if`, `if-else`)
* Loops:
  - `for`
  - `for-in` (fast enumeration: `for (id item in array)`)
  - `while`
  - `do-while`
* Return statement (`return`)

10. **Message Passing**
* Sending messages to objects (`[object method]`)
* Sending messages to superclass (`[super method]`)
* Sending messages to class (`[ClassName method]`)
* Messages with parameters (`[obj method:arg]`)

11. **Special Identifiers**
* `self` - current object
* `super` - parent class
* `nil` - null pointer

12. **C Functions**
* Function declaration and definition
* Function parameters
* Recursive calls

*Limitations (not supported in this grammar)*:
* Protocols (@protocol)
* Named categories (@interface ClassName (CategoryName))
* Blocks (closures)
* ARC specifiers (__strong, __weak, etc.)
* Generic types
* @synthesize, @dynamic syntax
* Property threading attributes
* Some property attributes (nonatomic, copy, retain, etc.)