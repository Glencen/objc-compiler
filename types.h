#ifndef TYPES_H
#define TYPES_H

enum class ValueKind {
    NONE,
    INT_LIT,
    FLOAT_LIT,
    BOOL_LIT,
    CHAR_LIT,
    STRING_LIT,
    NIL,
    OBJC_INT_LIT,
    OBJC_FLOAT_LIT,
    OBJC_BOOL_LIT,
    OBJC_STRING_LIT,
    IDENTIFIER,
    CLASS_NAME
};

enum class ReceiverKind {
    NONE,
    EXPR,
    CLASS_NAME,
    SUPER
};

enum class MsgSelectorKind {
    NONE,
    SIMPLE_SEL,
    ARGUMENT_LIST
};

enum class ExprKind {
    NONE,
    IDENTIFIER,
    LITERAL,
    OBJC_ARRAY_LITERAL,
    OBJC_BOXED_EXPR,
    NIL,
    BOXED_EXPR,
    MESSAGE,
    SELF,
    UNARY_MINUS,
    NOT,
    POST_INC,
    POST_DEC,
    ADDITION,
    SUBTRACTION,
    MULTIPLICATION,
    DIVISION,
    EQUAL,
    NOT_EQUAL,
    GREATER,
    LESS,
    LESS_OR_EQUAL,
    GREATER_OR_EQUAL,
    AND,
    OR,
    ASSIGN,
    ARRAY_ACCESS,
    FUNCTION_CALL,
    DOT,
    ARROW
};

enum class TypeKind {
    NONE,
    INT,
    FLOAT,
    BOOL,
    CHAR,
    TYPE_ID,
    CLASS_NAME,
    VOID
};

enum class StmtKind {
    NONE,
    EMPTY,
    EXPR,
    RETURN,
    IF,
    IF_ELSE,
    FOR_WITH_EXPR,
    FOR_WITH_DECL,
    FOR_IN,
    TYPED_FOR_IN,
    WHILE,
    DO_WHILE,
    COMPOUND,
    DECLARATION
};

enum class ParamDeclKind {
    NONE,
    IDENTIFIER,
    ARRAY,
    SIZED_ARRAY,
    FLEXIBLE_ARRAY
};

enum class MethodParamKind {
    NONE,
    IDENTIFIER,
    ARRAY,
    SIZED_ARRAY,
    FLEXIBLE_ARRAY
};

enum class MethodDefKind {
    NONE,
    ID,
    SEL
};

enum class MethodDeclKind {
    NONE,
    ID,
    SEL
};

enum class Attribute {
    NONE,
    READONLY,
    READWRITE,
    CLASS
};

enum class InitializerKind {
    NONE,
    EXPR,
    ARRAY
};

enum class InitDeclKind {
    NONE,
    DECLARATOR,
    INITIALIZED,
    ARRAY_INITIALIZED
};

enum class AccessModifier {
    NONE,
    PUBLIC,
    PROTECTED,
    PRIVATE
};

enum class ExternalDeclKind {
    NONE,
    INTERFACE,
    IMPLEMENTATION,
    CLASS_FW_DECL_LIST,
    FUNC_DECL,
    FUNC_DEF
};

#endif
