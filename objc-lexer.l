%{
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include "objc-parser.hpp"

#define STRING_BUF_SIZE 1024

typedef enum {
    TOK_KEYWORD,
    TOK_IDENTIFIER,
    TOK_CLASS_NAME,
    TOK_OPERATOR,
    TOK_DELIMITER,
    TOK_INTEGER,
    TOK_FLOAT,
    TOK_STRING,
    TOK_CHAR,
    TOK_COMMENT,
    TOK_OBJC_INTERFACE,
    TOK_OBJC_IMPLEMENTATION,
    TOK_OBJC_END,
    TOK_OBJC_PROPERTY,
    TOK_UNKNOWN,
    TOK_ERROR
} token_type_t;

extern YYSTYPE yylval;

void print_token(token_type_t type, const std::string& value);
void print_token_int(token_type_t type, long value);
void print_token_float(token_type_t type, double value);
const char* token_type_name(token_type_t type);

void set_token_int(long value);
void set_token_float(double value);
void set_token_string(const std::string& value);
void set_token_identifier(const std::string& value);
%}

%option noyywrap
%option nounput
%option noinput
%option never-interactive
%option yylineno

DIGIT    [0-9]
LETTER   [a-zA-Z_]
ID       {LETTER}({LETTER}|{DIGIT}|_)*
WS       [ \t\r]+
HEX      [0-9a-fA-F]
OCT      [0-7]

%x COMMENT
%x STRING
%x CHAR
%x INTERFACE_IMPL
%x CLASS_LIST
%x INTERFACE_IMPL_AFTER_NAME
%x INTERFACE_IMPL_PARENT
%x CLASS_LIST_AFTER_NAME

%%

%{
    static std::string string_buf;
%}

{WS}     {}

\n       {}

\/\/.*   { 
    print_token(TOK_COMMENT, yytext);
}

\/\*     { 
    BEGIN(COMMENT);
}

<COMMENT>[^*\n]+ {}

<COMMENT>\n {}

<COMMENT>\*+[^*/]* {}

<COMMENT>\*+\/  {
    BEGIN(INITIAL);
    print_token(TOK_COMMENT, "/*...*/");
}

\"       {
    BEGIN(STRING);
    string_buf.clear();
}

@        { 
    print_token(TOK_OPERATOR, yytext);
    return ATSIGN;
}

<STRING>[^"\\\n]+ {
    string_buf += yytext;
}

<STRING>\\[\\"'nrt] {
    switch(yytext[1]) {
        case '\\': string_buf += '\\'; break;
        case '"':  string_buf += '"';  break;
        case 'n':  string_buf += '\n'; break;
        case 'r':  string_buf += '\r'; break;
        case 't':  string_buf += '\t'; break;
        case '\'': string_buf += '\''; break;
    }
}

<STRING>\\x{HEX}{1,2} {
    char hex_val = (char)std::strtol(yytext + 2, NULL, 16);
    string_buf += hex_val;
}

<STRING>\\[0-7]{1,3} {
    char oct_val = (char)std::strtol(yytext + 1, NULL, 8);
    string_buf += oct_val;
}

<STRING>\n {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed string");
    return TOK_ERROR;
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed string");
    return TOK_ERROR;
}

<STRING>\" {
    BEGIN(INITIAL);
    print_token(TOK_STRING, string_buf);
    set_token_string(string_buf);
    return STRING_LIT;
}

[1-9]{DIGIT}*            { 
    long value = std::strtol(yytext, NULL, 10);
    print_token_int(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

0{OCT}*             { 
    long value = std::strtol(yytext, NULL, 8);
    print_token_int(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

0[xX]{HEX}+         { 
    long value = std::strtol(yytext, NULL, 16);
    print_token_int(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

{DIGIT}+\.{DIGIT}*([eE][-+]?{DIGIT}+)?   { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

\.{DIGIT}+([eE][-+]?{DIGIT}+)?           { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

{DIGIT}+[eE][-+]?{DIGIT}+                { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

\'       {
    BEGIN(CHAR);
    string_buf.clear();
}

<CHAR>[^'\\\n] {
    string_buf += yytext[0];
}

<CHAR>\\[\\"'nrtba] {
    switch(yytext[1]) {
        case '\\': string_buf += '\\'; break;
        case '"':  string_buf += '"';  break;
        case 'n':  string_buf += '\n'; break;
        case 'r':  string_buf += '\r'; break;
        case 't':  string_buf += '\t'; break;
        case '\'': string_buf += '\''; break;
        case 'b':  string_buf += '\b'; break;
        case 'a':  string_buf += '\a'; break;
    }
}

<CHAR>\\x{HEX}{1,2} {
    char hex_val = (char)std::strtol(yytext + 2, NULL, 16);
    string_buf += hex_val;
}

<CHAR>\\[0-7]{1,3} {
    char oct_val = (char)std::strtol(yytext + 1, NULL, 8);
    string_buf += oct_val;
}

<CHAR>\n {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed char constant");
    return TOK_ERROR;
}

<CHAR><<EOF>> {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed char constant");
    return TOK_ERROR;
}

<CHAR>\' {
    BEGIN(INITIAL);
    if (string_buf.length() == 1) {
        print_token(TOK_CHAR, string_buf);
        yylval.char_lit = string_buf[0];
        return CHAR_LIT;
    } else {
        print_token(TOK_ERROR, "Invalid char constant");
        return TOK_ERROR;
    }
}

"@interface"        { 
    print_token(TOK_KEYWORD, yytext); 
    BEGIN(INTERFACE_IMPL);
    return INTERFACE; 
}

"@implementation"   { 
    print_token(TOK_KEYWORD, yytext); 
    BEGIN(INTERFACE_IMPL);
    return IMPLEMENTATION; 
}

"@end"              { 
    print_token(TOK_KEYWORD, yytext); 
    BEGIN(INITIAL);
    return END; 
}

"@property"         { 
    print_token(TOK_KEYWORD, yytext); 
    BEGIN(INITIAL);
    return PROPERTY; 
}

"@class"            { 
    print_token(TOK_KEYWORD, yytext); 
    BEGIN(CLASS_LIST);
    return CLASS; 
}

<INTERFACE_IMPL>{WS} {}

<INTERFACE_IMPL>{ID} {
    print_token(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    BEGIN(INTERFACE_IMPL_AFTER_NAME);
    return CLASS_NAME;
}

<INTERFACE_IMPL>. {
    yyless(0);
    BEGIN(INITIAL);
}

<INTERFACE_IMPL_AFTER_NAME>{WS} {}

<INTERFACE_IMPL_AFTER_NAME>":" {
    print_token(TOK_DELIMITER, yytext);
    BEGIN(INTERFACE_IMPL_PARENT);
    return ':';
}

<INTERFACE_IMPL_AFTER_NAME>. {
    yyless(0);
    BEGIN(INITIAL);
}

<INTERFACE_IMPL_PARENT>{WS} {}

<INTERFACE_IMPL_PARENT>{ID} {
    print_token(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    BEGIN(INITIAL);
    return CLASS_NAME;
}

<INTERFACE_IMPL_PARENT>. {
    yyless(0);
    BEGIN(INITIAL);
}

<CLASS_LIST>{WS} {}

<CLASS_LIST>{ID} {
    print_token(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    BEGIN(CLASS_LIST_AFTER_NAME);
    return CLASS_NAME;
}

<CLASS_LIST>. {
    yyless(0);
    BEGIN(INITIAL);
}

<CLASS_LIST_AFTER_NAME>{WS} {}

<CLASS_LIST_AFTER_NAME>"," {
    print_token(TOK_DELIMITER, yytext);
    BEGIN(CLASS_LIST);
    return ',';
}

<CLASS_LIST_AFTER_NAME>";" {
    print_token(TOK_DELIMITER, yytext);
    BEGIN(INITIAL);
    return ';';
}

<CLASS_LIST_AFTER_NAME>. {
    yyless(0);
    BEGIN(INITIAL);
}

"@public"           { print_token(TOK_KEYWORD, yytext); return PUBLIC; }
"@private"          { print_token(TOK_KEYWORD, yytext); return PRIVATE; }
"@protected"        { print_token(TOK_KEYWORD, yytext); return PROTECTED; }

"YES"               { print_token(TOK_KEYWORD, yytext); yylval.bool_lit = true;; return BOOL_LIT; }
"NO"                { print_token(TOK_KEYWORD, yytext); yylval.bool_lit = false; return BOOL_LIT; }
"self"              { print_token(TOK_KEYWORD, yytext); return SELF; }
"super"             { print_token(TOK_KEYWORD, yytext); return SUPER; }
"nil"               { print_token(TOK_KEYWORD, yytext); return NIL; }
"readonly"          { print_token(TOK_KEYWORD, yytext); return READONLY; }
"readwrite"         { print_token(TOK_KEYWORD, yytext); return READWRITE; }

"BOOL"              { print_token(TOK_KEYWORD, yytext); return BOOL; }
"int"               { print_token(TOK_KEYWORD, yytext); return INT; }
"float"             { print_token(TOK_KEYWORD, yytext); return FLOAT; }
"char"              { print_token(TOK_KEYWORD, yytext); return CHAR; }
"id"                { print_token(TOK_KEYWORD, yytext); return TYPE_ID; }
"void"              { print_token(TOK_KEYWORD, yytext); return VOID; }
"if"                { print_token(TOK_KEYWORD, yytext); return IF; }
"else"              { print_token(TOK_KEYWORD, yytext); return ELSE; }
"while"             { print_token(TOK_KEYWORD, yytext); return WHILE; }
"do"                { print_token(TOK_KEYWORD, yytext); return DO; }
"for"               { print_token(TOK_KEYWORD, yytext); return FOR; }
"in"                { print_token(TOK_KEYWORD, yytext); return IN; }
"return"            { print_token(TOK_KEYWORD, yytext); return RETURN; }

{ID}                { 
    print_token(TOK_IDENTIFIER, yytext);
    set_token_identifier(yytext);
    return ID;
}

"+"                 { print_token(TOK_OPERATOR, yytext); return '+'; }
"-"                 { print_token(TOK_OPERATOR, yytext); return '-'; }
"*"                 { print_token(TOK_OPERATOR, yytext); return '*'; }
"/"                 { print_token(TOK_OPERATOR, yytext); return '/'; }
"++"                { print_token(TOK_OPERATOR, yytext); return INC; }
"--"                { print_token(TOK_OPERATOR, yytext); return DEC; }
"="                 { print_token(TOK_OPERATOR, yytext); return '='; }
"=="                { print_token(TOK_OPERATOR, yytext); return EQUAL; }
"!="                { print_token(TOK_OPERATOR, yytext); return NEQUAL; }
"<"                 { print_token(TOK_OPERATOR, yytext); return '<'; }
">"                 { print_token(TOK_OPERATOR, yytext); return '>'; }
"<="                { print_token(TOK_OPERATOR, yytext); return LESS_EQUAL; }
">="                { print_token(TOK_OPERATOR, yytext); return GREATER_EQUAL; }
"!"                 { print_token(TOK_OPERATOR, yytext); return '!'; }
"&&"                { print_token(TOK_OPERATOR, yytext); return AND; }
"||"                { print_token(TOK_OPERATOR, yytext); return OR; }
"->"                { print_token(TOK_OPERATOR, yytext); return ARROW; }
"."                 { print_token(TOK_OPERATOR, yytext); return '.'; }

"("                 { print_token(TOK_DELIMITER, yytext); return '('; }
")"                 { print_token(TOK_DELIMITER, yytext); return ')'; }
"{"                 { print_token(TOK_DELIMITER, yytext); return '{'; }
"}"                 { print_token(TOK_DELIMITER, yytext); return '}'; }
"["                 { print_token(TOK_DELIMITER, yytext); return '['; }
"]"                 { print_token(TOK_DELIMITER, yytext); return ']'; }
","                 { print_token(TOK_DELIMITER, yytext); return ','; }
";"                 { print_token(TOK_DELIMITER, yytext); return ';'; }
":"                 { print_token(TOK_DELIMITER, yytext); return ':'; }

<*>.                   { 
    print_token(TOK_UNKNOWN, yytext);
    return TOK_UNKNOWN;
}

%%

void print_token(token_type_t type, const std::string& value) {
    std::cout << std::left << std::setw(20) << token_type_name(type) << " '" << value << "'" << std::endl;
}

void print_token_int(token_type_t type, long value) {
    std::cout << std::left << std::setw(20) << token_type_name(type) << " " << value << std::endl;
}

void print_token_float(token_type_t type, double value) {
    std::cout << std::left << std::setw(20) << token_type_name(type) << " " << value << std::endl;
}

const char* token_type_name(token_type_t type) {
    switch (type) {
        case TOK_KEYWORD: return "KEYWORD";
        case TOK_IDENTIFIER: return "IDENTIFIER";
        case TOK_CLASS_NAME: return "CLASS_NAME";
        case TOK_OPERATOR: return "OPERATOR";
        case TOK_DELIMITER: return "DELIMITER";
        case TOK_INTEGER: return "INTEGER";
        case TOK_FLOAT: return "FLOAT";
        case TOK_STRING: return "STRING";
        case TOK_CHAR: return "CHAR";
        case TOK_COMMENT: return "COMMENT";
        case TOK_OBJC_INTERFACE: return "OBJC_INTERFACE";
        case TOK_OBJC_IMPLEMENTATION: return "OBJC_IMPLEMENTATION";
        case TOK_OBJC_END: return "OBJC_END";
        case TOK_OBJC_PROPERTY: return "OBJC_PROPERTY";
        case TOK_UNKNOWN: return "UNKNOWN";
        case TOK_ERROR: return "ERROR";
        default: return "UNKNOWN";
    }
}

void set_token_int(long value) {
    yylval.int_lit = value;
}

void set_token_float(double value) {
    yylval.float_lit = (float)value;
}

void set_token_string(const std::string& value) {
    yylval.str_lit = new std::string(value);
}

void set_token_identifier(const std::string& value) {
    yylval.identifier = new std::string(value);
}