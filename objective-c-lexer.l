%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STRING_BUF_SIZE 1024

typedef enum {
    TOK_KEYWORD,
    TOK_IDENTIFIER, 
    TOK_CLASS_ID,
    TOK_OPERATOR,
    TOK_DELIMITER,
    TOK_INTEGER,
    TOK_FLOAT,
    TOK_STRING,
    TOK_NSSTRING,
    TOK_CHAR,
    TOK_PREPROCESSOR,
    TOK_COMMENT,
    TOK_OBJC_INTERFACE,
    TOK_OBJC_IMPLEMENTATION,
    TOK_OBJC_END,
    TOK_OBJC_PROPERTY,
    TOK_UNKNOWN,
    TOK_ERROR
} token_type_t;

void print_token(token_type_t type, const char* value);
const char* token_type_name(token_type_t type);
%}

%option noyywrap
%option nounput
%option noinput

DIGIT    [0-9]
LETTER   [a-zA-Z]
ID       {LETTER}({LETTER}|{DIGIT}|_)*
WS       [ \t\r]+
HEX      [0-9a-fA-F]
OCT      [0-7]

%x COMMENT
%x STRING
%x NSSTRING
%x OBJC_ARRAY
%x OBJC_DICT

%%

%{
    static char string_buf[STRING_BUF_SIZE];
    static char *string_buf_ptr;
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
    string_buf_ptr = string_buf;
}

@\"      {
    BEGIN(NSSTRING);
    string_buf_ptr = string_buf;
    *string_buf_ptr++ = '@';
}

<STRING,NSSTRING>[^"\\\n]+ {
    char *p = yytext;
    while (*p) {
        if (string_buf_ptr < string_buf + STRING_BUF_SIZE - 1) {
            *string_buf_ptr++ = *p++;
        }
    }
}

<STRING,NSSTRING>\\[\\"'nrt] {
    switch(yytext[1]) {
        case '\\': *string_buf_ptr++ = '\\'; break;
        case '"':  *string_buf_ptr++ = '"';  break;
        case 'n':  *string_buf_ptr++ = '\n'; break;
        case 'r':  *string_buf_ptr++ = '\r'; break;
        case 't':  *string_buf_ptr++ = '\t'; break;
        case '\'': *string_buf_ptr++ = '\''; break;
    }
}

<STRING,NSSTRING>\\x{HEX}{1,2} {
    // Hex escape sequence
    char hex_val = (char)strtol(yytext + 2, NULL, 16);
    if (string_buf_ptr < string_buf + STRING_BUF_SIZE - 1) {
        *string_buf_ptr++ = hex_val;
    }
}

<STRING,NSSTRING>\\[0-7]{1,3} {
    // Octal escape sequence
    char oct_val = (char)strtol(yytext + 1, NULL, 8);
    if (string_buf_ptr < string_buf + STRING_BUF_SIZE - 1) {
        *string_buf_ptr++ = oct_val;
    }
}

<STRING,NSSTRING>\n {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed string");
}

<STRING>\" {
    *string_buf_ptr = '\0';
    BEGIN(INITIAL);
    print_token(TOK_STRING, string_buf);
}

<NSSTRING>\" {
    *string_buf_ptr = '\0';
    BEGIN(INITIAL);
    print_token(TOK_NSSTRING, string_buf);
}

[1-9]{DIGIT}*            { print_token(TOK_INTEGER, yytext); }
0{OCT}*             { print_token(TOK_INTEGER, yytext); }
0[xX]{HEX}+         { print_token(TOK_INTEGER, yytext); }

{DIGIT}+\.{DIGIT}*([eE][-+]?{DIGIT}+)?   { print_token(TOK_FLOAT, yytext); }
\.{DIGIT}+([eE][-+]?{DIGIT}+)?           { print_token(TOK_FLOAT, yytext); }
{DIGIT}+[eE][-+]?{DIGIT}+                { print_token(TOK_FLOAT, yytext); }

'([^'\\]|\\[\\"'nrt]|\\x[0-9a-fA-F]{1,2}|\\[0-7]{1,3})' { 
    print_token(TOK_CHAR, yytext); 
}

"@interface"        { print_token(TOK_OBJC_INTERFACE, yytext); }
"@implementation"   { print_token(TOK_OBJC_IMPLEMENTATION, yytext); }
"@end"              { print_token(TOK_OBJC_END, yytext); }
"@property"         { print_token(TOK_OBJC_PROPERTY, yytext); }

"@["    { 
    print_token(TOK_OPERATOR, yytext);
    BEGIN(OBJC_ARRAY);
}

"@{"    { 
    print_token(TOK_OPERATOR, yytext);
    BEGIN(OBJC_DICT);
}

<OBJC_ARRAY>@\" {
    string_buf_ptr = string_buf;
    *string_buf_ptr++ = '@';
    BEGIN(NSSTRING);
}

<OBJC_ARRAY>\] {
    print_token(TOK_DELIMITER, yytext);
    BEGIN(INITIAL);
}

<OBJC_DICT>@\" {
    string_buf_ptr = string_buf;
    *string_buf_ptr++ = '@';
    BEGIN(NSSTRING);
}

<OBJC_DICT>\} {
    print_token(TOK_DELIMITER, yytext);
    BEGIN(INITIAL);
}

"YES"               { print_token(TOK_KEYWORD, yytext); }
"NO"                { print_token(TOK_KEYWORD, yytext); }
"nil"               { print_token(TOK_KEYWORD, yytext); }
"Nil"               { print_token(TOK_KEYWORD, yytext); }
"NULL"              { print_token(TOK_KEYWORD, yytext); }
"self"              { print_token(TOK_KEYWORD, yytext); }
"super"             { print_token(TOK_KEYWORD, yytext); }

"id"                { print_token(TOK_KEYWORD, yytext); }
"Class"             { print_token(TOK_KEYWORD, yytext); }
"SEL"               { print_token(TOK_KEYWORD, yytext); }
"IMP"               { print_token(TOK_KEYWORD, yytext); }
"BOOL"              { print_token(TOK_KEYWORD, yytext); }

"int"               { print_token(TOK_KEYWORD, yytext); }
"float"             { print_token(TOK_KEYWORD, yytext); }
"char"              { print_token(TOK_KEYWORD, yytext); }
"if"                { print_token(TOK_KEYWORD, yytext); }
"else"              { print_token(TOK_KEYWORD, yytext); }
"while"             { print_token(TOK_KEYWORD, yytext); }
"do"                { print_token(TOK_KEYWORD, yytext); }
"for"               { print_token(TOK_KEYWORD, yytext); }
"switch"            { print_token(TOK_KEYWORD, yytext); }
"case"              { print_token(TOK_KEYWORD, yytext); }
"default"           { print_token(TOK_KEYWORD, yytext); }
"break"             { print_token(TOK_KEYWORD, yytext); }
"continue"          { print_token(TOK_KEYWORD, yytext); }
"return"            { print_token(TOK_KEYWORD, yytext); }
"const"             { print_token(TOK_KEYWORD, yytext); }

{ID}                { print_token(TOK_IDENTIFIER, yytext); }

"+"                 { print_token(TOK_OPERATOR, yytext); }
"-"                 { print_token(TOK_OPERATOR, yytext); }
"*"                 { print_token(TOK_OPERATOR, yytext); }
"/"                 { print_token(TOK_OPERATOR, yytext); }
"++"                { print_token(TOK_OPERATOR, yytext); }
"--"                { print_token(TOK_OPERATOR, yytext); }
"="                 { print_token(TOK_OPERATOR, yytext); }
"=="                { print_token(TOK_OPERATOR, yytext); }
"!="                { print_token(TOK_OPERATOR, yytext); }
"<"                 { print_token(TOK_OPERATOR, yytext); }
">"                 { print_token(TOK_OPERATOR, yytext); }
"<="                { print_token(TOK_OPERATOR, yytext); }
">="                { print_token(TOK_OPERATOR, yytext); }
"!"                 { print_token(TOK_OPERATOR, yytext); }
"&&"                { print_token(TOK_OPERATOR, yytext); }
"||"                { print_token(TOK_OPERATOR, yytext); }
"+="                { print_token(TOK_OPERATOR, yytext); }
"-="                { print_token(TOK_OPERATOR, yytext); }
"*="                { print_token(TOK_OPERATOR, yytext); }
"/="                { print_token(TOK_OPERATOR, yytext); }

"("                 { print_token(TOK_DELIMITER, yytext); }
")"                 { print_token(TOK_DELIMITER, yytext); }
"{"                 { print_token(TOK_DELIMITER, yytext); }
"}"                 { print_token(TOK_DELIMITER, yytext); }
"["                 { print_token(TOK_DELIMITER, yytext); }
"]"                 { print_token(TOK_DELIMITER, yytext); }
","                 { print_token(TOK_DELIMITER, yytext); }
";"                 { print_token(TOK_DELIMITER, yytext); }
"."                 { print_token(TOK_DELIMITER, yytext); }
":"                 { print_token(TOK_DELIMITER, yytext); }

#.*                 { print_token(TOK_PREPROCESSOR, yytext); }

.                   { 
    print_token(TOK_UNKNOWN, yytext); 
}

%%

void print_token(token_type_t type, const char* value) {
    printf("%-20s '%s'\n", token_type_name(type), value);
}

const char* token_type_name(token_type_t type) {
    switch (type) {
        case TOK_KEYWORD: return "KEYWORD";
        case TOK_IDENTIFIER: return "IDENTIFIER";
        case TOK_OPERATOR: return "OPERATOR";
        case TOK_DELIMITER: return "DELIMITER";
        case TOK_INTEGER: return "INTEGER";
        case TOK_FLOAT: return "FLOAT";
        case TOK_STRING: return "STRING";
        case TOK_NSSTRING: return "NSSTRING";
        case TOK_CHAR: return "CHAR";
        case TOK_PREPROCESSOR: return "PREPROCESSOR";
        case TOK_COMMENT: return "COMMENT";
        case TOK_OBJC_INTERFACE: return "OBJC_INTERFACE";
        case TOK_OBJC_IMPLEMENTATION: return "OBJC_IMPLEMENTATION";
        case TOK_OBJC_END: return "OBJC_END";
        case TOK_OBJC_PROPERTY: return "OBJC_PROPERTY";
        case TOK_UNKNOWN: return "UNKNOWN";
        default: return "UNKNOWN";
    }
}

int main(int argc, char **argv) {
    if (argc > 1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            fprintf(stderr, "Error: Cannot open file %s\n", argv[1]);
            return 1;
        }
        yyin = file;
    } else {
        printf("Enter Objective-C code (Ctrl+D to end):\n");
    }
    
    printf("%-20s %s\n", "Token Type", "Value");
    printf("%-20s %s\n", "--------------------", "-----");
    
    yylex();
    
    if (argc > 1) {
        fclose(yyin);
    }
    
    return 0;
}