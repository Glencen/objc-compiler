%{
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <iomanip>

#define STRING_BUF_SIZE 1024

typedef enum {
    TOK_KEYWORD,
    TOK_IDENTIFIER, 
    TOK_OPERATOR,
    TOK_DELIMITER,
    TOK_INTEGER,
    TOK_FLOAT,
    TOK_STRING,
    TOK_CHAR,
    TOK_COMMENT,
    TOK_ATSIGN,
    TOK_OBJC_INTERFACE,
    TOK_OBJC_IMPLEMENTATION,
    TOK_OBJC_END,
    TOK_OBJC_PROPERTY,
    TOK_UNKNOWN,
    TOK_ERROR
} token_type_t;

void print_token(token_type_t type, const std::string& value);
void print_token_int(token_type_t type, long value);
void print_token_float(token_type_t type, double value);
const char* token_type_name(token_type_t type);
%}

%option noyywrap
%option nounput
%option noinput
%option never-interactive

DIGIT    [0-9]
LETTER   [a-zA-Z_]
ID       {LETTER}({LETTER}|{DIGIT}|_)*
WS       [ \t\r]+
HEX      [0-9a-fA-F]
OCT      [0-7]

%x COMMENT
%x STRING
%x CHAR

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

@        { print_token(TOK_ATSIGN, yytext); }

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
    // Hex escape sequence
    char hex_val = (char)std::strtol(yytext + 2, NULL, 16);
    string_buf += hex_val;
}

<STRING>\\[0-7]{1,3} {
    // Octal escape sequence
    char oct_val = (char)std::strtol(yytext + 1, NULL, 8);
    string_buf += oct_val;
}

<STRING>\n {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed string");
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed string");
}

<STRING>\" {
    BEGIN(INITIAL);
    print_token(TOK_STRING, string_buf);
}

[1-9]{DIGIT}*            { 
    long value = std::strtol(yytext, NULL, 10);
    print_token_int(TOK_INTEGER, value); 
}

0{OCT}*             { 
    long value = std::strtol(yytext, NULL, 8);
    print_token_int(TOK_INTEGER, value); 
}

0[xX]{HEX}+         { 
    long value = std::strtol(yytext, NULL, 16);
    print_token_int(TOK_INTEGER, value); 
}

{DIGIT}+\.{DIGIT}*([eE][-+]?{DIGIT}+)?   { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value); 
}

\.{DIGIT}+([eE][-+]?{DIGIT}+)?           { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value); 
}

{DIGIT}+[eE][-+]?{DIGIT}+                { 
    double value = std::strtod(yytext, NULL);
    print_token_float(TOK_FLOAT, value); 
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
    // Hex escape sequence
    char hex_val = (char)std::strtol(yytext + 2, NULL, 16);
    string_buf += hex_val;
}

<CHAR>\\[0-7]{1,3} {
    // Octal escape sequence
    char oct_val = (char)std::strtol(yytext + 1, NULL, 8);
    string_buf += oct_val;
}

<CHAR>\n {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed char constant");
}

<CHAR><<EOF>> {
    BEGIN(INITIAL);
    print_token(TOK_ERROR, "Unclosed char constant");
}

<CHAR>\' {
    BEGIN(INITIAL);
    if (string_buf.length() == 1) {
        print_token(TOK_CHAR, string_buf);
    } else {
        print_token(TOK_ERROR, "Invalid char constant");
    }
}

"@interface"        { print_token(TOK_OBJC_INTERFACE, yytext); }
"@implementation"   { print_token(TOK_OBJC_IMPLEMENTATION, yytext); }
"@end"              { print_token(TOK_OBJC_END, yytext); }
"@property"         { print_token(TOK_OBJC_PROPERTY, yytext); }

"@["    { 
    print_token(TOK_OPERATOR, yytext);
}

"@{"    { 
    print_token(TOK_OPERATOR, yytext);
}

"@public"           { print_token(TOK_KEYWORD, yytext); }
"@private"          { print_token(TOK_KEYWORD, yytext); }
"@protected"        { print_token(TOK_KEYWORD, yytext); }

"YES"               { print_token(TOK_KEYWORD, yytext); }
"NO"                { print_token(TOK_KEYWORD, yytext); }
"self"              { print_token(TOK_KEYWORD, yytext); }
"super"             { print_token(TOK_KEYWORD, yytext); }

"BOOL"              { print_token(TOK_KEYWORD, yytext); }
"int"               { print_token(TOK_KEYWORD, yytext); }
"float"             { print_token(TOK_KEYWORD, yytext); }
"char"              { print_token(TOK_KEYWORD, yytext); }
"NSString"          { print_token(TOK_KEYWORD, yytext); }
"NSNumber"          { print_token(TOK_KEYWORD, yytext); }
"NSInteger"         { print_token(TOK_KEYWORD, yytext); }
"NSArray"           { print_token(TOK_KEYWORD, yytext); }
"NSDictionary"      { print_token(TOK_KEYWORD, yytext); }
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

<*>.                   { 
    print_token(TOK_UNKNOWN, yytext); 
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
        case TOK_OPERATOR: return "OPERATOR";
        case TOK_DELIMITER: return "DELIMITER";
        case TOK_INTEGER: return "INTEGER";
        case TOK_FLOAT: return "FLOAT";
        case TOK_STRING: return "STRING";
        case TOK_CHAR: return "CHAR";
        case TOK_COMMENT: return "COMMENT";
        case TOK_ATSIGN: return "ATSIGN";
        case TOK_OBJC_INTERFACE: return "OBJC_INTERFACE";
        case TOK_OBJC_IMPLEMENTATION: return "OBJC_IMPLEMENTATION";
        case TOK_OBJC_END: return "OBJC_END";
        case TOK_OBJC_PROPERTY: return "OBJC_PROPERTY";
        case TOK_UNKNOWN: return "UNKNOWN";
        case TOK_ERROR: return "ERROR";
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