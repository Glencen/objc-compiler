%{
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include "objc-parser.hpp"
#include "utils.h"
#include "output_utils.h"

extern YYSTYPE yylval;

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
%x CHARACTER
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

\n       {
    TokenOutput::getInstance().setCurrentLine(yylineno + 1);
}

\/\/.*   {
    TokenOutput::getInstance().addToken(TOK_COMMENT, yytext);
}

\/\*     {
    BEGIN(COMMENT);
}

<COMMENT>[^*\n]+ {}

<COMMENT>\n { TokenOutput::getInstance().setCurrentLine(yylineno + 1); }

<COMMENT>\*+[^*/]* {}

<COMMENT>\*+\/  {
    BEGIN(INITIAL);
    TokenOutput::getInstance().addToken(TOK_COMMENT, "/*...*/");
}

\"       {
    BEGIN(STRING);
    string_buf.clear();
}

@        { 
    TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext);
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
    TokenOutput::getInstance().addToken(TOK_ERROR, "Unclosed string");
    TokenOutput::getInstance().flushTokens();
    return TOK_ERROR;
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    TokenOutput::getInstance().addToken(TOK_ERROR, "Unclosed string");
    TokenOutput::getInstance().flushTokens();
    return TOK_ERROR;
}

<STRING>\" {
    BEGIN(INITIAL);
    TokenOutput::getInstance().addToken(TOK_STRING, string_buf);
    set_token_string(string_buf);
    return STRING_LIT;
}

[1-9]{DIGIT}*            { 
    long value = std::strtol(yytext, NULL, 10);
    TokenOutput::getInstance().addTokenInt(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

0{OCT}*             { 
    long value = std::strtol(yytext, NULL, 8);
    TokenOutput::getInstance().addTokenInt(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

0[xX]{HEX}+         { 
    long value = std::strtol(yytext, NULL, 16);
    TokenOutput::getInstance().addTokenInt(TOK_INTEGER, value);
    set_token_int(value);
    return INT_LIT;
}

{DIGIT}+\.{DIGIT}*([eE][-+]?{DIGIT}+)?   { 
    double value = std::strtod(yytext, NULL);
    TokenOutput::getInstance().addTokenFloat(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

\.{DIGIT}+([eE][-+]?{DIGIT}+)?           { 
    double value = std::strtod(yytext, NULL);
    TokenOutput::getInstance().addTokenFloat(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

{DIGIT}+[eE][-+]?{DIGIT}+                { 
    double value = std::strtod(yytext, NULL);
    TokenOutput::getInstance().addTokenFloat(TOK_FLOAT, value);
    set_token_float(value);
    return FLOAT_LIT;
}

\'       {
    BEGIN(CHARACTER);
    string_buf.clear();
}

<CHARACTER>[^'\\\n] {
    string_buf += yytext[0];
}

<CHARACTER>\\[\\"'nrtba] {
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

<CHARACTER>\\x{HEX}{1,2} {
    char hex_val = (char)std::strtol(yytext + 2, NULL, 16);
    string_buf += hex_val;
}

<CHARACTER>\\[0-7]{1,3} {
    char oct_val = (char)std::strtol(yytext + 1, NULL, 8);
    string_buf += oct_val;
}

<CHARACTER>\n {
    BEGIN(INITIAL);
    TokenOutput::getInstance().addToken(TOK_ERROR, "Unclosed char constant");
    TokenOutput::getInstance().flushTokens();
    return TOK_ERROR;
}

<CHARACTER><<EOF>> {
    BEGIN(INITIAL);
    TokenOutput::getInstance().addToken(TOK_ERROR, "Unclosed char constant");
    TokenOutput::getInstance().flushTokens();
    return TOK_ERROR;
}

<CHARACTER>\' {
    BEGIN(INITIAL);
    if (string_buf.length() == 1) {
        TokenOutput::getInstance().addToken(TOK_CHAR, string_buf);
        yylval.char_lit = string_buf[0];
        return CHAR_LIT;
    } else {
        TokenOutput::getInstance().addToken(TOK_ERROR, "Invalid char constant");
        TokenOutput::getInstance().flushTokens();
        return TOK_ERROR;
    }
}

"@interface"        { 
    TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); 
    BEGIN(INTERFACE_IMPL);
    return INTERFACE; 
}

"@implementation"   { 
    TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); 
    BEGIN(INTERFACE_IMPL);
    return IMPLEMENTATION; 
}

"@end"              { 
    TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); 
    BEGIN(INITIAL);
    return END; 
}

"@property"         { 
    TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); 
    BEGIN(INITIAL);
    return PROPERTY; 
}

"@class"            { 
    TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); 
    BEGIN(CLASS_LIST);
    return CLASS; 
}

<INTERFACE_IMPL>{WS} {}

<INTERFACE_IMPL>{ID} {
    TokenOutput::getInstance().addToken(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    add_class_name(yytext);
    BEGIN(INTERFACE_IMPL_AFTER_NAME);
    return CLASS_NAME;
}

<INTERFACE_IMPL>. {
    yyless(0);
    BEGIN(INITIAL);
}

<INTERFACE_IMPL_AFTER_NAME>{WS} {}

<INTERFACE_IMPL_AFTER_NAME>":" {
    TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext);
    BEGIN(INTERFACE_IMPL_PARENT);
    return ':';
}

<INTERFACE_IMPL_AFTER_NAME>. {
    yyless(0);
    BEGIN(INITIAL);
}

<INTERFACE_IMPL_PARENT>{WS} {}

<INTERFACE_IMPL_PARENT>{ID} {
    TokenOutput::getInstance().addToken(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    add_class_name(yytext);
    BEGIN(INITIAL);
    return CLASS_NAME;
}

<INTERFACE_IMPL_PARENT>. {
    yyless(0);
    BEGIN(INITIAL);
}

<CLASS_LIST>{WS} {}

<CLASS_LIST>{ID} {
    TokenOutput::getInstance().addToken(TOK_CLASS_NAME, yytext);
    set_token_identifier(yytext);
    add_class_name(yytext);
    BEGIN(CLASS_LIST_AFTER_NAME);
    return CLASS_NAME;
}

<CLASS_LIST>. {
    yyless(0);
    BEGIN(INITIAL);
}

<CLASS_LIST_AFTER_NAME>{WS} {}

<CLASS_LIST_AFTER_NAME>"," {
    TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext);
    BEGIN(CLASS_LIST);
    return ',';
}

<CLASS_LIST_AFTER_NAME>";" {
    TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext);
    BEGIN(INITIAL);
    return ';';
}

<CLASS_LIST_AFTER_NAME>. {
    yyless(0);
    BEGIN(INITIAL);
}

"@public"           { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return PUBLIC; }
"@private"          { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return PRIVATE; }
"@protected"        { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return PROTECTED; }

"true"               { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); yylval.bool_lit = true; return BOOL_LIT; }
"false"                { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); yylval.bool_lit = false; return BOOL_LIT; }
"self"              { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return SELF; }
"super"             { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return SUPER; }
"nil"               { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return NIL; }
"readonly"          { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return READONLY; }
"readwrite"         { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return READWRITE; }

"bool"              { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return BOOL; }
"int"               { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return INT; }
"float"             { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return FLOAT; }
"char"              { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return CHAR; }
"id"                { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return TYPE_ID; }
"void"              { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return VOID; }
"if"                { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return IF; }
"else"              { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return ELSE; }
"while"             { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return WHILE; }
"do"                { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return DO; }
"for"               { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return FOR; }
"in"                { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return IN; }
"return"            { TokenOutput::getInstance().addToken(TOK_KEYWORD, yytext); return RETURN; }

{ID}                { 
    if (is_class_name(yytext)) {
        TokenOutput::getInstance().addToken(TOK_CLASS_NAME, yytext);
        set_token_identifier(yytext);
        return CLASS_NAME;
    } else {
        TokenOutput::getInstance().addToken(TOK_IDENTIFIER, yytext);
        set_token_identifier(yytext);
        return ID;
    }
}

"+"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '+'; }
"-"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '-'; }
"*"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '*'; }
"/"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '/'; }
"++"                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return INC; }
"--"                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return DEC; }
"="                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '='; }
"=="                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return EQUAL; }
"!="                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return NEQUAL; }
"<"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '<'; }
">"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '>'; }
"<="                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return LESS_EQUAL; }
">="                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return GREATER_EQUAL; }
"!"                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '!'; }
"&&"                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return AND; }
"||"                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return OR; }
"->"                { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return ARROW; }
"."                 { TokenOutput::getInstance().addToken(TOK_OPERATOR, yytext); return '.'; }

"("                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return '('; }
")"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return ')'; }
"{"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return '{'; }
"}"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return '}'; }
"["                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return '['; }
"]"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return ']'; }
","                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return ','; }
";"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return ';'; }
":"                 { TokenOutput::getInstance().addToken(TOK_DELIMITER, yytext); return ':'; }

<*>.                   { 
    TokenOutput::getInstance().addToken(TOK_UNKNOWN, yytext);
    return TOK_UNKNOWN;
}

%%

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