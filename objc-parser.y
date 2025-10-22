%{
#include <iostream>
#include <cstdlib>
#include <cstdio>

using namespace std;

extern int yylex(void);

void yyerror(char const* s) {
    cout << s << endl;
}

%}

%union {
    int int_lit;
    char *identifier;
	char char_lit;
    float float_lit;
	bool bool_lit;
    char *c_str_lit;
    char *nsstring_lit;
    char *objc_object;
}

%token IMPORT
%token CONST
%token VAR
%token FUNC
%token RETURN
%token BREAK
%token CONTINUE
%token IF
%token ELSE
%token SWITCH
%token CASE
%token DEFAULT
%token FOR
%token WHILE
%token DO

%token FOUNDATION // библиотека

%token INT
%token FLOAT
%token BOOL
%token CHAR
%token NSSTRING
%token NSNUMBER
%token NSARRAY
%token NSDICTIONARY

%token 	<int_lit>		INT_LIT
%token	<float_lit>		FLOAT_LIT
%token	<bool_lit>		BOOL_LIT
%token	<identifier>	ID
%token	<char_lit>		CHAR_LIT
%token  <c_string>      C_STRING_LIT
%token  <nsstring>      NSSTRING_LIT
%token  <objc_object>   OBJECT_LIT

%right	'='
%left	OR
%left	AND
%left 	EQUAL NEQUAL '<' LESS_EQUAL '>' GREATER_EQUAL
%left	'+' '-'
%left	'*' '/'
%right	INC DEC '!' UMINUS

%start program

%%

program     :   stmt_list
            ;

stmt_list   :   stmt_list stmt
            |   stmt
            ;

stmt        :   decl
            |   simple_stmt
            ;

%%
