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
            |   compound_stmt
            |   if_stmt
            |   switch_stmt
            |   for_stmt
            |   while_stmt
            |   do_while_stmt
            ;

simple_stmt :   expr
            |   expr INC
            |   expr DEC
            |   return_stmt
            |   BREAK ';'
            |   CONTINUE ';'
            |   expr '=' expr
            ;

return_stmt :   RETURN expr_list
            |   RETURN
            ;

compound_stmt
            :   '{' stmt_list '}'
            ;

expr_list   :   expr_list expr
            |   expr
            ;

expr        :   INT_LIT
            |   FLOAT_LIT
            |   C_STRING_LIT
            |   NSSTRING_LIT
            |   CHAR_LIT
            |   BOOL_LIT
            |   OBJECT_LIT
            |   expr '+' expr
            |   expr '-' expr
            |   expr '*' expr
            |   expr '/' expr
            |   expr EQUAL expr
            |   expr NEQUAL expr
            |   expr '>' expr
            |   expr '<' expr
            |   expr LESS_EQUAL expr
            |   expr GREATER_EQUAL expr
            |   expr AND expr
            |   expr OR expr
            |   '!' expr
            |   '-' expr    %prec UMINUS\
            ;



%%
