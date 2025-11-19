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

%nonassoc NO_ELSE
%nonassoc ELSE

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
%token INTERFACE
%token IMPLEMENTATION
%token END
%token PROPERTY
%token PLUS
%token MINUS
%token ATSIGN

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
%token  <c_str_lit>     C_STRING_LIT
%token  <nsstring_lit>  NSSTRING_LIT
%token  <objc_object>   OBJECT_LIT

%token OBJECTATINDEX
%token COUNT

%right	'='
%left	OR
%left	AND
%left 	EQUAL NEQUAL '<' LESS_EQUAL '>' GREATER_EQUAL
%left	'+' '-'
%left	'*' '/'
%right	INC DEC '!' UMINUS
%left   '.' '['
%left   ARRAY_INDEX

%start program

%%

program     :   stmt_list
            ;

stmt_list   :   stmt_list stmt
            |
            ;

stmt        :   decl
            |   simple_stmt ';'
            |   compound_stmt
            |   if_stmt
            |   switch_stmt
            |   for_stmt
            |   while_stmt
            |   do_while_stmt
            |   interface_decl
            |   implementation_decl
            |   property_decl
            |   method_decl
            |   method_impl
            ;

simple_stmt :   return_stmt
            |   BREAK
            |   CONTINUE
            |   expr ';'
            ;

assignment_expr :   lvalue '=' expr
                |   lvalue '=' initializer
                ;

lvalue      :   ID
            |   array_access
            |   member_access
            ;

member_access : expr '.' ID
              ;

expr_inc_dec:   lvalue INC
            |   lvalue DEC
            ;

return_stmt :   RETURN expr_list
            |   RETURN
            ;

compound_stmt
            :   '{' stmt_list '}'
            ;

if_stmt     :   IF '(' expr ')' stmt    %prec NO_ELSE
            |   IF '(' expr ')' stmt ELSE stmt
            ;

switch_stmt :   SWITCH '(' expr ')' '{' case_list '}'
            |   SWITCH '(' expr ')' '{' case_list default_case '}'
            |   SWITCH '(' expr ')' '{' default_case '}'
            |   SWITCH '(' expr ')' '{' default_case case_list '}'
            ;

case_list   :   case_list case_stmt
            |   case_stmt
            ;

case_stmt   :   CASE expr ':' stmt_list
            ;

default_case:   DEFAULT ':' stmt_list
            ;

for_stmt    :   FOR '(' for_init ';' for_condition ';' for_iteration ')' stmt
            ;

for_init    :   assignment_expr
            |   decl
            |   expr
            |
            ;

for_condition
            :   expr
            |
            ;

for_iteration
            :   expr
            |
            ;

while_stmt  :   WHILE '(' expr ')' stmt
            ;

do_while_stmt
            :   DO stmt WHILE '(' expr ')' ';'
            ;

decl        :   var_decl
            |   const_decl
            ;

var_decl    :   type_spec declarator_list ';'
            ;

const_decl  :   CONST type_spec declarator_list ';'
            ;

type_spec   :   type_name
            |   type_name '*'
            |   array_type_spec
            ;

declarator_list
            :   declarator_list ',' declarator
            |   declarator
            ;

declarator  :   ID
            |   ID '=' expr
            |   ID '=' initializer
            |   ID '[' expr ']'
            |   ID '[' expr ']' '=' initializer
            |   ID '[' ']' '=' initializer
            ;

expr_list   :   expr_list ',' expr
            |   expr
            ;

type_name   :   INT
            |   FLOAT
            |   CHAR
            |   BOOL
            |   NSSTRING
            |   NSNUMBER
            |   NSARRAY
            |   NSDICTIONARY
            ;

expr        :   INT_LIT
            |   FLOAT_LIT
            |   C_STRING_LIT
            |   NSSTRING_LIT
            |   CHAR_LIT
            |   BOOL_LIT
            |   OBJECT_LIT
            |   ID
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
            |   '-' expr    %prec UMINUS
            |   array_access
            |   method_call_expr
            |   member_access
            |   '(' expr ')'
            |   assignment_expr
            |   expr_inc_dec
            ;

initializer :   array_literal
            |   nsarray_literal
            |   nsdictionary_literal
            ;

array_type_spec
            :   type_name '[' ']'
            |   type_name '[' expr ']'
            |   array_type_spec '[' ']'
            |   array_type_spec '[' expr ']'
            ;

array_literal
            :   '{' expr_list '}'
            |   '{' '}'
            ;

array_access:   expr '[' expr ']'   %prec ARRAY_INDEX
            ;

nsarray_literal
            :   ATSIGN '[' expr_list ']'
            |   ATSIGN '[' ']'
            ;

nsdictionary_literal
            :   ATSIGN '{' nsdict_pair_list '}'
            |   ATSIGN '{' '}'
            ;

nsdict_pair_list
            :   nsdict_pair_list ',' nsdict_pair
            |   nsdict_pair
            ;

nsdict_pair :   expr ':' expr
            ;

method_call_expr
            :   expr '.' OBJECTATINDEX '(' expr ')'
            |   expr '.' COUNT '(' ')'
            ;

interface_decl
            :   ATSIGN INTERFACE ID '{' interface_body '}' ATSIGN END ';'
            |   ATSIGN INTERFACE ID ':' ID '{' interface_body '}' ATSIGN END ';'
            ;

interface_body
            :   interface_body property_decl
            |   interface_body method_decl
            |
            ;

implementation_decl
            :   ATSIGN IMPLEMENTATION ID '{' implementation_body '}' ATSIGN END ';'
            ;

implementation_body
            :   implementation_body property_decl
            |   implementation_body method_impl
            |   implementation_body var_decl
            |
            ;

property_decl
            :   ATSIGN PROPERTY '(' property_attrs ')' type_spec declarator_list ';'
            |   ATSIGN PROPERTY type_spec declarator_list ';'
            ;

property_attrs
            :   property_attr
            |   property_attrs ',' property_attr
            ;

property_attr
            :   ID
            |   ID '=' ID
            ;

method_decl :   method_type method_selector ';'
            ;

method_impl :   method_type method_selector compound_stmt
            ;

method_type :   PLUS
            |   MINUS
            ;

method_selector
            :   '(' type_name ')' method_name_with_params
            |   method_name_with_params
            ;

method_name_with_params
            :   ID
            |   ID ':' method_params
            ;

method_params
            :   method_param
            |   method_params method_param
            ;

method_param:   ID ':' '(' type_name ')' ID
            ;

%%