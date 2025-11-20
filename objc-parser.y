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

%token PUBLIC
%token PROTECTED
%token PRIVATE

%token INT
%token FLOAT
%token BOOL
%token CHAR
%token NSSTRING
%token NSNUMBER
%token NSINTEGER
%token NSARRAY
%token NSDICTIONARY
%token VOID

%token PRINTF
%token NSLOG

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
%token SUPER
%token SELF

%right	'='
%left	OR
%left	AND
%left 	EQUAL NEQUAL '<' LESS_EQUAL '>' GREATER_EQUAL
%left	'+' '-'
%left	'*' '/'
%right	INC DEC '!' UMINUS
%left   '.' '['
%left   ARRAY_INDEX
%left   FUNCTION_CALL

%start program

%%

program     :   external_decl_list
            ;

external_decl_list
            :   external_decl_list external_decl
            |   external_decl
            ;

external_decl
            :   interface_decl
            |   implementation_decl
            |   function_def
            |   function_decl
            |   var_decl
            |   const_decl
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
            |   nslog_stmt
            |   printf_stmt
            ;

simple_stmt :   return_stmt
            |   BREAK
            |   CONTINUE
            |   expr
            ;

assignment_expr
            :   lvalue '=' expr
            |   lvalue '=' initializer
            ;

lvalue      :   ID
            |   array_access
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

switch_stmt :   SWITCH '(' expr ')' '{' switch_body '}'
            ;

switch_body :   case_list
            |   case_list default_case
            |   default_case
            |   default_case case_list
            |
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

for_init    :   for_assignment
            |   for_decl
            |
            ;

for_assignment
            :   lvalue '=' expr
            |   expr_inc_dec
            ;

for_decl    :   type_spec for_declarator_list
            |   CONST type_spec for_declarator_list
            ;

for_declarator_list
            :   for_declarator
            |   for_declarator_list ',' for_declarator
            ;

for_declarator
            :   ID
            |   ID '=' expr
            |   ID '=' initializer
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

declarator  :   for_declarator
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
            |   NSINTEGER
            |   NSARRAY
            |   NSDICTIONARY
            |   VOID
            ;

expr        :   INT_LIT
            |   FLOAT_LIT
            |   C_STRING_LIT
            |   NSSTRING_LIT
            |   CHAR_LIT
            |   BOOL_LIT
            |   OBJECT_LIT
            |   ID
            |   function_call_expr
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
            :   '[' receiver message_selector ']'
            ;

receiver    :   expr
            |   SUPER
            |   SELF
            ;

message_selector
            :   ID
            |   keyword_args
            ;

keyword_args
            :   ID ':' expr
            |   keyword_args ID ':' expr
            ;

function_decl
            :   type_spec ID '(' parameter_list_opt ')' ';'
            ;

function_def
            :   type_spec ID '(' parameter_list_opt ')' compound_stmt
            ;

parameter_list_opt
            :   parameter_list
            |
            ;

parameter_list
            :   parameter
            |   parameter_list ',' parameter
            ;

parameter   :   type_spec ID
            |   type_spec ID '[' ']'
            |   type_spec ID '[' expr ']'
            ;

function_call_expr
            :   ID '(' argument_list_opt ')'   %prec FUNCTION_CALL
            ;

argument_list_opt
            :   argument_list
            |
            ;

argument_list
            :   expr
            |   argument_list ',' expr
            ;

interface_decl
            :   ATSIGN INTERFACE ID '{' interface_body '}' ATSIGN END ';'
            |   ATSIGN INTERFACE ID ':' ID '{' interface_body '}' ATSIGN END ';'
            ;

interface_body
            :   interface_body property_decl
            |   interface_body method_decl
            |   interface_body function_decl
            |
            ;

implementation_decl
            :   ATSIGN IMPLEMENTATION ID '{' implementation_body '}' ATSIGN END ';'
            ;

implementation_body
            :   implementation_body property_decl
            |   implementation_body method_impl
            |   implementation_body var_decl
            |   implementation_body function_def
            |   implementation_body function_decl
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

nslog_stmt  :   NSLOG '(' nslog_arguments ')' ';'
            ;

nslog_arguments
            :   nslog_arguments ',' nslog_argument
            |   nslog_argument
            |
            ;

nslog_argument
            :   expr
            ;

printf_stmt :   PRINTF '(' printf_arguments ')' ';'
            ;

printf_arguments
            :   printf_format printf_args_opt
            ;

printf_format
            :   C_STRING_LIT
            |   NSSTRING_LIT
            ;

printf_args_opt
            :   ',' printf_arg_list
            |
            ;

printf_arg_list
            :   printf_arg_list ',' printf_arg
            |   printf_arg
            ;

printf_arg  :   expr
            ;

%%