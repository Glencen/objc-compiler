%{
#include <iostream>
#include <cstdlib>
#include <cstdio>

using namespace std;

extern int yylex(void);
void yyerror(char const* s);

%}

%union {
    int int_lit;
    char *identifier;
    char char_lit;
    float float_lit;
    bool bool_lit;
    char *str_lit;
}

%nonassoc NO_ELSE
%nonassoc ELSE

%token RETURN
%token IF
%token ELSE
%token FOR
%token WHILE
%token DO
%token INTERFACE
%token IMPLEMENTATION
%token END
%token PROPERTY

%token INT
%token FLOAT
%token BOOL
%token CHAR
%token NSARRAY
%token NSMUTABLEARRAY
%token NSSTRING
%token NSNUMBER
%token VOID

%token 	<int_lit>		INT_LIT
%token	<float_lit>		FLOAT_LIT
%token	<bool_lit>		BOOL_LIT
%token	<identifier>	ID
%token	<char_lit>		CHAR_LIT
%token  <str_lit>       STRING_LIT

%token PUBLIC
%token PROTECTED
%token PRIVATE
%token SUPER
%token SELF
%token CLASS
%token READWRITE
%token READONLY
%token IN
%token ATSIGN

%right	'='
%left	OR
%left	AND
%left 	EQUAL NEQUAL '<' LESS_EQUAL '>' GREATER_EQUAL
%left	'+' '-'
%left	'*' '/'
%right	INC DEC '!' UMINUS
%left   '{' '}' '[' ']'

%start program

%%

program     :   external_decl_list_e
            ;

external_decl_list_e
            :
            |   external_decl_list
            ;

external_decl_list
            :   external_decl
            |   external_decl_list external_decl
            ;

external_decl
            :   class_decl
            |   class_fw_decl_list
            |   func_decl
            |   func_def
            ;

class_fw_decl_list
            :   CLASS class_list ';'
            ;

class_list  :   ID
            |   class_list ',' ID
            ;

class_decl  :   class_interface
            |   class_implementation
            ;

class_interface
            :   INTERFACE ID ':' ID interface_body END
            |   INTERFACE ID interface_body END
            ;

interface_body
            :   instance_vars interface_decl_list
            |   interface_decl_list
            ;

instance_vars
            :   '{' instance_var_decl_list_e '}'
            ;

instance_var_decl_list_e
            :
            |   instance_var_decl_list
            ;

instance_var_decl_list
            :   access_modifier instance_var_decl
            |   access_modifier instance_var_decl instance_var_decl_list
            ;

instance_var_decl
            :   type ID ';'
            |   type ID '=' expr ';'
            |   c_array_decl ';'
            ;

access_modifier
            :   
            |   PUBLIC
            |   PROTECTED
            |   PRIVATE
            ;

interface_decl_list
            :   property
            |   method_decl
            |   interface_decl_list property
            |   interface_decl_list method_decl
            ;

property    :   PROPERTY '(' attribute ')' type ID ';'
            ;

attribute   :   READONLY
            |   READWRITE
            ;

method_decl :   class_method_decl
            |   instance_method_decl
            ;

class_method_decl
            :   '+' method_type method_sel ';'
            |   '+' '(' VOID ')' method_sel ';'
            |   '+' method_sel ';'
            ;

instance_method_decl
            :   '-' method_type method_sel ';'
            |   '-' '(' VOID ')' method_sel ';'
            |   '-' method_sel ';'
            ;

method_type :   '(' type ')'
            ;

method_sel  :   ID
            |   keyword_sel
            ;

keyword_sel :   keyword_decl
            |   keyword_sel keyword_decl
            ;

keyword_decl:   ':' method_type ID
            |   ':' ID
            |   ID ':' method_type ID
            |   ID ':' ID
            ;

type        :   INT
            |   CHAR
            |   FLOAT
            |   BOOL
            |   ID '*'
            |   NSNUMBER '*'
            |   NSSTRING '*'
            |   NSARRAY '*'
            |   NSMUTABLEARRAY '*'
            ;

class_implementation
            :   IMPLEMENTATION ID implementation_body END
            |   IMPLEMENTATION ID ':' ID implementation_body END
            ;

implementation_body
            :   instance_vars implementation_def_list
            |   implementation_def_list
            ;

implementation_def_list
            :   property
            |   method_def
            |   implementation_def_list property
            |   implementation_def_list method_def
            ;

method_def  :   class_method_def
            |   instance_method_def
            ;

class_method_def
            :   '+' method_type method_sel decl_list_e compound_stmt
            |   '+' '(' VOID ')' method_sel decl_list_e compound_stmt
            |   '+' method_sel decl_list_e compound_stmt
            ;

instance_method_def
            :   '-' method_type method_sel decl_list_e compound_stmt
            |   '-' '(' VOID ')' method_sel decl_list_e compound_stmt
            |   '-' method_sel decl_list_e compound_stmt
            ;

decl_list_e :
            |   decl_list
            ;

decl_list   :   decl
            |   decl_list decl
            ;

decl        :   type init_decl_list_e ';'
            ;

init_decl_list_e
            :
            |   init_decl_list
            ;

init_decl_list
            :   init_decl
            |   init_decl_list ',' init_decl
            ;

init_decl   :   ID
            |   ID '=' expr
            |   c_array_decl
            ;

c_array_decl:   ID '[' expr ']'
            |   ID '[' expr ']' '=' '{' expr_list_e '}'
            |   ID '[' ']' '=' '{' expr_list_e '}'
            ;

array_access:   expr '[' expr ']'
            ;

expr_list_e :
            |   expr_list
            ;

expr_list   :   expr
            |   expr_list ',' expr
            ;

compound_stmt
            :   '{' stmt_list_e '}'
            ;

stmt_list_e :
            |   stmt_list
            ;

stmt_list   :   stmt
            |   stmt_list stmt
            ;

stmt        :   ';'
            |   expr ';'
            |   RETURN expr_e ';'
            |   if_stmt
            |   for_stmt
            |   while_stmt
            |   do_while_stmt
            |   compound_stmt
            |   decl
            ;

expr_e      :
            |   expr
            ;

if_stmt     :   IF '(' expr ')' stmt    %prec NO_ELSE
            |   IF '(' expr ')' stmt ELSE stmt
            ;

for_stmt    :   FOR '(' expr_e ';' expr_e ';' expr_e ')' stmt
            |   FOR '(' ID IN expr ')' stmt
            |   FOR '(' type ID IN expr ')' stmt
            ;

while_stmt  :   WHILE '(' expr ')' stmt
            ;

do_while_stmt
            :   DO stmt WHILE '(' expr ')' ';'
            ;

expr        :   ID
            |   literal
            |   objc_literal
            |   '(' expr ')'
            |   msg_expr
            |   SELF
            |   '-' expr    %prec UMINUS
            |   '!' expr
            |   expr INC
            |   expr DEC
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
            |   expr '=' expr
            |   array_access
            |   func_call
            |   ATSIGN '(' expr ')'
            |   '{' expr_list_e '}'
            ;

msg_expr    :   '[' receiver msg_sel ']'
            ;

receiver    :   SELF
            |   SUPER
            |   ID
            |   msg_expr
            ;

msg_sel     :   ID
            |   keyword_arg_list
            ;

keyword_arg_list
            :   keyword_arg
            |   keyword_arg_list keyword_arg
            ;

keyword_arg :   ID ':' expr
            ;

literal     :   STRING_LIT
            |   CHAR_LIT
            |   BOOL_LIT
            |   num_literal
            ;

objc_literal:   ATSIGN literal
            |   ATSIGN '[' expr_list_e ']'
            ;

num_literal :   INT_LIT
            |   FLOAT_LIT
            ;

func_decl   :   type ID '(' param_list_e ')' ';'
            ;

func_def    :   type ID '(' param_list_e ')' compound_stmt
            ;

param_list_e:
            |   param_list
            ;

param_list  :   param_decl
            |   param_list ',' param_decl
            ;

param_decl  :   type ID
            ;

func_call   :   ID '(' expr_list_e ')'
            ;

%%