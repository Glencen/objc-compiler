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
%token TYPE_ID
%token CLASS_NAME

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
%token NIL

%right   '='
%left    OR
%left    AND
%left    EQUAL NEQUAL
%left    '<' '>' LESS_EQUAL GREATER_EQUAL
%left    '+' '-'
%left    '*' '/'
%left    INC DEC
%right   '!' UMINUS
%left    '(' '[' '.' ARROW

%start program

%%

program     :   external_decl_list_e
            ;

external_decl_list_e
            :   /* empty */
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

class_list  :   CLASS_NAME
            |   class_list ',' CLASS_NAME
            ;

class_decl  :   class_interface
            |   class_implementation
            ;

class_interface
            :   INTERFACE CLASS_NAME ':' ID interface_body END
            |   INTERFACE CLASS_NAME interface_body END
            ;

interface_body
            :   instance_vars interface_decl_list
            |   interface_decl_list
            ;

instance_vars
            :   '{' instance_var_decl_list_e '}'
            ;

instance_var_decl_list_e
            :   /* empty */
            |   instance_var_decl_list
            ;

instance_var_decl_list
            :   access_modifier instance_var_decl
            |   access_modifier instance_var_decl instance_var_decl_list
            ;

instance_var_decl
            :   type init_decl ';'
            ;

access_modifier
            :   /* empty */
            |   PUBLIC
            |   PROTECTED
            |   PRIVATE
            ;

interface_decl_list
            :   
            |   interface_decl_list property
            |   interface_decl_list method_decl
            ;

property    :   PROPERTY '(' attribute ')' type ID ';'
            |   PROPERTY type ID ';'
            ;

attribute   :   READONLY
            |   READWRITE
            ;

method_decl :   class_method_decl
            |   instance_method_decl
            ;

class_method_decl
            :   '+' method_no_args ';'
            |   '+' method_has_args ';'
            |   '+' method_sel ';'
            ;

instance_method_decl
            :   '-' method_no_args ';'
            |   '-' method_has_args ';'
            |   '-' method_sel ';'
            ;

method_no_args
            :   '(' type ')' ID
            |   '(' VOID ')' ID
            ;

method_has_args
            :   '(' type ')' method_sel
            |   '(' VOID ')' method_sel
            ;

method_sel  :   method_param
            |   method_sel method_param
            ;

method_param:   ID ':' '(' type ')' ID
            |   ID ':' '(' type '[' ']' ')' ID
            |   ID ':' '(' type array_size_spec ')' ID
            |   ID ':' '(' type array_size_spec '[' ']' ')' ID
            ;

type        :   INT
            |   CHAR
            |   FLOAT
            |   BOOL
            |   TYPE_ID
            |   CLASS_NAME '*'
            ;

class_implementation
            :   IMPLEMENTATION CLASS_NAME implementation_body END
            |   IMPLEMENTATION CLASS_NAME ':' ID implementation_body END
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
            :   '+' method_no_args compound_stmt
            |   '+' method_has_args compound_stmt
            |   '+' method_sel compound_stmt
            ;

instance_method_def
            :   '-' method_no_args compound_stmt
            |   '-' method_has_args compound_stmt
            |   '-' method_sel compound_stmt
            ;

decl        :   type declarator_list
            ;

declarator_list
            :   init_decl
            |   declarator_list ',' init_decl
            ;

declarator  :   ID
            |   declarator '[' expr ']'
            ;

init_decl   :   declarator
            |   declarator '=' initializer
            |   declarator '[' ']' '=' initializer
            ;

initializer :   expr
            |   '{' initializer_list_e '}'
            ;

initializer_list_e
            :   /* empty */
            |   initializer_list
            ;

initializer_list
            :   initializer
            |   initializer_list ',' initializer
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

stmt_list_e :   /* empty */
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
            |   decl ';'
            ;

expr_e      :   /* empty */
            |   expr
            ;

if_stmt     :   IF '(' expr ')' stmt    %prec NO_ELSE
            |   IF '(' expr ')' stmt ELSE stmt
            ;

for_stmt    :   FOR '(' for_init ';' expr_e ';' expr_e ')' stmt
            |   FOR '(' ID IN expr ')' stmt
            |   FOR '(' type ID IN expr ')' stmt
            ;

for_init    :   expr_e
            |   decl
            ;

while_stmt  :   WHILE '(' expr ')' stmt
            ;

do_while_stmt
            :   DO stmt WHILE '(' expr ')' ';'
            ;

expr        :   ID
            |   literal
            |   objc_literal
            |   NIL
            |   '(' expr ')'
            |   '[' receiver msg_sel ']'
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
            |   expr '[' expr ']'
            |   ID '(' expr_list_e ')'
            |   expr '.' expr
            |   expr ARROW expr
            ;

receiver    :   SUPER
            |   expr
            ;

msg_sel     :   ID
            |   msg_arg_list
            ;

msg_arg_list
            :   msg_arg
            |   msg_arg_list msg_arg
            ;

msg_arg :   ID ':' expr
            ;

literal     :   STRING_LIT
            |   CHAR_LIT
            |   BOOL_LIT
            |   num_literal
            ;

objc_literal:   ATSIGN literal
            |   ATSIGN '[' expr_list_e ']'
            |   ATSIGN '(' expr ')'
            ;

num_literal :   INT_LIT
            |   FLOAT_LIT
            ;

func_decl   :   type ID '(' param_list_e ')' ';'
            ;

func_def    :   type ID '(' param_list_e ')' compound_stmt
            ;

param_list_e:   /* empty */
            |   param_list
            ;

param_list  :   param_decl
            |   param_list ',' param_decl
            ;

param_decl  :   type ID
            |   type ID '[' ']'
            |   type ID array_size_spec
            |   type ID array_size_spec '[' ']'
            ;

array_size_spec
            :   '[' expr ']'
            |   array_size_spec '[' expr ']'
            ;

%%