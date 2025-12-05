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
%token  <c_str_lit>     C_STRING_LIT
%token  <nsstring_lit>  NSSTRING_LIT

%token SUPER
%token SELF
%token CLASS_NAME
%token CLASS
%token READWRITE
%token READONLY
%token IN

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

program     :   stmt_list
            |   class_decl_list
            |   class_fw_decl_list
            |   func_decl_list
            |   func_def_list
            ;

class_fw_decl_list
            :   CLASS class_list ';'
            ;

class_list  :   ID
            |   class_list ',' ID
            ;

class_decl_list
            :   class_decl
            |   class_decl_list class_decl
            ;

class_decl  :   class_interface
            |   class_implementation
            ;

class_interface
            :   INTERFACE ID ':' ID interface_body END
            |   INTERFACE ID interface_body END
            |   INTERFACE ID ':' CLASS_NAME interface_body END
            ;

interface_body
            :   instance_vars interface_decl_list
            |   interface_decl_list
            ;

instance_vars
            :   '{' decl_list '}'
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
            |   ID
            |   CLASS_NAME '*'
            |   NSNUMBER '*'
            ;

class_implementation
            :   IMPLEMENTATION ID implementation_body END
            |   IMPLEMENTATION ID ':' ID implementation_body END
            |   IMPLEMENTATION CLASS_NAME implementation_body END
            |   IMPLEMENTATION CLASS_NAME ':' ID implementation_body END
            |   IMPLEMENTATION ID ':' CLASS_NAME implementation_body END
            |   IMPLEMENTATION CLASS_NAME ':' CLASS_NAME implementation_body END
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
            |   nsarray_decl
            ;

c_array_decl:   ID array_decl
            |   ID array_decl '=' c_array_lit
            ;

c_array_lit:   '{' expr_list_e '}'
            ;

nsarray_decl:   nsarray_type '*' ID
            |   nsarray_type '*' ID '=' nsarray_lit
            ;

nsarray_type:   NSARRAY
            |   NSMUTABLEARRAY
            ;

array_decl  :   '[' ']'
            |   '[' expr ']'
            ;

array_access:   primary_expr '[' expr ']'
            ;

expr_list_e :
            |   expr_list
            ;

expr_list   :   expr
            |   expr_list ',' expr
            ;

nsarray_lit:   '@' '[' nsobject_list_e ']'
            ;

nsobject_list_e
            :
            |   nsobject_list
            ;

nsobject_list
            :   nsobject
            |   nsobject_list ',' nsobject
            ;

nsobject    :   expr
            |   '@' expr
            |   '@' '(' expr ')'
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

expr        :   primary_expr
            |   num_const
            |   SELF
            |   '-' expr    %prec UMINUS
            |   '!' expr
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
            ;

primary_expr:   ID
            |   literal
            |   nsarray_lit
            |   '(' expr ')'
            |   msg_expr
            ;

msg_expr    :   '[' receiver msg_sel ']'
            ;

receiver    :   SELF
            |   SUPER
            |   CLASS_NAME
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

literal     :   C_STRING_LIT
            |   CHAR_LIT
            |   NSSTRING_LIT
            ;

num_const   :   INT_LIT
            |   FLOAT_LIT
            ;

func_decl_list
            :   func_decl
            |   func_decl_list func_decl
            ;

func_def_list
            :   func_def
            |   func_def_list func_def
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