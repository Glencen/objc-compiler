%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylex(void);
extern FILE *yyin;
void yyerror(const char *s);

typedef enum { TYPE_INT, TYPE_FLOAT } var_type_t;

typedef struct {
    char* name;
    var_type_t type;
    union {
        long intval;
        double floatval;
    } value;
} variable_t;

#define MAX_VARS 100
variable_t vars[MAX_VARS];
int var_count = 0;

variable_t* find_var(const char* name) {
    for (int i = 0; i < var_count; i++)
        if (strcmp(vars[i].name, name) == 0)
            return &vars[i];
    return NULL;
}

void set_var(const char* name, var_type_t type, long intval, double floatval) {
    variable_t* var = find_var(name);
    if (var) {
        var->type = type;
        if (type == TYPE_INT) var->value.intval = intval;
        else var->value.floatval = floatval;
        return;
    }
    if (var_count < MAX_VARS) {
        vars[var_count].name = strdup(name);
        vars[var_count].type = type;
        if (type == TYPE_INT) vars[var_count].value.intval = intval;
        else vars[var_count].value.floatval = floatval;
        var_count++;
    }
}

%}

%union {
    long intval;
    double floatval;
    char* idname;
    var_type_t type;
}

%token <idname> IDENTIFIER
%token <intval> INTEGER
%token <floatval> FLOAT
%token '=' ';' '+' '-' '*' '/' '(' ')'

%type <floatval> expr_float
%type <type> type

%left '+' '-'
%left '*' '/'
%left UMINUS

%%

program:
      /* пусто */
    | program statement
    ;

statement:
      type IDENTIFIER '=' expr_float ';'
    {
        if ($1 == TYPE_INT)
            set_var($2, TYPE_INT, (long)$4, 0.0);
        else if ($1 == TYPE_FLOAT)
            set_var($2, TYPE_FLOAT, 0, $4);
        free($2);
    }
    ;

type:
      "int"    { $$ = TYPE_INT; }
    | "float"  { $$ = TYPE_FLOAT; }
    ;

expr_float:
       INTEGER           { $$ = (double)$1; }
    | FLOAT             { $$ = $1; }
    | IDENTIFIER        {
          variable_t* var = find_var($1);
          if (!var) { yyerror("Unknown variable"); $$ = 0; }
          else if (var->type == TYPE_INT) $$ = (double)var->value.intval;
          else if (var->type == TYPE_FLOAT) $$ = var->value.floatval;
          free($1);
      }
    | expr_float '+' expr_float { $$ = $1 + $3; }
    | expr_float '-' expr_float { $$ = $1 - $3; }
    | expr_float '*' expr_float { $$ = $1 * $3; }
    | expr_float '/' expr_float { $$ = $1 / $3; }
    | '(' expr_float ')'        { $$ = $2; }
    | '-' expr_float %prec UMINUS { $$ = -$2; }
    ;

%%

int main(int argc, char **argv) {
    if (argc > 1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            fprintf(stderr, "Error: Cannot open file %s\n", argv[1]);
            return 1;
        }
        yyin = file;
    }

    printf("Parsing simple typed assignments...\n");
    yyparse();
    printf("Parsing finished.\n");

    if (argc > 1) fclose(yyin);
    return 0;
}

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}
