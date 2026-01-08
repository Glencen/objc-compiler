%code requires {
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include "classes.h"
#include "output_utils.h"

using namespace std;
}

%code provides {
extern int yylex(void);
extern int yylineno;
extern char* yytext; 
void yyerror(char const* s);   
}

%{
#include "objc-parser.hpp"
ProgramNode* root = nullptr;

enum PropertyAttr {
    PROP_READONLY,
    PROP_READWRITE
};

PropertyNode::Attribute convertAttr(int attribute);
%}

%union {
    int int_lit;
    std::string *identifier;
    char char_lit;
    float float_lit;
    bool bool_lit;
    std::string *str_lit;
    ValueNode *value_node;
    ReceiverNode *receiver_node;
    MsgArgNode *msg_arg_node;
    MsgArgListNode *msg_arg_list_node;
    MsgSelectorNode *msg_selector_node;
    ExprListNode *expr_list_node;
    ExprNode *expr_node;
    TypeNode *type_node;
    DeclaratorListNode *declarator_list_node;
    DeclNode *decl_node;
    StmtListNode *stmt_list_node;
    StmtNode *stmt_node;
    ArraySizeSpecNode *array_size_spec_node;
    ParamDeclNode *param_decl_node;
    ParamListNode *param_list_node;
    FuncDefNode *func_def_node;
    FuncDeclNode *func_decl_node;
    MethodParamNode *method_param_node;
    MethodSelNode *method_sel_node;
    InstanceMethodDefNode *instance_method_def_node;
    ClassMethodDefNode *class_method_def_node;
    ImplementationDefListNode *implementation_def_list_node;
    ImplementationBodyNode *implementation_body_node;
    InstanceMethodDeclNode *instance_method_decl_node;
    ClassMethodDeclNode *class_method_decl_node;
    PropertyNode *property_node;
    InterfaceDeclListNode *interface_decl_list_node;
    InitializerListNode *initializer_list_node;
    InitializerNode *initializer_node;
    DeclaratorNode *declarator_node;
    InitDeclNode *init_decl_node;
    AccessModifierNode *access_modifier_node;
    InstanceVarDeclNode *instance_var_decl_node;
    InstanceVarsDeclListNode *instance_vars_decl_list_node;
    InstanceVarsNode *instance_vars_node;
    InterfaceBodyNode *interface_body_node;
    ImplementationNode *implementation_node;
    InterfaceNode *interface_node;
    ClassNameListNode *class_name_list_node;
    ExternalDeclNode *external_decl_node;
    ExternalDeclListNode *external_decl_list_node;
    ProgramNode *program_node;
    int prop_attr;
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
%token VOID
%token TYPE_ID

%token 	<int_lit>		INT_LIT
%token	<float_lit>		FLOAT_LIT
%token	<bool_lit>		BOOL_LIT
%token	<identifier>	ID
%token  <identifier>    CLASS_NAME
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

%type <value_node>                      literal
%type <receiver_node>                   receiver
%type <msg_arg_node>                    msg_arg
%type <msg_arg_list_node>               msg_arg_list
%type <msg_selector_node>               msg_sel
%type <expr_list_node>                  expr_list_e expr_list
%type <expr_node>                       expr_e expr
%type <type_node>                       type
%type <declarator_list_node>            declarator_list
%type <decl_node>                       decl
%type <stmt_list_node>                  stmt_list_e stmt_list
%type <stmt_node>                       stmt compound_stmt if_stmt for_stmt while_stmt do_while_stmt
%type <array_size_spec_node>            array_size_spec
%type <param_decl_node>                 param_decl
%type <param_list_node>                 param_list_e param_list
%type <func_def_node>                   func_def
%type <func_decl_node>                  func_decl
%type <method_param_node>               method_param
%type <method_sel_node>                 method_sel
%type <instance_method_def_node>        instance_method_def
%type <class_method_def_node>           class_method_def
%type <implementation_def_list_node>    implementation_def_list
%type <implementation_body_node>        implementation_body
%type <instance_method_decl_node>       instance_method_decl
%type <class_method_decl_node>          class_method_decl
%type <property_node>                   property
%type <prop_attr>                       attribute
%type <interface_decl_list_node>        interface_decl_list
%type <initializer_list_node>           initializer_list_e initializer_list
%type <initializer_node>                initializer
%type <declarator_node>                 declarator
%type <init_decl_node>                  init_decl
%type <access_modifier_node>            access_modifier
%type <instance_var_decl_node>          instance_var_decl
%type <instance_vars_decl_list_node>    instance_var_decl_list
%type <instance_vars_node>              instance_vars
%type <interface_body_node>             interface_body
%type <implementation_node>             class_implementation
%type <interface_node>                  class_interface
%type <class_name_list_node>            class_name_list
%type <external_decl_node>              external_decl
%type <external_decl_list_node>         external_decl_list_e external_decl_list
%type <program_node>                    program

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

program     :   external_decl_list_e    {root=ProgramNode::createProgram($1);}
            ;

external_decl_list_e
            :   /* empty */             {$$=ExternalDeclListNode::createExternalDeclList();}
            |   external_decl_list      {$$=$1;}
            ;

external_decl_list
            :   external_decl                       {$$=ExternalDeclListNode::createExternalDeclList($1);}
            |   external_decl_list external_decl    {$$=ExternalDeclListNode::addExternalDecl($1, $2);}
            ;

external_decl
            :   class_interface             {$$=ExternalDeclNode::createInterface($1);}
            |   class_implementation        {$$=ExternalDeclNode::createImplementation($1);}
            |   CLASS class_name_list ';'   {$$=ExternalDeclNode::createFwClassDeclList($2);}
            |   func_decl                   {$$=ExternalDeclNode::createFuncDecl($1);}
            |   func_def                    {$$=ExternalDeclNode::createFuncDef($1);}
            ;

class_name_list
            :   CLASS_NAME                          {$$=ClassNameListNode::createClassFwDeclList(ValueNode::createClassName($1));}
            |   class_name_list ',' CLASS_NAME      {$$=ClassNameListNode::addClassFwDecl($1, ValueNode::createClassName($3));}
            ;

class_interface
            :   INTERFACE CLASS_NAME ':' CLASS_NAME interface_body END  {$$=InterfaceNode::createInterface(ValueNode::createClassName($2), ValueNode::createClassName($4), $5);}
            |   INTERFACE CLASS_NAME interface_body END                 {$$=InterfaceNode::createInterface(ValueNode::createClassName($2), $3);}
            ;

interface_body
            :   instance_vars interface_decl_list   {$$=InterfaceBodyNode::createInterfaceBody($1, $2);}
            |   interface_decl_list                 {$$=InterfaceBodyNode::createInterfaceBody($1);}
            ;

instance_vars
            :   '{' '}'                             {$$=InstanceVarsNode::createInstanceVars();}
            |   '{' instance_var_decl_list '}'      {$$=InstanceVarsNode::createInstanceVars($2);}
            ;

instance_var_decl_list
            :   instance_var_decl                           {$$=InstanceVarsDeclListNode::createInstanceVarsDeclList($1);}
            |   instance_var_decl instance_var_decl_list    {$$=InstanceVarsDeclListNode::addInstanceVarDecl($1, $2);}
            ;

instance_var_decl
            :   access_modifier type init_decl ';'      {$$=InstanceVarDeclNode::createInstanceVarDecl($1, $2, $3);}
            ;

access_modifier
            :   /* empty */     {$$=nullptr;}
            |   PUBLIC          {$$=AccessModifierNode::createPublic();}
            |   PROTECTED       {$$=AccessModifierNode::createProtected();}
            |   PRIVATE         {$$=AccessModifierNode::createPrivate();}
            ;

interface_decl_list
            :   /* empty */                                     {$$=InterfaceDeclListNode::createInterfaceDeclList();}
            |   interface_decl_list property                    {$$=InterfaceDeclListNode::addProperty($1, $2);}
            |   interface_decl_list class_method_decl           {$$=InterfaceDeclListNode::addClassMethodDecl($1, $2);}
            |   interface_decl_list instance_method_decl        {$$=InterfaceDeclListNode::addInstanceMethodDecl($1, $2);}
            ;

property    :   PROPERTY '(' attribute ')' type ID ';'      {$$=PropertyNode::createProperty(convertAttr($3), $5, ValueNode::createIdentifier($6));}
            |   PROPERTY type ID ';'                        {$$=PropertyNode::createProperty($2, ValueNode::createIdentifier($3));}
            ;

attribute   :   READONLY        {$$=PROP_READONLY;}
            |   READWRITE       {$$=PROP_READWRITE;}
            ;

class_method_decl
            :   '+' '(' type ')' ID ';'             {$$=ClassMethodDeclNode::createClassMethodDecl($3, ValueNode::createIdentifier($5));}
            |   '+' '(' VOID ')' ID ';'             {$$=ClassMethodDeclNode::createClassMethodDecl(ValueNode::createIdentifier($5));}
            |   '+' '(' type ')' method_sel ';'     {$$=ClassMethodDeclNode::createClassMethodDecl($3, $5);}
            |   '+' '(' VOID ')' method_sel ';'     {$$=ClassMethodDeclNode::createClassMethodDecl($5);}
            ;

instance_method_decl
            :   '-' '(' type ')' ID ';'             {$$=InstanceMethodDeclNode::createInstanceMethodDecl($3, ValueNode::createIdentifier($5));}
            |   '-' '(' VOID ')' ID ';'             {$$=InstanceMethodDeclNode::createInstanceMethodDecl(ValueNode::createIdentifier($5));}
            |   '-' '(' type ')' method_sel ';'     {$$=InstanceMethodDeclNode::createInstanceMethodDecl($3, $5);}
            |   '-' '(' VOID ')' method_sel ';'     {$$=InstanceMethodDeclNode::createInstanceMethodDecl($5);}
            ;

method_sel  :   method_param                {$$=MethodSelNode::createMethodSel($1);}
            |   method_sel method_param     {$$=MethodSelNode::addMethodParam($1, $2);}
            ;

method_param:   ID ':' '(' type ')' ID                              {$$=MethodParamNode::createMethodParam(ValueNode::createIdentifier($1), $4, ValueNode::createIdentifier($6));}
            |   ID ':' '(' type '[' ']' ')' ID                      {$$=MethodParamNode::createArrayMethodParam(ValueNode::createIdentifier($1), $4, ValueNode::createIdentifier($8));}
            |   ID ':' '(' type array_size_spec ')' ID              {$$=MethodParamNode::createSizedArrayMethodParam(ValueNode::createIdentifier($1), $4, $5, ValueNode::createIdentifier($7));}
            |   ID ':' '(' type array_size_spec '[' ']' ')' ID      {$$=MethodParamNode::createSizedArrayOfArraysMethodParam(ValueNode::createIdentifier($1), $4, $5, ValueNode::createIdentifier($9));}
            ;

type        :   INT                 {$$=TypeNode::createIntType();}
            |   CHAR                {$$=TypeNode::createCharType();}
            |   FLOAT               {$$=TypeNode::createFloatType();}
            |   BOOL                {$$=TypeNode::createBoolType();}
            |   TYPE_ID             {$$=TypeNode::createIdType();}
            |   CLASS_NAME '*'      {$$=TypeNode::createClassNameType(ValueNode::createClassName($1));}
            ;

class_implementation
            :   IMPLEMENTATION CLASS_NAME implementation_body END                   {$$=ImplementationNode::createImplementation(ValueNode::createClassName($2), $3);}
            |   IMPLEMENTATION CLASS_NAME ':' CLASS_NAME implementation_body END    {$$=ImplementationNode::createImplementation(ValueNode::createClassName($2), ValueNode::createClassName($4), $5);}
            ;

implementation_body
            :   instance_vars implementation_def_list       {$$=ImplementationBodyNode::createImplementationBody($1, $2);}
            |   implementation_def_list                     {$$=ImplementationBodyNode::createImplementationBody($1);}
            ;

implementation_def_list
            :   property                                        {$$=ImplementationDefListNode::createImplementationDefList($1);}
            |   class_method_def                                {$$=ImplementationDefListNode::createImplementationDefList($1);}
            |   instance_method_def                             {$$=ImplementationDefListNode::createImplementationDefList($1);}
            |   implementation_def_list property                {$$=ImplementationDefListNode::addProperty($1, $2);}
            |   implementation_def_list class_method_def        {$$=ImplementationDefListNode::addClassMethodDef($1, $2);}
            |   implementation_def_list instance_method_def     {$$=ImplementationDefListNode::addInstanceMethodDef($1, $2);}
            ;

class_method_def
            :   '+' '(' type ')' ID compound_stmt               {$$=ClassMethodDefNode::createClassMethodDef($3, ValueNode::createIdentifier($5), $6);}
            |   '+' '(' VOID ')' ID compound_stmt               {$$=ClassMethodDefNode::createClassMethodDef(ValueNode::createIdentifier($5), $6);}
            |   '+' '(' type ')' method_sel compound_stmt       {$$=ClassMethodDefNode::createClassMethodDef($3, $5, $6);}
            |   '+' '(' VOID ')' method_sel compound_stmt       {$$=ClassMethodDefNode::createClassMethodDef($5, $6);}
            ;

instance_method_def
            :   '-' '(' type ')' ID compound_stmt               {$$=InstanceMethodDefNode::createInstanceMethodDef($3, ValueNode::createIdentifier($5), $6);}
            |   '-' '(' VOID ')' ID compound_stmt               {$$=InstanceMethodDefNode::createInstanceMethodDef(ValueNode::createIdentifier($5), $6);}
            |   '-' '(' type ')' method_sel compound_stmt       {$$=InstanceMethodDefNode::createInstanceMethodDef($3, $5, $6);}
            |   '-' '(' VOID ')' method_sel compound_stmt       {$$=InstanceMethodDefNode::createInstanceMethodDef($5, $6);}
            ;

decl        :   type declarator_list    {$$=DeclNode::createDecl($1, $2);}
            ;

declarator_list
            :   init_decl                           {$$=DeclaratorListNode::createExternalDeclList($1);}
            |   declarator_list ',' init_decl       {$$=DeclaratorListNode::addExternalDecl($1, $3);}
            ;

declarator  :   ID                          {$$=DeclaratorNode::createDeclarator(ValueNode::createIdentifier($1));}
            |   declarator '[' expr ']'     {$$=DeclaratorNode::addArrayAccess($1, $3);}
            ;

init_decl   :   declarator                              {$$=InitDeclNode::createDeclarator($1);}
            |   declarator '=' initializer              {$$=InitDeclNode::createInitialized($1, $3);}
            |   declarator '[' ']' '=' initializer      {$$=InitDeclNode::createArrayInitialized($1, $5);}
            ;

initializer :   expr                            {$$=InitializerNode::createExpr($1);}
            |   '{' initializer_list_e '}'      {$$=InitializerNode::createArrayInitializer($2);}
            ;

initializer_list_e
            :   /* empty */             {$$=InitializerListNode::createInitializerList();}
            |   initializer_list        {$$=$1;}
            ;

initializer_list
            :   initializer                             {$$=InitializerListNode::createInitializerList($1);}
            |   initializer_list ',' initializer        {$$=InitializerListNode::addInitializer($1, $3);}
            ;

expr_list_e :   /* empty */     {$$=nullptr;}
            |   expr_list       {$$=$1;}
            ;

expr_list   :   expr                    {$$=ExprListNode::createExprList($1);}
            |   expr_list ',' expr      {$$=ExprListNode::addExprToList($1, $3);}
            ;

compound_stmt
            :   '{' stmt_list_e '}'     {$$=StmtNode::createCompound($2);}
            ;

stmt_list_e :   /* empty */     {$$=StmtListNode::createStmtList();}
            |   stmt_list       {$$=$1;}
            ;

stmt_list   :   stmt                {$$=StmtListNode::createStmtList($1);}
            |   stmt_list stmt      {$$=StmtListNode::addStmtToList($1, $2);}
            ;

stmt        :   ';'                 {$$=StmtNode::createEmpty();}
            |   expr ';'            {$$=StmtNode::createExpr($1);}
            |   RETURN expr_e ';'   {$$=StmtNode::createReturn($2);}
            |   if_stmt             {$$=$1;}
            |   for_stmt            {$$=$1;}
            |   while_stmt          {$$=$1;}
            |   do_while_stmt       {$$=$1;}
            |   compound_stmt       {$$=$1;}
            |   decl ';'            {$$=StmtNode::createDeclaration($1);}
            ;

expr_e      :   /* empty */     {$$=nullptr;}
            |   expr            {$$=$1;}
            ;

if_stmt     :   IF '(' expr ')' stmt    %prec NO_ELSE       {$$=StmtNode::createIf($3, $5);}
            |   IF '(' expr ')' stmt ELSE stmt              {$$=StmtNode::createIfElse($3, $5, $7);}
            ;

for_stmt    :   FOR '(' expr_e ';' expr_e ';' expr_e ')' stmt   {$$=StmtNode::createFor($3, $5, $7, $9);}
            |   FOR '(' decl ';' expr_e ';' expr_e ')' stmt     {$$=StmtNode::createFor($3, $5, $7, $9);}
            |   FOR '(' ID IN expr ')' stmt                     {$$=StmtNode::createForIn(ValueNode::createIdentifier($3), $5, $7);}
            |   FOR '(' type ID IN expr ')' stmt                {$$=StmtNode::createTypedForIn($3, ValueNode::createIdentifier($4), $6, $8);}
            ;

while_stmt  :   WHILE '(' expr ')' stmt     {$$=StmtNode::createWhile($3, $5);}
            ;

do_while_stmt
            :   DO stmt WHILE '(' expr ')' ';'      {$$=StmtNode::createDoWhile($2, $5);}
            ;

expr        :   ID                              {$$=ExprNode::createIdentifier(ValueNode::createIdentifier($1));}
            |   literal                         {$$=ExprNode::createLiteral($1);}
            |   ATSIGN '[' expr_list_e ']'      {$$=ExprNode::createObjcArrayLiteral($3);}
            |   ATSIGN '(' expr ')'             {$$=ExprNode::createObjcBoxedExpr($3);}
            |   NIL                             {$$=ExprNode::createNil();}
            |   '(' expr ')'                    {$$=ExprNode::createBoxedExpr($2);}
            |   '[' receiver msg_sel ']'        {$$=ExprNode::createMessageSend($2, $3);}
            |   SELF                            {$$=ExprNode::createSelf();}
            |   '-' expr    %prec UMINUS        {$$=ExprNode::createUnaryMinus($2);}
            |   '!' expr                        {$$=ExprNode::createNot($2);}
            |   expr INC                        {$$=ExprNode::createPostInc($1);}
            |   expr DEC                        {$$=ExprNode::createPostDec($1);}
            |   expr '+' expr                   {$$=ExprNode::createAddition($1, $3);}
            |   expr '-' expr                   {$$=ExprNode::createSubtraction($1, $3);}
            |   expr '*' expr                   {$$=ExprNode::createMultiplication($1, $3);}
            |   expr '/' expr                   {$$=ExprNode::createDivision($1, $3);}
            |   expr EQUAL expr                 {$$=ExprNode::createEqual($1, $3);}
            |   expr NEQUAL expr                {$$=ExprNode::createNotEqual($1, $3);}
            |   expr '>' expr                   {$$=ExprNode::createGreater($1, $3);}
            |   expr '<' expr                   {$$=ExprNode::createLess($1, $3);}
            |   expr LESS_EQUAL expr            {$$=ExprNode::createLessOrEqual($1, $3);}
            |   expr GREATER_EQUAL expr         {$$=ExprNode::createGreaterOrEqual($1, $3);}
            |   expr AND expr                   {$$=ExprNode::createAnd($1, $3);}
            |   expr OR expr                    {$$=ExprNode::createOr($1, $3);}
            |   expr '=' expr                   {$$=ExprNode::createAssign($1, $3);}
            |   expr '[' expr ']'               {$$=ExprNode::createArrayAccess($1, $3);}
            |   ID '(' expr_list_e ')'          {$$=ExprNode::createFunctionCall(ValueNode::createIdentifier($1), $3);}
            |   expr '.' expr                   {$$=ExprNode::createDot($1, $3);}
            |   expr ARROW expr                 {$$=ExprNode::createArrow($1, $3);}
            ;

receiver    :   SUPER       {$$=ReceiverNode::createSuper();}
            |   CLASS_NAME  {$$=ReceiverNode::createClassName(ValueNode::createClassName($1));}
            |   expr        {$$=ReceiverNode::createExpr($1);}
            ;

msg_sel     :   ID                  {$$=MsgSelectorNode::createSimpleSel(ValueNode::createIdentifier($1));}
            |   msg_arg_list        {$$=MsgSelectorNode::createArgumentList($1);}
            ;

msg_arg_list
            :   msg_arg                     {$$=MsgArgListNode::createMsgArgList($1);}
            |   msg_arg_list msg_arg        {$$=MsgArgListNode::addMsgArg($1, $2);}
            ;

msg_arg     :   ID ':' expr     {$$=MsgArgNode::createMsgArg(ValueNode::createIdentifier($1), $3);}
            ;

literal     :   STRING_LIT              {$$=ValueNode::createString($1);}
            |   CHAR_LIT                {$$=ValueNode::createChar($1);}
            |   BOOL_LIT                {$$=ValueNode::createBool($1);}
            |   INT_LIT                 {$$=ValueNode::createInt($1);}
            |   FLOAT_LIT               {$$=ValueNode::createFloat($1);}
            |   ATSIGN STRING_LIT       {$$=ValueNode::createObjcString($2);}
            |   ATSIGN BOOL_LIT         {$$=ValueNode::createObjcBool($2);}
            |   ATSIGN INT_LIT          {$$=ValueNode::createObjcInt($2);}
            |   ATSIGN FLOAT_LIT        {$$=ValueNode::createObjcFloat($2);}
            ;

func_decl   :   type ID '(' param_list_e ')' ';'    {$$=FuncDeclNode::createFuncDecl($1, ValueNode::createIdentifier($2), $4);}
            ;

func_def    :   type ID '(' param_list_e ')' compound_stmt      {$$=FuncDefNode::createFuncDef($1, ValueNode::createIdentifier($2), $4, $6);}
            ;

param_list_e:   /* empty */     {$$=ParamListNode::createParamList();}
            |   param_list      {$$=$1;}
            ;

param_list  :   param_decl                      {$$=ParamListNode::createParamList($1);}
            |   param_list ',' param_decl       {$$=ParamListNode::addParamDecl($1, $3);}
            ;

param_decl  :   type ID                             {$$=ParamDeclNode::createParamDecl($1, ValueNode::createIdentifier($2));}
            |   type ID '[' ']'                     {$$=ParamDeclNode::createArrayParamDecl($1, ValueNode::createIdentifier($2));}
            |   type ID array_size_spec             {$$=ParamDeclNode::createSizedArrayParamDecl($1, ValueNode::createIdentifier($2), $3);}
            |   type ID array_size_spec '[' ']'     {$$=ParamDeclNode::createSizedArrayOfArraysParamDecl($1, ValueNode::createIdentifier($2), $3);}
            ;

array_size_spec
            :   '[' expr ']'                        {$$=ArraySizeSpecNode::createArraySizeSpec($2);}
            |   array_size_spec '[' expr ']'        {$$=ArraySizeSpecNode::addDimension($1, $3);}
            ;

%%

PropertyNode::Attribute convertAttr(int attribute) {
    return (attribute == PROP_READONLY) ? PropertyNode::Attribute::READONLY : PropertyNode::Attribute::READWRITE;
}

void yyerror(const char* s) {
    cout << "Parser error: " << s << " at line " << yylineno << endl;
    cout << "Current token: " << yytext << endl;
    TokenOutput::getInstance().setParserError();
}