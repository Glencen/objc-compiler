#ifndef CLASSES_H
#define CLASSES_H

#include <iostream>
#include <string>
#include <list>

using namespace std;

class StmtNode;
class ExprNode;
class PropertyNode;
class TypeNode;
class ValueNode;
class InitDeclNode;
class DeclaratorNode;
class InstanceVarsNode;
class InitializerNode;
class InitializerListNode;

class AstNode {
protected:
    static unsigned int maxId;
    unsigned int id;

    void appendDotNode(string &res) const;
    void appendDotEdge(string &res, const AstNode *child, const string &edgeLabel) const;

public:
    AstNode() {id = ++maxId;};
    virtual ~AstNode() = default;

    unsigned int getId() const {return id;};

    virtual string getDotLabel() const = 0;
    virtual string toDot() const = 0;
};

class ValueNode : public AstNode {
public:
    enum ValueType {
        NONE,
        INT_LIT,
        FLOAT_LIT,
        BOOL_LIT,
        CHAR_LIT,
        STRING_LIT,
        NIL,
        OBJC_INT_LIT,
        OBJC_FLOAT_LIT,
        OBJC_BOOL_LIT,
        OBJC_STRING_LIT,
        IDENTIFIER,
        CLASS_NAME
    };

    static ValueNode* createInt(int value);
    static ValueNode* createFloat(float value);
    static ValueNode* createBool(bool value);
    static ValueNode* createChar(char value);
    static ValueNode* createString(string *value);
    static ValueNode* createNil();
    static ValueNode* createObjcInt(int value);
    static ValueNode* createObjcFloat(float value);
    static ValueNode* createObjcBool(bool value);
    static ValueNode* createObjcString(string *value);
    static ValueNode* createIdentifier(string *value);
    static ValueNode* createClassName(string *value);

    ValueType getValueType() const;
    int getInt() const;
    float getFloat() const;
    bool getBool() const;
    char getChar() const;
    string* getString() const;
    string* getObjcInt() const;
    string* getObjcFloat() const;
    string* getObjcBool() const;
    string* getObjcString() const;
    string* getIdentifier() const;
    string* getClassName() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueType valueType;
    int intValue;
    float floatValue;
    bool boolValue;
    char charValue;
    string *stringValue;

    ValueNode();
};

class ReceiverNode : public AstNode {
public:
    enum ReceiverType {
        NONE,
        EXPR,
        CLASS_NAME,
        SUPER
    };

    static ReceiverNode* createExpr(ExprNode *expr);
    static ReceiverNode* createClassName(ValueNode *className);
    static ReceiverNode* createSuper();

    ReceiverType getType() const;
    ExprNode* getExpr() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ReceiverType type;
    ValueNode *className;
    ExprNode *expr;

    ReceiverNode();
};

class MsgArgNode : public AstNode {
public:
    static MsgArgNode* createMsgArg(ValueNode *identifier, ExprNode *arg);

    ValueNode* getIdentifier() const;
    ExprNode* getArg() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *identifier;
    ExprNode *arg;

    MsgArgNode();
};

class MsgArgListNode : public AstNode {
public:
    static MsgArgListNode* createMsgArgList(MsgArgNode *arg);
    static MsgArgListNode* addMsgArg(MsgArgListNode *list, MsgArgNode *arg);

    list<MsgArgNode*>* getMsgArgList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<MsgArgNode*> *msgArgs;

    MsgArgListNode();
};

class MsgSelectorNode : public AstNode {
public:
    enum MsgSelectorType {
        NONE,
        SIMPLE_SEL,
        ARGUMENT_LIST
    };

    static MsgSelectorNode* createSimpleSel(ValueNode *identifier);
    static MsgSelectorNode* createArgumentList(MsgArgListNode *list);

    MsgSelectorType getType() const;
    ValueNode* getIdentifier() const;
    MsgArgListNode* getMsgArgList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    MsgSelectorType type;
    ValueNode *identifier;
    MsgArgListNode *argList;

    MsgSelectorNode();
};

class ExprListNode : public AstNode {
public:
    static ExprListNode* createExprList(ExprNode *expr);
    static ExprListNode* addExprToList(ExprListNode *exprList, ExprNode *expr);

    list<ExprNode*>* getExprList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ExprNode*> *exprList;
    ExprListNode();
};

class ExprNode : public AstNode {
public:
    enum ExprType {
        NONE,
        IDENTIFIER,
        LITERAL,
        OBJC_ARRAY_LITERAL,
        OBJC_BOXED_EXPR,
        NIL,
        BOXED_EXPR,
        MESSAGE,
        SELF,
        UNARY_MINUS,
        NOT,
        POST_INC,
        POST_DEC,
        ADDITION,
        SUBTRACTION,
        MULTIPLICATION,
        DIVISION,
        EQUAL,
        NOT_EQUAL,
        GREATER,
        LESS,
        LESS_OR_EQUAL,
        GREATER_OR_EQUAL,
        AND,
        OR,
        ASSIGN,
        ARRAY_ACCESS,
        FUNCTION_CALL,
        DOT,
        ARROW
    };

    static ExprNode* createIdentifier(ValueNode *value);
    static ExprNode* createLiteral(ValueNode *value);
    static ExprNode* createObjcArrayLiteral(ExprListNode *exprList);
    static ExprNode* createObjcBoxedExpr(ExprNode *expr);
    static ExprNode* createNil();
    static ExprNode* createBoxedExpr(ExprNode *expr);
    static ExprNode* createMessageSend(ReceiverNode *receiver, MsgSelectorNode *selector);
    static ExprNode* createSelf();
    static ExprNode* createUnaryMinus(ExprNode *operand);
    static ExprNode* createNot(ExprNode *operand);
    static ExprNode* createPostInc(ExprNode *operand);
    static ExprNode* createPostDec(ExprNode *operand);
    static ExprNode* createAddition(ExprNode *left, ExprNode *right);
    static ExprNode* createSubtraction(ExprNode *left, ExprNode *right);
    static ExprNode* createMultiplication(ExprNode *left, ExprNode *right);
    static ExprNode* createDivision(ExprNode *left, ExprNode *right);
    static ExprNode* createEqual(ExprNode *left, ExprNode *right);
    static ExprNode* createNotEqual(ExprNode *left, ExprNode *right);
    static ExprNode* createGreater(ExprNode *left, ExprNode *right);
    static ExprNode* createLess(ExprNode *left, ExprNode *right);
    static ExprNode* createLessOrEqual(ExprNode *left, ExprNode *right);
    static ExprNode* createGreaterOrEqual(ExprNode *left, ExprNode *right);
    static ExprNode* createAnd(ExprNode *left, ExprNode *right);
    static ExprNode* createOr(ExprNode *left, ExprNode *right);
    static ExprNode* createAssign(ExprNode *left, ExprNode *right);
    static ExprNode* createArrayAccess(ExprNode *operand, ExprNode *index);
    static ExprNode* createFunctionCall(ValueNode *funcId, ExprListNode *args);
    static ExprNode* createDot(ExprNode *left, ExprNode *right);
    static ExprNode* createArrow(ExprNode *left, ExprNode *right);

    ExprType getType() const;
    ValueNode* getIdentifier() const;
    ValueNode* getLiteral() const;
    ExprNode* getLeft() const;
    ExprNode* getRight() const;
    ExprNode* getOperand() const;
    ExprNode* getIndex() const;
    ValueNode* getFuncId() const;
    ExprListNode* getArgs() const;
    ReceiverNode* getReceiver() const;
    MsgSelectorNode* getSelector() const;
    ExprListNode* getObjcArrayExprList() const;
    ExprNode* getBoxedExpr() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExprType type;
    ValueNode *identifier;
    ValueNode *literalValue;
    ExprNode *left;
    ExprNode *right;
    ExprNode *operand;
    ExprNode *index;
    ValueNode *funcId;
    ExprListNode *args;
    ReceiverNode *receiver;
    MsgSelectorNode *selector;
    ExprListNode *objcArrayExprList;
    ExprNode *boxedExpr;

    ExprNode();
};

class TypeNode : public AstNode {
public:
    enum TypeKind {
        NONE,
        INT,
        FLOAT,
        BOOL,
        CHAR,
        TYPE_ID,
        CLASS_NAME,
        VOID
    };

    static TypeNode* createIntType();
    static TypeNode* createCharType();
    static TypeNode* createFloatType();
    static TypeNode* createBoolType();
    static TypeNode* createIdType();
    static TypeNode* createClassNameType(ValueNode *classNameValue);
    static TypeNode* createVoid();

    TypeKind getKind() const;
    ValueNode* getClassName() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    TypeKind kind;
    ValueNode *classNameValue;

    TypeNode();
};

class DeclaratorListNode : public AstNode {
public:
    static DeclaratorListNode* createExternalDeclList(InitDeclNode *initDecl);
    static DeclaratorListNode* addExternalDecl(DeclaratorListNode *declaratorList, InitDeclNode *initDecl);

    list<InitDeclNode*>* getInitDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<InitDeclNode*> *initDeclList;

    DeclaratorListNode();
};

class DeclNode : public AstNode {
public:
    static DeclNode* createDecl(TypeNode *type, DeclaratorListNode *declaratorList);

    TypeNode* getType() const;
    DeclaratorListNode* getDeclaratorList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    TypeNode *type;
    DeclaratorListNode *declaratorList;

    DeclNode();
};

class StmtListNode : public AstNode {
public:
    static StmtListNode* createStmtList();
    static StmtListNode* createStmtList(StmtNode *stmt);
    static StmtListNode* addStmtToList(StmtListNode *list, StmtNode *stmt);

    list<StmtNode*>* getStmtList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<StmtNode*> *stmts;

    StmtListNode();
};

class StmtNode : public AstNode {
public:
    enum StmtType {
        NONE,
        EMPTY,
        EXPR,
        RETURN,
        IF,
        IF_ELSE,
        FOR_WITH_EXPR,
        FOR_WITH_DECL,
        FOR_IN,
        TYPED_FOR_IN,
        WHILE,
        DO_WHILE,
        COMPOUND,
        DECLARATION
    };

    static StmtNode* createEmpty();
    static StmtNode* createExpr(ExprNode *expr);
    static StmtNode* createReturn(ExprNode *expr);
    static StmtNode* createIf(ExprNode *condition, StmtNode *thenBranch);
    static StmtNode* createIfElse(ExprNode *condition, StmtNode *thenBranch, StmtNode *elseBranch);
    static StmtNode* createFor(ExprNode *expr, ExprNode *condition, ExprNode *post, StmtNode *body);
    static StmtNode* createFor(DeclNode *decl, ExprNode *condition, ExprNode *post, StmtNode *body);
    static StmtNode* createForIn(ValueNode *id, ExprNode *collection, StmtNode *body);
    static StmtNode* createTypedForIn(TypeNode *type, ValueNode *id, ExprNode *collection, StmtNode *body);
    static StmtNode* createWhile(ExprNode *condition, StmtNode *body);
    static StmtNode* createDoWhile(StmtNode *body, ExprNode *condition);
    static StmtNode* createCompound(StmtListNode *compound);
    static StmtNode* createDeclaration(DeclNode *decl);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    StmtType type;
    ExprNode *expr;
    ExprNode *condition;
    StmtNode *thenBranch;
    StmtNode *elseBranch;
    ExprNode *post;
    ValueNode *forInId;
    TypeNode *forInType;
    ExprNode *collection;
    StmtNode *body;
    StmtListNode *compound;
    DeclNode *decl;

    StmtNode();
};

class ArraySizeSpecNode : public AstNode {
public:
    static ArraySizeSpecNode* createArraySizeSpec(ExprNode *size);
    static ArraySizeSpecNode* addDimension(ArraySizeSpecNode *spec, ExprNode *size);

    list<ExprNode*>* getSizes() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ExprNode*> *sizes;

    ArraySizeSpecNode();
};

class ParamDeclNode : public AstNode {
public:
    enum ParamDeclKind {
        NONE,
        IDENTIFIER,
        ARRAY,
        SIZED_ARRAY,
        ARRAY_OF_ARRAYS
    };

    static ParamDeclNode* createParamDecl(TypeNode *type, ValueNode *identifier);
    static ParamDeclNode* createArrayParamDecl(TypeNode *type, ValueNode *identifier);
    static ParamDeclNode* createSizedArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec);
    static ParamDeclNode* createSizedArrayOfArraysParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec);

    ParamDeclKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    ArraySizeSpecNode* getSizeSpec() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ParamDeclKind kind;
    TypeNode *type;
    ValueNode *identifier;
    ArraySizeSpecNode *arraySizeSpec;

    ParamDeclNode();
};

class ParamListNode : public AstNode {
public:
    static ParamListNode* createParamList();
    static ParamListNode* createParamList(ParamDeclNode *paramDecl);
    static ParamListNode* addParamDecl(ParamListNode *paramList, ParamDeclNode *paramDecl);

    list<ParamDeclNode*>* getParamList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ParamDeclNode*> *paramList;

    ParamListNode();
};

class FuncDefNode : public AstNode {
public:
    static FuncDefNode* createFuncDef(TypeNode *type, ValueNode *identifier, ParamListNode *paramList, StmtNode *compoundStmt);

    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    ParamListNode* getParamList() const;
    StmtNode* getCompoundStmt() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    TypeNode *type;
    ValueNode *identifier;
    ParamListNode *paramList;
    StmtNode *compoundStmt;

    FuncDefNode();
};

class FuncDeclNode : public AstNode {
public:
    static FuncDeclNode* createFuncDecl(TypeNode *type, ValueNode *identifier, ParamListNode *paramList);

    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    ParamListNode* getParamList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    TypeNode *type;
    ValueNode *identifier;
    ParamListNode *paramList;

    FuncDeclNode();
};

class MethodParamNode : public AstNode {
public:
    enum MethodParamKind {
        NONE,
        IDENTIFIER,
        ARRAY,
        SIZED_ARRAY,
        ARRAY_OF_ARRAYS
    };

    static MethodParamNode* createMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier);
    static MethodParamNode* createArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier);
    static MethodParamNode* createSizedArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier);
    static MethodParamNode* createSizedArrayOfArraysMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier);

    MethodParamKind getKind() const;
    ValueNode* getSelectorIdentifier() const;
    TypeNode* getType() const;
    ValueNode* getParamIdentifier() const;
    ArraySizeSpecNode* getArraySizeSpec() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    MethodParamKind kind;
    ValueNode *selectorIdentifier;
    TypeNode *type;
    ValueNode *paramIdentifier;
    ArraySizeSpecNode *arraySizeSpec;

    MethodParamNode();
};

class MethodSelNode : public AstNode {
public:
    static MethodSelNode* createMethodSel(MethodParamNode *methodParam);
    static MethodSelNode* addMethodParam(MethodSelNode *methodSel, MethodParamNode *methodParam);

    list<MethodParamNode*>* getMethodParamList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<MethodParamNode*> *methodParams;

    MethodSelNode();
};

class InstanceMethodDefNode : public AstNode {
public:
    enum InstanceMethodDefKind {
        NONE,
        TYPE_ID,
        TYPE_SEL,
        VOID_ID,
        VOID_SEL
    };

    static InstanceMethodDefNode* createInstanceMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt);
    static InstanceMethodDefNode* createInstanceMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt);

    InstanceMethodDefKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;
    StmtNode* getCompoundStmt() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceMethodDefKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;
    StmtNode *compoundStmt;

    InstanceMethodDefNode();
};

class ClassMethodDefNode : public AstNode {
public:
    enum ClassMethodDefKind {
        NONE,
        TYPE_ID,
        TYPE_SEL,
        VOID_ID,
        VOID_SEL
    };

    static ClassMethodDefNode* createClassMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt);
    static ClassMethodDefNode* createClassMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt);

    ClassMethodDefKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;
    StmtNode* getCompoundStmt() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ClassMethodDefKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;
    StmtNode *compoundStmt;

    ClassMethodDefNode();
};

class ImplementationDefListNode : public AstNode {
public:
    static ImplementationDefListNode* createImplementationDefList(PropertyNode *property);
    static ImplementationDefListNode* createImplementationDefList(ClassMethodDefNode *classMethodDef);
    static ImplementationDefListNode* createImplementationDefList(InstanceMethodDefNode *instanceMethodDef);
    static ImplementationDefListNode* addProperty(ImplementationDefListNode *implementationDefList, PropertyNode *property);
    static ImplementationDefListNode* addClassMethodDef(ImplementationDefListNode *implementationDefList, ClassMethodDefNode *classMethodDef);
    static ImplementationDefListNode* addInstanceMethodDef(ImplementationDefListNode *implementationDefList, InstanceMethodDefNode *instanceMethodDef);

    list<PropertyNode*>* getProperties() const;
    list<ClassMethodDefNode*>* getClassMethodDefs() const;
    list<InstanceMethodDefNode*>* getInstanceMethodDefs() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<PropertyNode*> *properties;
    list<ClassMethodDefNode*> *classMethodDefs;
    list<InstanceMethodDefNode*> *instanceMethodDefs;
    
    ImplementationDefListNode();
};

class ImplementationBodyNode : public AstNode {
public:
    static ImplementationBodyNode* createImplementationBody(ImplementationDefListNode *implementationDefList);
    static ImplementationBodyNode* createImplementationBody(InstanceVarsNode *instanceVars, ImplementationDefListNode *implementationDefList);

    InstanceVarsNode* getInstanceVars() const;
    ImplementationDefListNode* getImplementationDefList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceVarsNode *instanceVars;
    ImplementationDefListNode *implementationDefList;

    ImplementationBodyNode();
};

class InstanceMethodDeclNode : public AstNode {
public:
    enum InstanceMethodDeclKind {
        NONE,
        TYPE_ID,
        TYPE_SEL,
        VOID_ID,
        VOID_SEL
    };

    static InstanceMethodDeclNode* createInstanceMethodDecl(TypeNode *type, ValueNode *identifier);
    static InstanceMethodDeclNode* createInstanceMethodDecl(TypeNode *type, MethodSelNode *methodSel);

    InstanceMethodDeclKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceMethodDeclKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;

    InstanceMethodDeclNode();
};

class ClassMethodDeclNode : public AstNode {
public:
    enum ClassMethodDeclKind {
        NONE,
        TYPE_ID,
        TYPE_SEL,
        VOID_ID,
        VOID_SEL
    };

    static ClassMethodDeclNode* createClassMethodDecl(TypeNode *type, ValueNode *identifier);
    static ClassMethodDeclNode* createClassMethodDecl(TypeNode *type, MethodSelNode *methodSel);

    ClassMethodDeclKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ClassMethodDeclKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;

    ClassMethodDeclNode();
};

class PropertyNode : public AstNode {
public:
    enum Attribute {
        NONE,
        READONLY,
        READWRITE
    };

    static PropertyNode* createProperty(Attribute attr, TypeNode *type, ValueNode *name);
    static PropertyNode* createProperty(TypeNode *type, ValueNode *name);

    Attribute getAttribute() const;
    TypeNode* getType() const;
    ValueNode* getName() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    Attribute attribute;
    TypeNode *type;
    ValueNode *name;

    PropertyNode();
};

class InterfaceDeclListNode : public AstNode {
public:
    static InterfaceDeclListNode* createInterfaceDeclList();
    static InterfaceDeclListNode* addProperty(InterfaceDeclListNode *interfaceDeclList, PropertyNode *property);
    static InterfaceDeclListNode* addClassMethodDecl(InterfaceDeclListNode *interfaceDeclList, ClassMethodDeclNode *classMethodDecl);
    static InterfaceDeclListNode* addInstanceMethodDecl(InterfaceDeclListNode *interfaceDeclList, InstanceMethodDeclNode *instanceMethodDecl);

    list<PropertyNode*>* getProperties() const;
    list<ClassMethodDeclNode*>* getClassMethodDecls() const;
    list<InstanceMethodDeclNode*>* getInstanceMethodDecls() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<PropertyNode*> *properties;
    list<ClassMethodDeclNode*> *classMethodDecls;
    list<InstanceMethodDeclNode*> *instanceMethodDecls;

    InterfaceDeclListNode();
};

class InitializerListNode : public AstNode {
public:
    static InitializerListNode* createInitializerList();
    static InitializerListNode* createInitializerList(InitializerNode *initializer);
    static InitializerListNode* addInitializer(InitializerListNode *initList, InitializerNode *initializer);

    list<InitializerNode*>* getInitializerList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<InitializerNode*>* initializers;

    InitializerListNode();
};

class InitializerNode : public AstNode {
public:
    enum InitializerType {
        NONE,
        EXPR,
        ARRAY
    };

    static InitializerNode* createExpr(ExprNode *expr);
    static InitializerNode* createArrayInitializer(InitializerListNode *initList);

    InitializerType getType() const;
    ExprNode* getExpr() const;
    InitializerListNode* getInitializerList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InitializerType type;
    ExprNode *expr;
    InitializerListNode *initList;

    InitializerNode();
};

class DeclaratorNode : public AstNode {
public:
    static DeclaratorNode* createDeclarator(ValueNode *identifier);
    static DeclaratorNode* addArrayAccess(DeclaratorNode *decl, ExprNode *size);

    ValueNode* getIdentifier() const;
    list<ExprNode*>* getArraySizes() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *identifier;
    list<ExprNode*> *arraySizes;

    DeclaratorNode();
};

class InitDeclNode : public AstNode {
public:
    enum InitDeclType {
        NONE,
        DECLARATOR,
        INITIALIZED,
        ARRAY_INITIALIZED
    };

    static InitDeclNode* createDeclarator(DeclaratorNode *declarator);
    static InitDeclNode* createInitialized(DeclaratorNode *declarator, InitializerNode *initializer);
    static InitDeclNode* createArrayInitialized(DeclaratorNode *declarator, InitializerNode *initializer);

    InitDeclType getType() const;
    DeclaratorNode* getDeclarator() const;
    InitializerNode* getInitializer() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InitDeclType type;
    DeclaratorNode *declarator;
    InitializerNode *initializer;

    InitDeclNode();
};

class AccessModifierNode : public AstNode {
public:
    enum AccessModifier {
        NONE,
        PUBLIC,
        PROTECTED,
        PRIVATE
    };

    static AccessModifierNode* createPublic();
    static AccessModifierNode* createProtected();
    static AccessModifierNode* createPrivate();

    AccessModifier getAccessType() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    AccessModifier accessType;

    AccessModifierNode();
};

class InstanceVarDeclNode : public AstNode {
public:
    static InstanceVarDeclNode* createInstanceVarDecl(AccessModifierNode *accessModifier, TypeNode *type, InitDeclNode *initDecl);

    AccessModifierNode* getAccessModifier() const;
    TypeNode* getType() const;
    InitDeclNode* getInitDecl() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    AccessModifierNode *accessModifier;
    TypeNode *type;
    InitDeclNode *initDecl;

    InstanceVarDeclNode();
};

class InstanceVarsDeclListNode : public AstNode {
public:
    static InstanceVarsDeclListNode* createInstanceVarsDeclList(InstanceVarDeclNode *instanceVarDecl);
    static InstanceVarsDeclListNode* addInstanceVarDecl(InstanceVarDeclNode *instanceVarDecl, InstanceVarsDeclListNode *instanceVarsDeclList);

    list<InstanceVarDeclNode*>* getInstanceVarsDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<InstanceVarDeclNode*> *instanceVarDecls;

    InstanceVarsDeclListNode();
};

class InstanceVarsNode : public AstNode {
public:
    static InstanceVarsNode* createInstanceVars();
    static InstanceVarsNode* createInstanceVars(InstanceVarsDeclListNode *instanceVarsDeclList);

    InstanceVarsDeclListNode* getInstanceVarsDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceVarsDeclListNode *instanceVarsDeclList;

    InstanceVarsNode();
};

class InterfaceBodyNode : public AstNode {
public:
    static InterfaceBodyNode* createInterfaceBody(InterfaceDeclListNode *interfaceDeclList);
    static InterfaceBodyNode* createInterfaceBody(InstanceVarsNode *instanceVars, InterfaceDeclListNode *interfaceDeclList);

    InstanceVarsNode* getInstanceVars() const;
    InterfaceDeclListNode* getInterfaceDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceVarsNode *instanceVars;
    InterfaceDeclListNode *interfaceDeclList;

    InterfaceBodyNode();
};

class ImplementationNode : public AstNode {
public:
    static ImplementationNode* createImplementation(ValueNode *className, ImplementationBodyNode *implementationBody);
    static ImplementationNode* createImplementation(ValueNode *className, ValueNode *superClassName, ImplementationBodyNode *implementationBody);

    ValueNode* getClassName() const;
    ValueNode* getSuperClassName() const;
    ImplementationBodyNode* getImplementationBody() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *className;
    ValueNode *superClassName;
    ImplementationBodyNode *implementationBody;

    ImplementationNode();
};

class InterfaceNode : public AstNode {
public:
    static InterfaceNode* createInterface(ValueNode *className, InterfaceBodyNode *interfaceBody);
    static InterfaceNode* createInterface(ValueNode *className, ValueNode *superClassName, InterfaceBodyNode *interfaceBody);

    ValueNode* getClassName() const;
    ValueNode* getSuperClassName() const;
    InterfaceBodyNode* getInterfaceBody() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *className;
    ValueNode *superClassName;
    InterfaceBodyNode *interfaceBody;

    InterfaceNode();
};

class ClassNameListNode : public AstNode {
public:
    static ClassNameListNode* createClassFwDeclList(ValueNode *className);
    static ClassNameListNode* addClassFwDecl(ClassNameListNode *classFwDeclList, ValueNode *className);

    list<ValueNode*>* getClassFwDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ValueNode*> *classFwDeclList;

    ClassNameListNode();
};

class ExternalDeclNode : public AstNode {
public:
    enum ExternalDeclType {
        NONE,
        INTERFACE,
        IMPLEMENTATION,
        CLASS_FW_DECL_LIST,
        FUNC_DECL,
        FUNC_DEF
    };

    static ExternalDeclNode* createInterface(InterfaceNode *interface);
    static ExternalDeclNode* createImplementation(ImplementationNode *implementation);
    static ExternalDeclNode* createFwClassDeclList(ClassNameListNode *classNames);
    static ExternalDeclNode* createFuncDecl(FuncDeclNode *funcDecl);
    static ExternalDeclNode* createFuncDef(FuncDefNode *funcDef);

    ExternalDeclType getType() const;
    InterfaceNode* getInterface() const;
    ImplementationNode* getImplementation() const;
    ClassNameListNode* getClassNameList() const;
    FuncDeclNode* getFuncDecl() const;
    FuncDefNode* getFuncDef() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExternalDeclType type;
    InterfaceNode *interface;
    ImplementationNode *implementation;
    ClassNameListNode *classNames;
    FuncDeclNode *funcDecl;
    FuncDefNode *funcDef;

    ExternalDeclNode();
};

class ExternalDeclListNode : public AstNode {
public:
    static ExternalDeclListNode* createExternalDeclList();
    static ExternalDeclListNode* createExternalDeclList(ExternalDeclNode *externalDecl);
    static ExternalDeclListNode* addExternalDecl(ExternalDeclListNode *externalDeclList, ExternalDeclNode *externalDecl);

    list<ExternalDeclNode*>* getExternalDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ExternalDeclNode*> *externalDeclList;

    ExternalDeclListNode();
};

class ProgramNode : public AstNode {
public:
    static ProgramNode* createProgram(ExternalDeclListNode *externalDeclList);

    ExternalDeclListNode* getExternalDeclList() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExternalDeclListNode *externalDeclList;

    ProgramNode();
};

#endif