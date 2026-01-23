#ifndef CLASSES_H
#define CLASSES_H

#include <iostream>
#include <string>
#include <list>
#include "types.h"

class Type;
class ConstantsTable;
class LocalVariablesTable;
class ClassesTableElement;

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

    ValueKind getValueKind() const;
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

    void setClassName(string className);
    void setLocalVarId(int id);
    int getLocalVarId() const;
    void setIsLocalVar(bool val);
    bool getIsLocalVar() const;

    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueKind valueType;
    int intValue;
    float floatValue;
    bool boolValue;
    char charValue;
    string *stringValue;

    int localVarId;
    bool isLocalVar;

    ValueNode();
};

class ReceiverNode : public AstNode {
public:
    static ReceiverNode* createExpr(ExprNode *expr);
    static ReceiverNode* createClassName(ValueNode *className);
    static ReceiverNode* createSuper();

    ReceiverKind getKind() const;
    ExprNode* getExpr() const;

    void fillLiterals(ConstantsTable* constantTable);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ReceiverKind kind;
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

    void fillLiterals(ConstantsTable* constantTable);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<MsgArgNode*> *msgArgs;

    MsgArgListNode();
};

class MsgSelectorNode : public AstNode {
public:
    static MsgSelectorNode* createSimpleSel(ValueNode *identifier);
    static MsgSelectorNode* createArgumentList(MsgArgListNode *list);

    MsgSelectorKind getKind() const;
    ValueNode* getIdentifier() const;
    MsgArgListNode* getMsgArgList() const;

    void fillLiterals(ConstantsTable* constantTable);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    MsgSelectorKind kind;
    ValueNode *identifier;
    MsgArgListNode *argList;

    MsgSelectorNode();
};

class ExprListNode : public AstNode {
public:
    static ExprListNode* createExprList(ExprNode *expr);
    static ExprListNode* addExprToList(ExprListNode *exprList, ExprNode *expr);

    list<ExprNode*>* getExprList() const;

    void fillLiterals(ConstantsTable* constantTable);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ExprNode*> *exprList;
    ExprListNode();
};

class ExprNode : public AstNode {
public:
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

    void fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement);
    void fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance);
    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);
    void processObjcMessage(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance);
    void checkTypeCompatibility(Type* leftType, Type* rightType, const string& operation);

    ExprKind getKind() const;
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

    void setType(Type* type);
    Type* getExprType() const;
    void setFieldRefConstantId(int id);
    int getFieldRefConstantId() const;
    void setMethodRefConstantId(int id);
    int getMethodRefConstantId() const;
    void setIsFieldAccess(bool val);
    bool getIsFieldAccess() const;
    void setIsMethodCall(bool val);
    bool getIsMethodCall() const;
    void setClassName(const string& name);
    string getClassName() const;

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExprKind kind;
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

    Type* exprType;
    int fieldRefConstantId;
    int methodRefConstantId;
    bool isFieldAccess;
    bool isMethodCall;
    string className;

    ExprNode();
};

class TypeNode : public AstNode {
public:
    static TypeNode* createIntType();
    static TypeNode* createCharType();
    static TypeNode* createFloatType();
    static TypeNode* createBoolType();
    static TypeNode* createIdType();
    static TypeNode* createClassNameType(ValueNode *classNameValue);
    static TypeNode* createVoid();

    TypeKind getKind() const;
    ValueNode* getClassName() const;

    void fillLiterals(ConstantsTable* constantTable);

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

    void fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, TypeNode* typeNode);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement);
    void fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance);
    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<StmtNode*> *stmts;

    StmtListNode();
};

class StmtNode : public AstNode {
public:
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

    StmtKind getKind() const;
    StmtListNode* getCompound() const;

    void fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement);
    void fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance);
    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    StmtKind kind;
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

    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ExprNode*> *sizes;

    ArraySizeSpecNode();
};

class ParamDeclNode : public AstNode {
public:
    static ParamDeclNode* createParamDecl(TypeNode *type, ValueNode *identifier);
    static ParamDeclNode* createArrayParamDecl(TypeNode *type, ValueNode *identifier);
    static ParamDeclNode* createSizedArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec);
    static ParamDeclNode* createFlexibleArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec);

    ParamDeclKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    ArraySizeSpecNode* getSizeSpec() const;

    void fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillTables();
    void semanticTransform();

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

    void fillTables();

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
    static MethodParamNode* createMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier);
    static MethodParamNode* createArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier);
    static MethodParamNode* createSizedArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier);
    static MethodParamNode* createFlexibleArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier);

    MethodParamKind getKind() const;
    ValueNode* getSelectorIdentifier() const;
    TypeNode* getType() const;
    ValueNode* getParamIdentifier() const;
    ArraySizeSpecNode* getArraySizeSpec() const;

    void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillLiterals(ConstantsTable* constantTable);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<MethodParamNode*> *methodParams;

    MethodSelNode();
};

class MethodDefNode : public AstNode {
public:
    static MethodDefNode* createInstanceMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt);
    static MethodDefNode* createInstanceMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt);
    static MethodDefNode* createClassMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt);
    static MethodDefNode* createClassMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt);

    MethodDefKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;
    StmtNode* getCompoundStmt() const;
    bool isInstanceMethod() const;
    bool isClassMethod() const;

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
    void semanticTransform();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    MethodDefKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;
    StmtNode *compoundStmt;
    bool isInstanceMethodFlag;

    MethodDefNode();
};

class ImplementationDefListNode : public AstNode {
public:
    static ImplementationDefListNode* createImplementationDefListWithClassMethod(MethodDefNode *classMethodDef);
    static ImplementationDefListNode* createImplementationDefListWithInstMethod(MethodDefNode *instanceMethodDef);
    static ImplementationDefListNode* addClassMethodDef(ImplementationDefListNode *implementationDefList, MethodDefNode *classMethodDef);
    static ImplementationDefListNode* addInstanceMethodDef(ImplementationDefListNode *implementationDefList, MethodDefNode *instanceMethodDef);

    list<MethodDefNode*>* getClassMethodDefs() const;
    list<MethodDefNode*>* getInstanceMethodDefs() const;

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<MethodDefNode*> *classMethodDefs;
    list<MethodDefNode*> *instanceMethodDefs;
    
    ImplementationDefListNode();
};

class MethodDeclNode : public AstNode {
public:
    static MethodDeclNode* createInstanceMethodDecl(TypeNode *type, ValueNode *identifier);
    static MethodDeclNode* createInstanceMethodDecl(TypeNode *type, MethodSelNode *methodSel);
    static MethodDeclNode* createClassMethodDecl(TypeNode *type, ValueNode *identifier);
    static MethodDeclNode* createClassMethodDecl(TypeNode *type, MethodSelNode *methodSel);

    MethodDeclKind getKind() const;
    TypeNode* getType() const;
    ValueNode* getIdentifier() const;
    MethodSelNode* getMethodSel() const;
    bool isInstanceMethod() const;
    bool isClassMethod() const;
    
    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    MethodDeclKind kind;
    TypeNode *type;
    ValueNode *identifier;
    MethodSelNode *methodSel;
    bool isInstanceMethodFlag;

    MethodDeclNode();
};

class PropertyNode : public AstNode {
public:
    static PropertyNode* createProperty(Attribute attr, TypeNode *type, ValueNode *name);
    static PropertyNode* createProperty(TypeNode *type, ValueNode *name);

    Attribute getAttribute() const;
    TypeNode* getType() const;
    ValueNode* getName() const;

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);

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
    static InterfaceDeclListNode* addClassMethodDecl(InterfaceDeclListNode *interfaceDeclList, MethodDeclNode *classMethodDecl);
    static InterfaceDeclListNode* addInstanceMethodDecl(InterfaceDeclListNode *interfaceDeclList, MethodDeclNode *instanceMethodDecl);

    list<PropertyNode*>* getProperties() const;
    list<MethodDeclNode*>* getClassMethodDecls() const;
    list<MethodDeclNode*>* getInstanceMethodDecls() const;

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<PropertyNode*> *properties;
    list<MethodDeclNode*> *classMethodDecls;
    list<MethodDeclNode*> *instanceMethodDecls;

    InterfaceDeclListNode();
};

class InitializerListNode : public AstNode {
public:
    static InitializerListNode* createInitializerList();
    static InitializerListNode* createInitializerList(InitializerNode *initializer);
    static InitializerListNode* addInitializer(InitializerListNode *initList, InitializerNode *initializer);

    list<InitializerNode*>* getInitializerList() const;

    void fillTables(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<InitializerNode*>* initializers;

    InitializerListNode();
};

class InitializerNode : public AstNode {
public:
    static InitializerNode* createExpr(ExprNode *expr);
    static InitializerNode* createArrayInitializer(InitializerListNode *initList);

    InitializerKind getKind() const;
    ExprNode* getExpr() const;
    InitializerListNode* getInitializerList() const;

    void fillTables(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InitializerKind kind;
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

    void fillTables(ConstantsTable* constantTable);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *identifier;
    list<ExprNode*> *arraySizes;

    DeclaratorNode();
};

class InitDeclNode : public AstNode {
public:
    static InitDeclNode* createDeclarator(DeclaratorNode *declarator);
    static InitDeclNode* createInitialized(DeclaratorNode *declarator, InitializerNode *initializer);
    static InitDeclNode* createArrayInitialized(DeclaratorNode *declarator, InitializerNode *initializer);

    InitDeclKind getKind() const;
    DeclaratorNode* getDeclarator() const;
    InitializerNode* getInitializer() const;

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement, TypeNode* typeNode);
    void semanticTransform(LocalVariablesTable* localVariables, TypeNode* typeNode);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InitDeclKind kind;
    DeclaratorNode *declarator;
    InitializerNode *initializer;

    InitDeclNode();
};

class AccessModifierNode : public AstNode {
public:
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

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
    void semanticTransform(LocalVariablesTable* localVariables);

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

    void fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
    void semanticTransform(LocalVariablesTable* localVariables);

    string getDotLabel() const override;
    string toDot() const override;

protected:
    InstanceVarsDeclListNode *instanceVarsDeclList;

    InstanceVarsNode();
};

class ImplementationNode : public AstNode {
public:
    static ImplementationNode* createImplementation(ValueNode *className, InstanceVarsNode *instanceVars, ImplementationDefListNode *implDefList);
    static ImplementationNode* createImplementation(ValueNode *className, ValueNode *superClassName, InstanceVarsNode *instanceVars, ImplementationDefListNode *implDefList);

    ValueNode* getClassName() const;
    ValueNode* getSuperClassName() const;
    InstanceVarsNode* getInstanceVars() const;
    ImplementationDefListNode* getImplDefList() const;

    void setClassName(string className);
    void setSuperClassName(string superClassName);

    void fillTables();
    void semanticTransform();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *className;
    ValueNode *superClassName;
    InstanceVarsNode *instanceVars;
    ImplementationDefListNode *implDefList;

    ImplementationNode();
};

class InterfaceNode : public AstNode {
public:
    static InterfaceNode* createInterface(ValueNode *className, InstanceVarsNode *instanceVars, InterfaceDeclListNode *interfaceDeclList);
    static InterfaceNode* createInterface(ValueNode *className, ValueNode *superClassName, InstanceVarsNode *instanceVars, InterfaceDeclListNode *interfaceDeclList);

    ValueNode* getClassName() const;
    ValueNode* getSuperClassName() const;
    InstanceVarsNode* getInstanceVars() const;
    InterfaceDeclListNode* getInterfaceDeclList() const;

    void setClassName(string className);
    void setSuperClassName(string superClassName);

    void fillTables();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ValueNode *className;
    ValueNode *superClassName;
    InstanceVarsNode *instanceVars;
    InterfaceDeclListNode *interfaceDeclList;

    InterfaceNode();
};

class ClassNameListNode : public AstNode {
public:
    static ClassNameListNode* createClassFwDeclList(ValueNode *className);
    static ClassNameListNode* addClassFwDecl(ClassNameListNode *classFwDeclList, ValueNode *className);

    list<ValueNode*>* getClassFwDeclList() const;

    void fillTables();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    list<ValueNode*> *classFwDeclList;

    ClassNameListNode();
};

class ExternalDeclNode : public AstNode {
public:
    static ExternalDeclNode* createInterface(InterfaceNode *interface);
    static ExternalDeclNode* createImplementation(ImplementationNode *implementation);
    static ExternalDeclNode* createFwClassDeclList(ClassNameListNode *classNames);
    static ExternalDeclNode* createFuncDecl(FuncDeclNode *funcDecl);
    static ExternalDeclNode* createFuncDef(FuncDefNode *funcDef);

    ExternalDeclKind getKind() const;
    InterfaceNode* getInterface() const;
    ImplementationNode* getImplementation() const;
    ClassNameListNode* getClassNameList() const;
    FuncDeclNode* getFuncDecl() const;
    FuncDefNode* getFuncDef() const;

    void fillTables();
    void semanticTransform();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExternalDeclKind kind;
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

    void fillTables();
    void semanticTransform();

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

    void fillTables();
    void semanticTransform();

    string getDotLabel() const override;
    string toDot() const override;

protected:
    ExternalDeclListNode *externalDeclList;

    ProgramNode();
};

#endif