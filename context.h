#ifndef CONTEXT_H
#define CONTEXT_H

#include <map>
#include <memory>
#include <stack>
#include <unordered_set>
#include "classes.h"
#include "semantic_exceptions.h"

using namespace std;

class FieldInfo;
class MethodInfo;
class LocalVarInfo;
class SemanticContext;

class Type {
public:
    TypeKind dataType = TypeKind::NONE;
    string className = "";
    vector<int> arraySizes;
    int arrayDimension = 0;

    Type(TypeKind type);
    Type(TypeKind type, const string& className);
    Type(TypeKind type, int arraySize);
    Type(TypeKind type, const string& className, int arraySize);
    Type(TypeKind type, const vector<int>& arraySizes);
    Type(TypeKind type, const string& className, const vector<int>& arraySizes);

    string getDescriptor() const;
    bool equal(const Type* other) const;
    bool isCastableTo(const Type* other) const;
    bool isPrimitive() const;
    bool isNumeric() const;
    bool isArray() const;
};

class SymbolInfo {
public:
    enum class SymbolKind {
        CLASS,
        METHOD,
        FIELD,
        VARIABLE,
        FUNCTION
    };
    
    SymbolKind kind;
    string name;
    Type type;
    
    SymbolInfo(SymbolKind kind, const string& name, const Type& type);
    virtual ~SymbolInfo() = default;
    
    SymbolInfo(const SymbolInfo&) = default;
    SymbolInfo& operator=(const SymbolInfo&) = default;
    SymbolInfo(SymbolInfo&&) = default;
    SymbolInfo& operator=(SymbolInfo&&) = default;

    virtual string toString() const;
    bool isKind(SymbolKind k) const;
};

class ClassInfo : public SymbolInfo {
public:
    ClassInfo* superclass = nullptr;
    bool hasInterface = false;
    bool hasImplementation = false;
    map<string, string> propertyIvarMapping;
    InterfaceNode* interface = nullptr;
    ImplementationNode* implementation = nullptr;
    
    map<string, unique_ptr<FieldInfo>> fields;
    map<string, vector<unique_ptr<MethodInfo>>> methods;

    ClassInfo(const string& name, ClassInfo* superclass = nullptr);

    string toString() const override;

    void markAsInterface();
    void markAsImplementation();
    bool isComplete() const;
    
    bool isSubclassOf(const ClassInfo* other) const;

    FieldInfo* lookupField(const string& name, bool includeSuper = true);
    const FieldInfo* lookupField(const string& name, bool includeSuper = true) const;
    MethodInfo* lookupMethod(const string& name, const vector<const Type*>& argTypes = {}, const vector<string>& keywords = {}, bool includeSuper = true);
    const MethodInfo* lookupMethod(const string& name, const vector<const Type*>& argTypes = {}, const vector<string>& keywords = {}, bool includeSuper = true) const;

    void addField(unique_ptr<FieldInfo> field);
    void addMethod(unique_ptr<MethodInfo> method);

    void addPropertyMapping(const string& property, const string& ivar);
    string getIvarForProperty(const string& property) const;
    
    size_t getFieldCount(bool instanceOnly = false) const;
    size_t getMethodCount(bool instanceOnly = false) const;

    void setSuperclass(ClassInfo* superclass);
    void setInterface(InterfaceNode* node);
    void setImplementation(ImplementationNode* node);

    bool hasSuperclass() const;

private:
    void deepCopyFields(map<string, unique_ptr<FieldInfo>>& dest, const map<string, unique_ptr<FieldInfo>>& src) const;
    void deepCopyMethods(map<string, vector<unique_ptr<MethodInfo>>>& dest, const map<string, vector<unique_ptr<MethodInfo>>>& src) const;
};

class MethodInfo : public SymbolInfo {
public:
    ClassInfo* declaringClass = nullptr;
    bool isClassMethod;
    
    vector<unique_ptr<LocalVarInfo>> parameters;
    map<string, unique_ptr<LocalVarInfo>> localVars;
    
    StmtNode* body = nullptr;
    
    string selector; // "setName:withAge:"
    vector<string> keywords; // ["setName", "withAge"]
    vector<const Type*> parameterTypes;

    MethodInfo(const string& name, const Type& returnType, bool isClassMethod, ClassInfo* declaringClass);

    string toString() const override;

    bool matchesSignature(const vector<const Type*>& argTypes, const vector<string>& keywords) const;
    LocalVarInfo* lookupLocalVar(const string& name);
    const LocalVarInfo* lookupLocalVar(const string& name) const;

    void addParameter(unique_ptr<LocalVarInfo> param);
    void addLocalVar(unique_ptr<LocalVarInfo> var);
    
    const Type& getReturnType() const;
    size_t getParameterCount() const;
    const LocalVarInfo* getParameter(size_t index) const;

private:
    void deepCopyParameters(vector<unique_ptr<LocalVarInfo>>& dest, const vector<unique_ptr<LocalVarInfo>>& src) const;
};

class FieldInfo : public SymbolInfo {
public:
    ClassInfo* declaringClass = nullptr;
    bool isInstance;
    ExprNode* initialValue = nullptr;
    Attribute attribute = Attribute::NONE;
    string getterName;
    string setterName;
    AccessModifier accessModifier = AccessModifier::PROTECTED;

    FieldInfo(const string& name, const Type& type, bool isInstance = true, ClassInfo* declaringClass = nullptr, AccessModifier access = AccessModifier::PROTECTED);

    string toString() const override;
    
    bool hasGetter() const;
    bool hasSetter() const;
    bool isClassField() const;
    bool isInstanceField() const;

    void setAccessModifier(AccessModifier access);
};

class LocalVarInfo : public SymbolInfo {
public:
    MethodInfo* enclosingMethod = nullptr;
    bool isParameter;

    LocalVarInfo(const string& name, const Type& type, bool isParameter = false, MethodInfo* enclosingMethod = nullptr);

    string toString() const override;

    bool isLocal() const;
    bool isParam() const;
};

class FunctionInfo : public SymbolInfo {
public:
    vector<unique_ptr<LocalVarInfo>> parameters;
    map<string, unique_ptr<LocalVarInfo>> localVars;
    StmtNode* body = nullptr;

    FunctionInfo(const string& name, const Type& returnType);

    string toString() const override;
    
    LocalVarInfo* lookupLocalVar(const string& name);
    const LocalVarInfo* lookupLocalVar(const string& name) const;
    
    void addParameter(unique_ptr<LocalVarInfo> param);
    void addLocalVar(unique_ptr<LocalVarInfo> var);
    
    const Type& getReturnType() const;
    size_t getParameterCount() const;
    const LocalVarInfo* getParameter(size_t index) const;
    
private:
    void deepCopyParameters(vector<unique_ptr<LocalVarInfo>>& dest, const vector<unique_ptr<LocalVarInfo>>& src) const;
};

class SemanticContext {
private:
    SemanticContext() = default;
    ~SemanticContext() = default;
    SemanticContext(const SemanticContext&) = delete;
    SemanticContext& operator=(const SemanticContext&) = delete;

    map<string, unique_ptr<ClassInfo>> classes;
    map<string, vector<unique_ptr<FunctionInfo>>> functions;
    
    struct Scope {
        enum ScopeKind {
            GLOBAL_SCOPE,
            FUNCTION_SCOPE,
            CLASS_SCOPE,
            METHOD_SCOPE,
            LOOP_SCOPE,
            CONDITIONAL_SCOPE,
            BLOCK_STMT_SCOPE
        };

        string name;
        map<string, unique_ptr<LocalVarInfo>> locals;
        Scope* parent;
        ScopeKind kind;
        bool isActive = true;
        
        Scope(const string& name = "", Scope* parent = nullptr, ScopeKind kind = GLOBAL_SCOPE);
        LocalVarInfo* lookup(const string& name);
        const LocalVarInfo* lookup(const string& name) const;
    };
    
    stack<Scope*> activeScopes;
    vector<unique_ptr<Scope>> usedScopes;
    Scope* currentScope = nullptr;
    ClassInfo* currentClass = nullptr;
    MethodInfo* currentMethod = nullptr;
    FunctionInfo* currentFunction = nullptr;
    
    unordered_set<string> reservedNames;
    map<string, string> inheritanceHierarchy;
    
public:
    static SemanticContext& getInstance();

    bool addClass(unique_ptr<ClassInfo> cls);
    bool addMethod(const string& className, unique_ptr<MethodInfo> method);
    bool addField(const string& className, unique_ptr<FieldInfo> field);
    bool addFunction(unique_ptr<FunctionInfo> func);
    bool addLocalVar(unique_ptr<LocalVarInfo> var);
    bool addParameter(MethodInfo* method, unique_ptr<LocalVarInfo> param);
    bool addParameter(FunctionInfo* func, unique_ptr<LocalVarInfo> param);

    void enterScope(Scope::ScopeKind kind = Scope::GLOBAL_SCOPE, const string& name = "global");
    void leaveScope();
    void enterClassScope(ClassInfo* cls);
    void enterMethodScope(MethodInfo* method);
    void enterFunctionScope(FunctionInfo* func);
    void enterLoopScope();
    void enterConditionalScope();
    void enterBlockStmtScope();
    
    SymbolInfo* lookup(const string& name) const;
    ClassInfo* lookupClass(const string& name) const;
    MethodInfo* lookupMethod(const string& className, const string& methodName, const vector<const Type*>& argTypes = {}, const vector<string>& keywords = {}) const;
    FieldInfo* lookupField(const string& className, const string& fieldName) const;
    LocalVarInfo* lookupLocalVar(const string& name) const;
    FunctionInfo* lookupFunction(const string& name) const;

    bool existsInCurrentScope(const string& name) const;
    bool existsInParentScopes(const string& name) const;
    vector<LocalVarInfo*> getVisibleLocalVars() const;

    bool isReservedName(const string& name) const;
    
    bool isAssignable(const Type& from, const Type& to) const;
    bool isConvertible(const Type& from, const Type& to) const;
    unique_ptr<Type> commonType(const Type& t1, const Type& t2) const;
    
    bool validateInheritance() const;
    bool checkCyclicInheritance() const;
    bool checkDuplicateSymbols() const;
    
    void setCurrentClass(ClassInfo* cls);
    void setCurrentMethod(MethodInfo* method);
    void setCurrentFunction(FunctionInfo* func);
    
    ClassInfo* getCurrentClass() const;
    MethodInfo* getCurrentMethod() const;
    FunctionInfo* getCurrentFunction() const;
    
    Scope* getCurrentScope() const;
    bool isInGlobalScope() const;
    
    string generateGetterName(const string& fieldName) const;
    string generateSetterName(const string& fieldName) const;
    string mangleMethodName(const string& selector, const vector<string>& keywords) const;
    string demangleMethodName(const string& mangledName) const;
    
    void dumpSymbolTable() const;
    void dumpClassHierarchy() const;
    void dumpCurrentScope() const;

    void initSemanticContext();
    
private:
    Scope* createScope(const string& name, Scope::ScopeKind kind, Scope* parent = nullptr);
    void activateScope(Scope* scope);
    void deactivateCurrentScope();

    bool checkCyclicInheritance(const string& className, unordered_set<string>& visited) const;
    void resolveInheritance();
    
    void initReservedNames();
    void initNSObjectClass();
    void initNSStringClass();
    void initNSArrayClass();
    void initNSNumberClass();
    void initInOutFuncsClass();
};

#endif