#include <vector>
#include <map>
#include <stack>
#include <unordered_set>
#include <optional>
#include "classes.h"

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
    
    virtual unique_ptr<SymbolInfo> clone() const;
    
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
    
    map<string, unique_ptr<FieldInfo>> fields;
    map<string, vector<unique_ptr<MethodInfo>>> methods;

    ClassInfo(const string& name, ClassInfo* superclass = nullptr);

    unique_ptr<SymbolInfo> clone() const override;
    string toString() const override;
    
    bool isSubclassOf(const ClassInfo* other) const;

    FieldInfo* lookupField(const string& name, bool includeSuper = true);
    const FieldInfo* lookupField(const string& name, bool includeSuper = true) const;
    MethodInfo* lookupMethod(const string& name, const vector<const Type*>& argTypes = {}, bool includeSuper = true);
    const MethodInfo* lookupMethod(const string& name, const vector<const Type*>& argTypes = {}, bool includeSuper = true) const;

    void addField(unique_ptr<FieldInfo> field);
    void addMethod(unique_ptr<MethodInfo> method);
    
    size_t getFieldCount(bool instanceOnly = false) const;
    size_t getMethodCount(bool instanceOnly = false) const;

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

    MethodInfo::MethodInfo(const string& name, const Type& returnType, bool isClassMethod, ClassInfo* declaringClass);

    unique_ptr<SymbolInfo> clone() const override;
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

    FieldInfo(const string& name, const Type& type, bool isInstance = true, ClassInfo* declaringClass = nullptr);

    unique_ptr<SymbolInfo> clone() const override;
    string toString() const override;
    
    bool hasGetter() const;
    bool hasSetter() const;
    bool isClassField() const;
    bool isInstanceField() const;
};

class LocalVarInfo : public SymbolInfo {
public:
    MethodInfo* enclosingMethod = nullptr;
    bool isParameter;

    LocalVarInfo(const string& name, const Type& type, bool isParameter = false, MethodInfo* enclosingMethod = nullptr);

    unique_ptr<SymbolInfo> clone() const override;
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

    unique_ptr<SymbolInfo> clone() const override;
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

        map<string, unique_ptr<LocalVarInfo>> locals;
        Scope* parent;
        ScopeKind kind;
        
        Scope(Scope* parent = nullptr, ScopeKind kind = GLOBAL_SCOPE);
        LocalVarInfo* lookup(const string& name);
        const LocalVarInfo* lookup(const string& name) const;
    };
    
    stack<unique_ptr<Scope>> scopes;
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

    void enterScope(Scope::ScopeKind kind = Scope::BLOCK_STMT_SCOPE);
    void leaveScope();
    void enterClassScope(ClassInfo* cls);
    void enterMethodScope(MethodInfo* method);
    void enterFunctionScope(FunctionInfo* func);
    void SemanticContext::enterLoopScope();
    void SemanticContext::enterConditionalScope();
    void SemanticContext::enterBlockStmtScope();
    
    SymbolInfo* lookup(const string& name) const;
    ClassInfo* lookupClass(const string& name) const;
    MethodInfo* lookupMethod(const string& className, const string& methodName, const vector<const Type*>& argTypes = {}) const;
    FieldInfo* lookupField(const string& className, const string& fieldName) const;
    LocalVarInfo* lookupLocalVar(const string& name) const;
    FunctionInfo* lookupFunction(const string& name) const;
    
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
    
    bool isInClassScope() const;
    bool isInMethodScope() const;
    bool isInFunctionScope() const;
    bool isGlobalScope() const;
    
    string generateGetterName(const string& fieldName) const;
    string generateSetterName(const string& fieldName) const;
    string mangleMethodName(const string& selector, const vector<string>& keywords) const;
    string demangleMethodName(const string& mangledName) const;
    
    void dumpSymbolTable() const;
    void dumpClassHierarchy() const;
    void dumpCurrentScope() const;
    
private:
    bool checkCyclicInheritance(const string& className, unordered_set<string>& visited) const;
    void resolveInheritance();
    
    optional<Scope*> currentScope();
    
    void initReservedNames();
};