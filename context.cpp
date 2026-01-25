#include "context.h"
#include <iostream>
#include <algorithm>
#include <functional>

//--------------------------------------------------------------Type--------------------------------------------------------------

Type::Type(TypeKind type)
    : dataType(type), className(""), arraySizes(), arrayDimension(0) {}

Type::Type(TypeKind type, const string& className)
    : dataType(type), className(className), arraySizes(), arrayDimension(0) {}

Type::Type(TypeKind type, int arraySize)
    : dataType(type), className(""), arraySizes({arraySize}), arrayDimension(1) {}

Type::Type(TypeKind type, const string& className, int arraySize)
    : dataType(type), className(className), arraySizes({arraySize}), arrayDimension(1) {}

Type::Type(TypeKind type, const vector<int>& arraySizes)
    : dataType(type), className(""), arraySizes(arraySizes), arrayDimension(arraySizes.size()) {}

Type::Type(TypeKind type, const string& className, const vector<int>& arraySizes)
    : dataType(type), className(className), arraySizes(arraySizes), arrayDimension(arraySizes.size()) {}

string Type::getDescriptor() const {
    string res = "";
    
    if (isArray()) {
        for (int i = 0; i < arrayDimension; i++) {
            res += "[";
        }
    }
    
    switch (dataType) {
        case TypeKind::INT:         res += "I"; break;
        case TypeKind::FLOAT:       res += "F"; break;
        case TypeKind::BOOL:        res += "Z"; break;
        case TypeKind::CHAR:        res += "C"; break; //TODO: change to "B" to support implicit int to char casting ???
        case TypeKind::TYPE_ID:     res += "Ljava/lang/Object;"; break;
        case TypeKind::CLASS_NAME:  res += "L" + className + ";"; break;
        case TypeKind::VOID:        res += "V"; break;
        default:                    break;
    }
    
    return res;
}

bool Type::equal(const Type* other) const {
    if (other) {
        if (this->dataType == other->dataType && this->className == other->className
            && this->arrayDimension == other->arrayDimension && this->arraySizes == other->arraySizes) {
            return true;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

bool Type::isCastableTo(const Type* other) const {
    if (!(this->arrayDimension == other->arrayDimension) && !(this->arraySizes == other->arraySizes)) {
        return false;
    }
    if (this->dataType == other->dataType) {
        if (this->dataType == TypeKind::CLASS_NAME && this->className == other->className) {
            return true;
        }
        else {
            return false;
        }
        return true;
    }
    if (this->dataType == TypeKind::INT && other->dataType == TypeKind::CHAR
        || this->dataType == TypeKind::CHAR && other->dataType == TypeKind::INT) {
        return true;
    }
    if (this->dataType == TypeKind::TYPE_ID && other->dataType == TypeKind::CLASS_NAME
        || this->dataType == TypeKind::CLASS_NAME && other->dataType == TypeKind::TYPE_ID) {
        return true;
    }
    if (this->dataType == TypeKind::FLOAT && other->dataType == TypeKind::INT
        || this->dataType == TypeKind::INT && other->dataType == TypeKind::FLOAT) {
        return true;
    }
    if (this->dataType == TypeKind::BOOL && other->dataType == TypeKind::FLOAT
        || this->dataType == TypeKind::FLOAT && other->dataType == TypeKind::BOOL) {
        return true;
    }
    if (this->dataType == TypeKind::BOOL && other->dataType == TypeKind::INT
        || this->dataType == TypeKind::INT && other->dataType == TypeKind::BOOL) {
        return true;
    }
    if (this->dataType == TypeKind::CHAR && other->dataType == TypeKind::FLOAT
        || this->dataType == TypeKind::FLOAT && other->dataType == TypeKind::CHAR) {
        return true;
    }
    if (this->dataType == TypeKind::BOOL && other->dataType == TypeKind::CHAR
        || this->dataType == TypeKind::CHAR && other->dataType == TypeKind::BOOL) {
        return true;
    }
    if (dataType == TypeKind::CLASS_NAME &&
        other->dataType == TypeKind::CLASS_NAME) {

        auto& ctx = SemanticContext::getInstance();

        ClassInfo* thisClass  = ctx.lookupClass(className);
        ClassInfo* otherClass = ctx.lookupClass(other->className);

        if (!thisClass || !otherClass) {
            return false;
        }

        return thisClass->isSubclassOf(otherClass) ||
               otherClass->isSubclassOf(thisClass);
    }
    
    return false; //TODO: сделать проверку nil
}

bool Type::isPrimitive() const {
    return  this->dataType == TypeKind::INT ||
            this->dataType == TypeKind::FLOAT ||
            this->dataType == TypeKind::CHAR ||
            this->dataType == TypeKind::BOOL ||
            this->dataType == TypeKind::VOID;
}

bool Type::isNumeric() const {
    return this->dataType == TypeKind::INT || this->dataType == TypeKind::FLOAT || this->dataType == TypeKind::BOOL;
}

bool Type::isArray() const {
    return !arraySizes.empty() && arrayDimension > 0;
}

string Type::toString() const { // TODO: добавить отображение размерности массивов
    switch (dataType) {
        case TypeKind::INT:         return "int";
        case TypeKind::FLOAT:       return "float";
        case TypeKind::BOOL:        return "bool";
        case TypeKind::CHAR:        return "char";
        case TypeKind::TYPE_ID:     return "id";
        case TypeKind::CLASS_NAME:  return className;
        case TypeKind::VOID:        return "void";
        default:                    return "undefined type";
    }
}

//--------------------------------------------------------------SymbolInfo--------------------------------------------------------------

SymbolInfo::SymbolInfo(SymbolKind kind, const string& name, const Type& type)
    : kind(kind), name(name), type(type) {}

string SymbolInfo::toString() const {
    const char* kindStr = "";
    switch(kind) {
        case SymbolKind::CLASS: kindStr = "CLASS"; break;
        case SymbolKind::METHOD: kindStr = "METHOD"; break;
        case SymbolKind::FIELD: kindStr = "FIELD"; break;
        case SymbolKind::VARIABLE: kindStr = "VARIABLE"; break;
        case SymbolKind::FUNCTION: kindStr = "FUNCTION"; break;
    }
    return string(kindStr) + " " + name + " : " + type.getDescriptor();
}

bool SymbolInfo::isKind(SymbolKind k) const {
    return kind == k;
}

//--------------------------------------------------------------LocalVarInfo--------------------------------------------------------------

LocalVarInfo::LocalVarInfo(const string& name, const Type& type, bool isParameter, MethodInfo* enclosingMethod)
    : SymbolInfo(SymbolKind::VARIABLE, name, type),
      enclosingMethod(enclosingMethod),
      isParameter(isParameter) {}

string LocalVarInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (" + string(isParameter ? "parameter" : "local") + ")";
    return base;
}

Type LocalVarInfo::getType() const {
    return type;
}

bool LocalVarInfo::isLocal() const {
    return !isParameter;
}

bool LocalVarInfo::isParam() const {
    return isParameter;
}

//--------------------------------------------------------------FieldInfo--------------------------------------------------------------

FieldInfo::FieldInfo(const string& name, const Type& type, bool isInstance, ClassInfo* declaringClass, AccessModifier access)
    : SymbolInfo(SymbolKind::FIELD, name, type),
      declaringClass(declaringClass),
      isInstance(isInstance),
      accessModifier(access) {}

string FieldInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (" + string(isInstance ? "instance" : "class") + ")";
    if (hasGetter()) {
        base += " [getter: " + getterName + "]";
    }
    if (hasSetter()) {
        base += " [setter: " + setterName + "]";
    }
    return base;
}

Type FieldInfo::getType() const {
    return type;
}

bool FieldInfo::hasGetter() const {
    return !getterName.empty();
}

bool FieldInfo::hasSetter() const {
    return !setterName.empty();
}

bool FieldInfo::isClassField() const {
    return !isInstance;
}

bool FieldInfo::isInstanceField() const {
    return isInstance;
}

void FieldInfo::setAccessModifier(AccessModifier access) {
    accessModifier = access;
}

//--------------------------------------------------------------MethodInfo--------------------------------------------------------------

MethodInfo::MethodInfo(const string& name, const Type& returnType, bool isClassMethod, ClassInfo* declaringClass)
    : SymbolInfo(SymbolKind::METHOD, name, returnType),
      declaringClass(declaringClass),
      isClassMethod(isClassMethod),
      accessModifier(AccessModifier::PUBLIC) {}

string MethodInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (" + string(isClassMethod ? "class" : "instance") + " method)";
    base += " params: " + to_string(parameters.size());
    return base;
}

bool MethodInfo::matchesSignature(const vector<const Type*>& argTypes, const vector<string>& keywords) const {
    if (argTypes.size() != parameterTypes.size()) {
        return false;
    }
    
    if (keywords.size() != this->keywords.size()) {
        return false;
    }
    
    for (size_t i = 0; i < keywords.size(); i++) {
        if (!keywords[i].empty() && !this->keywords[i].empty() && keywords[i] != this->keywords[i]) {
            return false;
        }
    }
    
    // Проверяем типы параметров
    for (size_t i = 0; i < argTypes.size(); i++) {
        if (parameterTypes[i] && argTypes[i]) {
            if (!parameterTypes[i]->equal(argTypes[i])) {
                return false;
            }
        } else {
            // Один из типов отсутствует
            return false;
        }
    }
    
    return true;
}

LocalVarInfo* MethodInfo::lookupLocalVar(const string& name) {
    auto it = localVars.find(name);
    if (it != localVars.end()) {
        return it->second.get();
    }
    
    for (const auto& param : parameters) {
        if (param->name == name) {
            return param.get();
        }
    }
    
    return nullptr;
}

const LocalVarInfo* MethodInfo::lookupLocalVar(const string& name) const {
    auto it = localVars.find(name);
    if (it != localVars.end()) {
        return it->second.get();
    }
    
    for (const auto& param : parameters) {
        if (param->name == name) {
            return param.get();
        }
    }
    
    return nullptr;
}

void MethodInfo::addParameter(unique_ptr<LocalVarInfo> param) {
    if (param) {
        param->isParameter = true;
        param->enclosingMethod = this;
        parameters.push_back(move(param));
    }
}

void MethodInfo::addLocalVar(unique_ptr<LocalVarInfo> var) {
    if (var) {
        var->isParameter = false;
        var->enclosingMethod = this;
        localVars[var->name] = move(var);
    }
}

const Type& MethodInfo::getReturnType() const {
    return type;
}

size_t MethodInfo::getParameterCount() const {
    return parameters.size();
}

const LocalVarInfo* MethodInfo::getParameter(size_t index) const {
    if (index < parameters.size()) {
        return parameters[index].get();
    }
    return nullptr;
}

//--------------------------------------------------------------ClassInfo--------------------------------------------------------------

ClassInfo::ClassInfo(const string& name, ClassInfo* superclass)
    : SymbolInfo(SymbolKind::CLASS, name, Type(TypeKind::CLASS_NAME, name)),
      superclass(superclass) {}

string ClassInfo::toString() const {
    string base = SymbolInfo::toString();
    if (superclass) {
        base += " extends " + superclass->name;
    }
    base += " (fields: " + to_string(fields.size()) + ", methods: " + to_string(getMethodCount()) + ")";
    return base;
}

void ClassInfo::markAsInterface() {
    hasInterface = true;
}

void ClassInfo::markAsImplementation() {
    hasImplementation = true;
}

bool ClassInfo::isComplete() const {
    if (name.find("java/lang/") == 0 || name.find("rtl/") == 0) {
        return true;
    }
    return hasInterface && hasImplementation;
}

bool ClassInfo::isSubclassOf(const ClassInfo* other) const {
    if (!other) return false;
    for (const ClassInfo* current = superclass; current; current = current->superclass) {
        if (current == other) {
            return true;
        }
    }
    return false;
}

FieldInfo* ClassInfo::lookupField(const string& name, bool includeSuper) {
    auto it = fields.find(name);
    if (it != fields.end()) {
        return it->second.get();
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupField(name, true);
    }
    
    return nullptr;
}

const FieldInfo* ClassInfo::lookupField(const string& name, bool includeSuper) const {
    auto it = fields.find(name);
    if (it != fields.end()) {
        return it->second.get();
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupField(name, true);
    }
    
    return nullptr;
}

MethodInfo* ClassInfo::lookupMethod(const string& name, const vector<const Type*>& argTypes, const vector<string>& keywords, bool includeSuper, bool isClassMethod) {
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (auto& method : it->second) {
            if (method->isClassMethod != isClassMethod) {
                continue;
            }
            if (method->matchesSignature(argTypes, keywords)) {
                return method.get();
            }
        }
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupMethod(name, argTypes, keywords, true, isClassMethod);
    }
    
    return nullptr;
}

const MethodInfo* ClassInfo::lookupMethod(const string& name, const vector<const Type*>& argTypes, const vector<string>& keywords, bool includeSuper, bool isClassMethod) const {
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (auto& method : it->second) {
            if (method->isClassMethod != isClassMethod) {
                continue;
            }
            if (method->matchesSignature(argTypes, keywords)) {
                return method.get();
            }
        }
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupMethod(name, argTypes, keywords, true, isClassMethod);
    }
    
    return nullptr;
}

void ClassInfo::addField(unique_ptr<FieldInfo> field) {
    if (field) {
        field->declaringClass = this;
        fields[field->name] = move(field);
    }
}

void ClassInfo::addMethod(unique_ptr<MethodInfo> method) {
    if (!method) return;
    
    auto& methodList = methods[method->name];
    for (auto& existingMethod : methodList) {
        if (existingMethod->isClassMethod != method->isClassMethod) {
            continue;
        }
        if (existingMethod->keywords == method->keywords &&
            !existingMethod->matchesSignature(method->parameterTypes, method->keywords)) {
            throw semantic_exception("Method overloading is not supported for selector '" + method->name + "'",
                "ClassInfo::addMethod", -1, -1);
        }
        if (existingMethod->matchesSignature(method->parameterTypes, method->keywords)) {
            throw semantic_exception("Method with same signature already exists: " + method->name,
                "ClassInfo::addMethod", -1, -1);
        }
    }
    
    if (superclass) {
        auto superIt = superclass->methods.find(method->name);
        if (superIt != superclass->methods.end()) {
            for (const auto& superMethod : superIt->second) {
                if (superMethod->isClassMethod != method->isClassMethod) {
                    continue;
                }
                if (superMethod->keywords == method->keywords &&
                    !superMethod->matchesSignature(method->parameterTypes, method->keywords)) {
                    throw semantic_exception("Method '" + method->name + "' overrides with a different signature",
                        "ClassInfo::addMethod", -1, -1);
                }
            }
        }
        MethodInfo* superMethod = superclass->lookupMethod(method->name, method->parameterTypes, method->keywords, true, method->isClassMethod);
        if (superMethod) {
            if (!method->getReturnType().equal(&superMethod->getReturnType())) {
                throw semantic_exception("Method '" + method->name + "' return type mismatch with overridden method",
                    "ClassInfo::addMethod", -1, -1,
                    "Overridden: " + superMethod->getReturnType().toString() + 
                    ", New: " + method->getReturnType().toString());
            }
        }
    }
    
    methodList.push_back(move(method));
}

void ClassInfo::addPropertyMapping(const string& property, const string& ivar) {
    propertyIvarMapping[property] = ivar;
}

string ClassInfo::getIvarForProperty(const string& property) const {
    auto it = propertyIvarMapping.find(property);
    return it != propertyIvarMapping.end() ? it->second : "_" + property;
}

size_t ClassInfo::getFieldCount(bool instanceOnly) const {
    if (!instanceOnly) {
        return fields.size();
    }
    
    size_t count = 0;
    for (const auto& [name, field] : fields) {
        if (field->isInstanceField()) {
            ++count;
        }
    }
    return count;
}

size_t ClassInfo::getMethodCount(bool instanceOnly) const {
    size_t count = 0;
    for (const auto& [name, methodList] : methods) {
        for (const auto& method : methodList) {
            if (!instanceOnly || !method->isClassMethod) {
                ++count;
            }
        }
    }
    return count;
}

void ClassInfo::setSuperclass(ClassInfo* superclass) {
    this->superclass = superclass;
}

void ClassInfo::setInterface(InterfaceNode* node) {
    if (node) {
        interface = node;
    }
}

void ClassInfo::setImplementation(ImplementationNode* node) {
    if (node) {
        implementation = node;
    }
}

bool ClassInfo::hasSuperclass() const {
    return superclass != nullptr;
}

//--------------------------------------------------------------FunctionInfo--------------------------------------------------------------

FunctionInfo::FunctionInfo(const string& name, const Type& returnType)
    : SymbolInfo(SymbolKind::FUNCTION, name, returnType) {}

string FunctionInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (function, params: " + to_string(parameters.size()) + ")";
    return base;
}

LocalVarInfo* FunctionInfo::lookupLocalVar(const string& name) {
    auto it = localVars.find(name);
    if (it != localVars.end()) {
        return it->second.get();
    }
    
    for (const auto& param : parameters) {
        if (param->name == name) {
            return param.get();
        }
    }
    return nullptr;
}

const LocalVarInfo* FunctionInfo::lookupLocalVar(const string& name) const {
    auto it = localVars.find(name);
    if (it != localVars.end()) {
        return it->second.get();
    }
    
    for (const auto& param : parameters) {
        if (param->name == name) {
            return param.get();
        }
    }
    return nullptr;
}

void FunctionInfo::addParameter(unique_ptr<LocalVarInfo> param) {
    if (param) {
        param->isParameter = true;
        param->enclosingMethod = nullptr;
        parameters.push_back(move(param));
    }
}

void FunctionInfo::addLocalVar(unique_ptr<LocalVarInfo> var) {
    if (var) {
        var->isParameter = false;
        var->enclosingMethod = nullptr;
        localVars[var->name] = move(var);
    }
}

const Type& FunctionInfo::getReturnType() const {
    return type;
}

size_t FunctionInfo::getParameterCount() const {
    return parameters.size();
}

const LocalVarInfo* FunctionInfo::getParameter(size_t index) const {
    if (index < parameters.size()) {
        return parameters[index].get();
    }
    return nullptr;
}

//--------------------------------------------------------------SemanticContext--------------------------------------------------------------

SemanticContext::Scope::Scope(const string& name, Scope* parent, ScopeKind kind)
    : name(name), parent(parent), kind(kind) {}

LocalVarInfo* SemanticContext::Scope::lookup(const string& name) {
    auto it = locals.find(name);
    if (it != locals.end()) return it->second.get();
    return parent ? parent->lookup(name) : nullptr;
}

const LocalVarInfo* SemanticContext::Scope::lookup(const string& name) const {
    auto it = locals.find(name);
    if (it != locals.end()) return it->second.get();
    return parent ? parent->lookup(name) : nullptr;
}

SemanticContext& SemanticContext::getInstance() {
    static SemanticContext instance;
    return instance;
}

bool SemanticContext::addClass(unique_ptr<ClassInfo> cls) {
    if (!cls || cls->name.empty()) return false;
    
    if (classes.find(cls->name) != classes.end()) {
        return false;  // Класс уже существует
    }
    
    classes[cls->name] = move(cls);
    return true;
}

bool SemanticContext::addMethod(const string& className, unique_ptr<MethodInfo> method) {
    if (!method) return false;
    
    auto it = classes.find(className);
    if (it == classes.end()) return false;
    
    it->second->addMethod(move(method));
    return true;
}

bool SemanticContext::addField(const string& className, unique_ptr<FieldInfo> field) {
    if (!field) return false;
    
    auto it = classes.find(className);
    if (it == classes.end()) return false;
    
    it->second->addField(move(field));
    return true;
}

bool SemanticContext::addFunction(unique_ptr<FunctionInfo> func) {
    if (!func || func->name.empty()) return false;
    
    functions[func->name].push_back(move(func));
    return true;
}

bool SemanticContext::addLocalVar(unique_ptr<LocalVarInfo> var) {
    if (!var || var->name.empty() || !currentScope) {
        return false;
    }
    
    if (currentScope->locals.find(var->name) != currentScope->locals.end()) {
        return false;
    }
    
    currentScope->locals[var->name] = move(var);
    return true;
}

bool SemanticContext::addParameter(MethodInfo* method, unique_ptr<LocalVarInfo> param) {
    if (!method || !param) return false;
    
    method->addParameter(move(param));
    return true;
}

bool SemanticContext::addParameter(FunctionInfo* func, unique_ptr<LocalVarInfo> param) {
    if (!func || !param) return false;
    
    func->addParameter(move(param));
    return true;
}

void SemanticContext::enterScope(Scope::ScopeKind kind, const string& name) {
    Scope* parent = currentScope;
    Scope* newScope = createScope(name, kind, parent);
    activateScope(newScope);
}

void SemanticContext::leaveScope() {
    if (!activeScopes.empty()) {
        deactivateCurrentScope();
    }
}

void SemanticContext::enterClassScope(ClassInfo* cls) {
    setCurrentClass(cls);
    enterScope(Scope::CLASS_SCOPE, cls ? cls->name : "anonymous_class");
}

void SemanticContext::enterMethodScope(MethodInfo* method) {
    setCurrentMethod(method);
    string scopeName = method ? method->name : "anonymous_method";
    if (currentClass) {
        scopeName = currentClass->name + "::" + scopeName;
    }
    enterScope(Scope::METHOD_SCOPE, scopeName);
}

void SemanticContext::enterFunctionScope(FunctionInfo* func) {
    setCurrentFunction(func);
    string scopeName = func ? func->name : "anonymous_function";
    enterScope(Scope::FUNCTION_SCOPE, scopeName);
}

void SemanticContext::enterLoopScope() {
    static int loopCounter = 0;
    string name = "loop_" + to_string(loopCounter++);
    enterScope(Scope::LOOP_SCOPE, name);
}

void SemanticContext::enterConditionalScope() {
    static int conditionalCounter = 0;
    string name = "conditional_" + to_string(conditionalCounter++);
    enterScope(Scope::CONDITIONAL_SCOPE, name);
}

void SemanticContext::enterBlockStmtScope() {
    static int blockStmtCounter = 0;
    string name = "block_stmt_" + to_string(blockStmtCounter++);
    enterScope(Scope::BLOCK_STMT_SCOPE, name);
}

SymbolInfo* SemanticContext::lookup(const string& name) const {
    if (auto var = lookupLocalVar(name)) {
        return var;
    }
    
    if (currentClass) {
        if (auto field = currentClass->lookupField(name, true)) {
            return field;
        }
    }
    
    if (auto cls = lookupClass(name)) {
        return cls;
    }
    
    if (auto func = lookupFunction(name)) {
        return func;
    }
    
    return nullptr;
}

ClassInfo* SemanticContext::lookupClass(const string& name) const {
    vector<string> searchPaths = {
        name,
        "java/lang/" + name,
        "rtl/" + name
    };

    for (const auto& searchName : searchPaths) {
        auto it = classes.find(searchName);
        if (it != classes.end()) {
            return it->second.get();
        }
    }

    return nullptr;
}

MethodInfo* SemanticContext::lookupMethod(const string& className, const string& methodName, const vector<const Type*>& argTypes, 
                                            const vector<string>& keywords, bool isClassMethod) const {
    auto cls = lookupClass(className);
    if (!cls) return nullptr;

    return cls->lookupMethod(methodName, argTypes, keywords, true, isClassMethod);
}

FieldInfo* SemanticContext::lookupField(const string& className, const string& fieldName) const {
    auto cls = lookupClass(className);
    if (!cls) return nullptr;
    
    return cls->lookupField(fieldName, true);
}

LocalVarInfo* SemanticContext::lookupLocalVar(const string& name) const {
    for (Scope* scope = currentScope; scope != nullptr; scope = scope->parent) {
        if (!scope->isActive) continue;
        
        auto it = scope->locals.find(name);
        if (it != scope->locals.end()) {
            return it->second.get();
        }
        
        if (scope->kind == Scope::FUNCTION_SCOPE && currentFunction) {
            for (const auto& param : currentFunction->parameters) {
                if (param->name == name) {
                    return param.get();
                }
            }
        } else if (scope->kind == Scope::METHOD_SCOPE && currentMethod) {
            for (const auto& param : currentMethod->parameters) {
                if (param->name == name) {
                    return param.get();
                }
            }
        }
    }
    
    return nullptr;
}

FunctionInfo* SemanticContext::lookupFunction(const string& name) const {
    auto it = functions.find(name);
    if (it == functions.end() || it->second.empty()) {
        return nullptr;
    }
    
    // Возвращаем первую функцию с таким именем
    // В реальности нужно учитывать перегрузку
    return it->second.front().get();
}

bool SemanticContext::existsInCurrentScope(const string& name) const {
    if (!currentScope) return false;
    return currentScope->locals.find(name) != currentScope->locals.end();
}

// Проверка, существует ли переменная в ЛЮБОМ родительском scope
bool SemanticContext::existsInParentScopes(const string& name) const {
    if (!currentScope) return false;
    
    for (Scope* parent = currentScope->parent; parent != nullptr; parent = parent->parent) {
        if (!parent->isActive) continue;
        
        if (parent->locals.find(name) != parent->locals.end()) {
            return true;
        }
        
        if (parent->kind == Scope::FUNCTION_SCOPE && currentFunction) {
            for (const auto& param : currentFunction->parameters) {
                if (param->name == name) {
                    return true;
                }
            }
        } else if (parent->kind == Scope::METHOD_SCOPE && currentMethod) {
            for (const auto& param : currentMethod->parameters) {
                if (param->name == name) {
                    return true;
                }
            }
        }
    }
    
    return false;
}

vector<LocalVarInfo*> SemanticContext::getVisibleLocalVars() const {
    vector<LocalVarInfo*> result;
    unordered_set<string> seenNames;
    
    if (currentMethod) {
        for (const auto& param : currentMethod->parameters) {
            result.push_back(param.get());
            seenNames.insert(param->name);
        }
    } else if (currentFunction) {
        for (const auto& param : currentFunction->parameters) {
            result.push_back(param.get());
            seenNames.insert(param->name);
        }
    }
    
    for (Scope* scope = currentScope; scope != nullptr; scope = scope->parent) {
        if (!scope->isActive) continue;
        
        for (const auto& [name, var] : scope->locals) {
            if (seenNames.find(name) == seenNames.end()) {
                result.push_back(var.get());
                seenNames.insert(name);
            }
        }
    }
    
    return result;
}

bool SemanticContext::isReservedName(const string& name) const {
    return reservedNames.find(name) != reservedNames.end();
}

bool SemanticContext::isAssignable(const Type& from, const Type& to) const { //TODO: проверить касты массивов и nil
    if (from.equal(&to)) return true;
    
    // NONE подразумевает nil
    if (from.dataType == TypeKind::NONE && 
        (to.dataType == TypeKind::CLASS_NAME)) {
        return true;
    }
    
    // автоматические числовые преобразования (расширяющие)
    if (from.isNumeric() && to.isNumeric()) {
        return true;
    }

    if (from.dataType == TypeKind::INT && to.dataType == TypeKind::CHAR 
        || from.dataType == TypeKind::CHAR && to.dataType == TypeKind::INT) {
        return true;
    }
    if (from.dataType == TypeKind::FLOAT && to.dataType == TypeKind::CHAR 
        || from.dataType == TypeKind::CHAR && to.dataType == TypeKind::FLOAT) {
        return true;
    }
    if (from.dataType == TypeKind::BOOL && to.dataType == TypeKind::CHAR 
        || from.dataType == TypeKind::CHAR && to.dataType == TypeKind::BOOL) {
        return true;
    }
    
    if (from.dataType == TypeKind::CLASS_NAME && to.dataType == TypeKind::CLASS_NAME) {
        auto fromClass = lookupClass(from.className);
        auto toClass = lookupClass(to.className);
        
        if (fromClass && toClass) {
            return fromClass->isSubclassOf(toClass); // Только вверх по иерархии
        }
    }
    
    if (from.isArray() && to.isArray()) {
        if (from.arrayDimension != to.arrayDimension) return false;
        
        // Для массивов - ковариантность (только вверх по иерархии)
        Type fromElem(from.dataType, from.className);
        Type toElem(to.dataType, to.className);
        return isAssignable(fromElem, toElem);
    }
    
    return false;
}

bool SemanticContext::isConvertible(const Type& from, const Type& to) const { //TODO: делать проверки каста массивов[] в тип элемента
    if (isAssignable(from, to)) return true;
    
    if (from.isNumeric() && to.isNumeric()) {
        return true;
    }
    
    // Дополнительные явные преобразования:
    
    // char <-> int (уже в числовых, но для ясности)
    if ((from.dataType == TypeKind::CHAR && to.dataType == TypeKind::INT) ||
        (from.dataType == TypeKind::INT && to.dataType == TypeKind::CHAR)) {
        return true;
    }
    
    // TYPE_ID <-> CLASS_NAME
    if ((from.dataType == TypeKind::TYPE_ID && to.dataType == TypeKind::CLASS_NAME) ||
        (from.dataType == TypeKind::CLASS_NAME && to.dataType == TypeKind::TYPE_ID)) {
        return true;
    }
    
    // bool <-> char
    if ((from.dataType == TypeKind::BOOL && to.dataType == TypeKind::CHAR) ||
        (from.dataType == TypeKind::CHAR && to.dataType == TypeKind::BOOL)) {
        return true;
    }
    
    // char -> float (уже в числовых, но для ясности)
    if (from.dataType == TypeKind::CHAR && to.dataType == TypeKind::FLOAT) {
        return true;
    }
    
    // char[] из String или char (для строковых преобразований)
    if (from.dataType == TypeKind::CHAR && to.isArray() && 
        to.dataType == TypeKind::CHAR) {
        return true;
    }
    
    // Явные преобразования по иерархии классов (вверх и вниз)
    if (from.dataType == TypeKind::CLASS_NAME && 
        to.dataType == TypeKind::CLASS_NAME) {
        auto fromClass = lookupClass(from.className);
        auto toClass = lookupClass(to.className);
        
        if (fromClass && toClass) {
            // Явное преобразование разрешено в обе стороны
            return fromClass->isSubclassOf(toClass) || 
                   toClass->isSubclassOf(fromClass) ||
                   fromClass == toClass;
        }
    }
    
    // Явные преобразования массивов
    if (from.isArray() && to.isArray()) {
        if (from.arrayDimension != to.arrayDimension) return false;
        
        Type fromElem(from.dataType, from.className);
        Type toElem(to.dataType, to.className);
        
        // для массивов при явном преобразовании разрешены более широкие преобразования
        return isConvertible(fromElem, toElem);
    }
    
    // NONE подразумевает nil
    if (from.dataType == TypeKind::NONE && 
        (to.dataType == TypeKind::CLASS_NAME)) {
        return true;
    }
    
    return false;
}

unique_ptr<Type> SemanticContext::commonType(const Type& t1, const Type& t2) const {
    // 1. Если типы одинаковы
    if (t1.equal(&t2)) {
        return make_unique<Type>(t1);
    }
    
    // 2. Числовые типы
    if (t1.isNumeric() && t2.isNumeric()) {
        // float имеет приоритет над int
        if (t1.dataType == TypeKind::FLOAT || t2.dataType == TypeKind::FLOAT) {
            return make_unique<Type>(TypeKind::FLOAT);
        }
        return make_unique<Type>(TypeKind::INT);
    }
    
    // 3. Наследование
    if (t1.dataType == TypeKind::CLASS_NAME && t2.dataType == TypeKind::CLASS_NAME) {
        auto cls1 = lookupClass(t1.className);
        auto cls2 = lookupClass(t2.className);
        
        if (cls1 && cls2) {
            // Ищем общего предка
            for (ClassInfo* p1 = cls1; p1; p1 = p1->superclass) {
                for (ClassInfo* p2 = cls2; p2; p2 = p2->superclass) {
                    if (p1 == p2) {
                        return make_unique<Type>(TypeKind::CLASS_NAME, p1->name);
                    }
                }
            }
        }
        
        // Общий предок - Object
        return make_unique<Type>(TypeKind::CLASS_NAME, "Object");
    }
    
    // 4. Массивы
    if (t1.isArray() && t2.isArray() && 
        t1.arrayDimension == t2.arrayDimension) {
        auto elemType = commonType(
            Type(t1.dataType, t1.className),
            Type(t2.dataType, t2.className)
        );
        if (elemType) {
            return make_unique<Type>(
                elemType->dataType, 
                elemType->className,
                t1.arraySizes  // Размеры из первого массива
            );
        }
    }
    
    // 5. Несовместимы
    return nullptr;
}

bool SemanticContext::validateInheritance() const {
    for (const auto& [name, cls] : classes) {
        if (cls->superclass) {
            auto super = lookupClass(cls->superclass->name);
            if (!super) {
                cerr << "Class " << name << " inherits from undefined class " 
                         << cls->superclass->name << endl;
                return false;
            }
        }
    }
    return true;
}

bool SemanticContext::checkCyclicInheritance() const {
    unordered_set<string> visited;
    
    for (const auto& [name, cls] : classes) {
        if (checkCyclicInheritance(name, visited)) {
            cerr << "Cyclic inheritance with class " 
                     << name << endl;
            return false;
        }
        visited.clear();
    }
    return true;
}

SemanticContext::Scope* SemanticContext::createScope(
    const string& name, 
    Scope::ScopeKind kind, 
    Scope* parent) {
    
    auto scope = make_unique<Scope>(name, parent, kind);
    Scope* scopePtr = scope.get();
    usedScopes.push_back(move(scope));
    return scopePtr;
}

void SemanticContext::activateScope(Scope* scope) {
    if (!scope) return;
    
    scope->isActive = true;
    activeScopes.push(scope);
    currentScope = scope;
}

void SemanticContext::deactivateCurrentScope() {
    if (activeScopes.empty()) return;
    
    Scope* top = activeScopes.top();
    top->isActive = false;
    activeScopes.pop();
    
    currentScope = activeScopes.empty() ? nullptr : activeScopes.top();
}

bool SemanticContext::checkCyclicInheritance(const string& className, unordered_set<string>& visited) const {
    if (visited.find(className) != visited.end()) {
        return true;  // Цикл обнаружен
    }
    
    visited.insert(className);
    
    auto cls = lookupClass(className);
    if (!cls || !cls->superclass) {
        return false;
    }
    
    return checkCyclicInheritance(cls->superclass->name, visited);
}

bool SemanticContext::checkDuplicateSymbols() const {
    bool hasErrors = false;
    
    // Проверяем дубликаты классов
    unordered_set<string> seen;
    for (const auto& [name, cls] : classes) {
        if (seen.find(name) != seen.end()) {
            cerr << "Дублирующийся класс: " << name << endl;
            hasErrors = true;
        }
        seen.insert(name);
    }
    
    // Проверяем дубликаты полей в каждом классе
    for (const auto& [name, cls] : classes) {
        unordered_set<string> fieldNames;
        
        for (const auto& [fieldName, field] : cls->fields) {
            if (fieldNames.find(fieldName) != fieldNames.end()) {
                cerr << "Дублирующееся поле " << fieldName 
                         << " в классе " << name << endl;
                hasErrors = true;
            }
            fieldNames.insert(fieldName);
        }
    }
    
    return !hasErrors;
}

void SemanticContext::setCurrentClass(ClassInfo* cls) {
    currentClass = cls;
    if (cls) {
        currentMethod = nullptr;
        currentFunction = nullptr;
    }
}

void SemanticContext::setCurrentMethod(MethodInfo* method) {
    currentMethod = method;
    if (method) {
        currentFunction = nullptr;
    }
}

void SemanticContext::setCurrentFunction(FunctionInfo* func) {
    currentFunction = func;
    if (func) {
        currentMethod = nullptr;
    }
}

ClassInfo* SemanticContext::getCurrentClass() const {
    return currentClass;
}

MethodInfo* SemanticContext::getCurrentMethod() const {
    return currentMethod;
}

FunctionInfo* SemanticContext::getCurrentFunction() const {
    return currentFunction;
}

SemanticContext::Scope* SemanticContext::getCurrentScope() const {
    return currentScope;
}

bool SemanticContext::isInGlobalScope() const {
    return currentScope->kind == Scope::GLOBAL_SCOPE;
}

string SemanticContext::generateGetterName(const string& fieldName) const {
    // Преобразуем "fieldName" в "getFieldName"
    string getter = "get";
    if (!fieldName.empty()) {
        getter += static_cast<char>(toupper(fieldName[0]));
        getter += fieldName.substr(1);
    }
    return getter;
}

string SemanticContext::generateSetterName(const string& fieldName) const {
    // Преобразуем "fieldName" в "setFieldName"
    string setter = "set";
    if (!fieldName.empty()) {
        setter += static_cast<char>(toupper(fieldName[0]));
        setter += fieldName.substr(1);
    }
    return setter;
}

string SemanticContext::mangleMethodName(const string& selector, const vector<string>& keywords) const {
    string mangled = selector;
    for (const auto& keyword : keywords) {
        mangled += "$" + keyword;
    }
    return mangled;
}

string SemanticContext::demangleMethodName(const string& mangledName) const {
    // Простой деманглинг - убираем разделители $
    string result;
    for (char c : mangledName) {
        if (c != '$') result += c;
    }
    return result;
}

void SemanticContext::dumpSymbolTable() const {
    cout << "\n=== Symbol Table ===" << endl;
    
    for (const auto& [name, cls] : classes) {
        cout << "\nClass: " << name;
        if (cls->superclass) {
            cout << " extends " << cls->superclass->name;
        }
        
        cout << "\n  Fields (" << cls->getFieldCount() << "):" << endl;
        for (const auto& [fieldName, field] : cls->fields) {
            cout << "    " << field->toString() << endl;
        }
        
        cout << "  Methods (" << cls->getMethodCount() << "):" << endl;
        for (const auto& [methodName, methodList] : cls->methods) {
            for (const auto& method : methodList) {
                cout << "    " << method->toString() << endl;
            }
        }
    }
    
    cout << "\nFunctions:" << endl;
    for (const auto& [name, funcList] : functions) {
        for (const auto& func : funcList) {
            cout << "  " << func->toString() << endl;
        }
    }
}

void SemanticContext::dumpClassHierarchy() const {
    cout << "\n=== Class Hierarchy ===" << endl;
    
    // Находим корневые классы (без суперкласса)
    vector<ClassInfo*> roots;
    for (const auto& [name, cls] : classes) {
        if (!cls->superclass) {
            roots.push_back(cls.get());
        }
    }
    
    // Рекурсивно выводим иерархию
    function<void(ClassInfo*, int)> printClass = [&](ClassInfo* cls, int depth) {
        string indent(depth * 2, ' ');
        cout << indent << cls->name;
        
        if (!cls->fields.empty() || !cls->methods.empty()) {
            cout << " [";
            bool first = true;
            if (!cls->fields.empty()) {
                cout << cls->getFieldCount() << " fields";
                first = false;
            }
            if (!cls->methods.empty()) {
                if (!first) cout << ", ";
                cout << cls->getMethodCount() << " methods";
            }
            cout << "]";
        }
        cout << endl;
        
        // Находим наследников
        for (const auto& [name, child] : classes) {
            if (child->superclass == cls) {
                printClass(child.get(), depth + 1);
            }
        }
    };
    
    for (auto root : roots) {
        printClass(root, 0);
    }
}

void SemanticContext::dumpCurrentScope() const {
    if (!currentScope) {
        cout << "No active scope" << endl;
        return;
    }
    
    cout << "\n=== Current Scope ===" << endl;
    cout << "Scope name: " << (currentScope->name.empty() ? "unnamed" : currentScope->name) << endl;
    cout << "Scope kind: ";
    switch(currentScope->kind) {
        case Scope::GLOBAL_SCOPE: cout << "GLOBAL"; break;
        case Scope::CLASS_SCOPE: cout << "CLASS"; break;
        case Scope::METHOD_SCOPE: cout << "METHOD"; break;
        case Scope::FUNCTION_SCOPE: cout << "FUNCTION"; break;
        case Scope::LOOP_SCOPE: cout << "LOOP"; break;
        case Scope::CONDITIONAL_SCOPE: cout << "CONDITIONAL"; break;
        case Scope::BLOCK_STMT_SCOPE: cout << "BLOCK"; break;
    }
    cout << endl;
    
    cout << "Local variables (" << currentScope->locals.size() << "):" << endl;
    for (const auto& [name, var] : currentScope->locals) {
        bool shadows = existsInParentScopes(name);
        cout << "  " << var->toString();
        if (shadows) {
            cout << " [shadows parent variable]";
        }
        cout << endl;
    }
}

void SemanticContext::initSemanticContext() {
    initReservedNames();
    initNSObjectClass();
    initNSStringClass();
    initNSArrayClass();
    initNSNumberClass();
    initInOutFuncsClass();
    resolveInheritance();
    enterScope();
}

void SemanticContext::resolveInheritance() {
    for (auto& [name, cls] : classes) {
        if (cls->superclass && cls->superclass->name != name) {
            ClassInfo* realSuperclass = lookupClass(cls->superclass->name);
            if (realSuperclass) {
                cls->setSuperclass(realSuperclass);
            } else {
                cerr << "Warning: Superclass '" << cls->superclass->name 
                     << "' not found for class '" << name << "'" << endl;
            }
        }
    }
}

void SemanticContext::initReservedNames() {
    reservedNames = {
        "break", "case", "char", "const", "continue", "default", 
        "do", "double", "else", "enum", "float", "for", "goto", 
        "if", "int", "long", "return", "short", "switch", "void", 
        "while"
    };
    
    reservedNames.insert({
        "id", "SEL", "IMP", "BOOL", "YES", "NO",
        "nil", "Nil", "NULL", "self", "super"
    });
}

void SemanticContext::initNSObjectClass() { // TODO: пересмотреть набор методов и полей
    if (lookupClass("rtl/NSObject")) {
        return;
    }
    
    auto nsObjectClass = make_unique<ClassInfo>("rtl/NSObject", nullptr);
    nsObjectClass->markAsImplementation();
    
    // Конструктор <init> ()V
    {
        auto constructor = make_unique<MethodInfo>(
            "<init>",
            Type(TypeKind::VOID),
            false,
            nsObjectClass.get()
        );
        constructor->selector = "<init>";
        constructor->keywords = {};
        constructor->parameterTypes = {};
        nsObjectClass->addMethod(move(constructor));
    }
    
    // alloc ()Lrtl/NSObject;
    {
        auto allocStatic = make_unique<MethodInfo>(
            "alloc",
            Type(TypeKind::CLASS_NAME, "rtl/NSObject"),
            true,
            nsObjectClass.get()
        );
        allocStatic->selector = "alloc";
        allocStatic->keywords = {};
        allocStatic->parameterTypes = {};
        nsObjectClass->addMethod(move(allocStatic));
    }
    
    // init ()Lrtl/NSObject;
    {
        auto initDynamic = make_unique<MethodInfo>(
            "init",
            Type(TypeKind::CLASS_NAME, "rtl/NSObject"),
            false,
            nsObjectClass.get()
        );
        initDynamic->selector = "init";
        initDynamic->keywords = {};
        initDynamic->parameterTypes = {};
        nsObjectClass->addMethod(move(initDynamic));
    }
    
    // new ()Lrtl/NSObject;
    {
        auto newStatic = make_unique<MethodInfo>(
            "new",
            Type(TypeKind::CLASS_NAME, "rtl/NSObject"),
            true,
            nsObjectClass.get()
        );
        newStatic->selector = "new";
        newStatic->keywords = {};
        newStatic->parameterTypes = {};
        nsObjectClass->addMethod(move(newStatic));
    }
    
    // getClassDynamic ()Ljava/lang/Class;
    {
        auto getClassDynamic = make_unique<MethodInfo>(
            "getClassDynamic",
            Type(TypeKind::CLASS_NAME, "java/lang/Class"),
            false,
            nsObjectClass.get()
        );
        getClassDynamic->selector = "getClassDynamic";
        getClassDynamic->keywords = {};
        getClassDynamic->parameterTypes = {};
        nsObjectClass->addMethod(move(getClassDynamic));
    }
    
    // getClassStatic ()Ljava/lang/Class;
    {
        auto getClassStatic = make_unique<MethodInfo>(
            "getClassStatic",
            Type(TypeKind::CLASS_NAME, "java/lang/Class"),
            true,
            nsObjectClass.get()
        );
        getClassStatic->selector = "getClassStatic";
        getClassStatic->keywords = {};
        getClassStatic->parameterTypes = {};
        nsObjectClass->addMethod(move(getClassStatic));
    }
    
    // isSubclassOfClass (Ljava/lang/Class;)I
    {
        auto isSubclassOfClassStatic = make_unique<MethodInfo>(
            "isSubclassOfClass",
            Type(TypeKind::INT),
            true,
            nsObjectClass.get()
        );
        isSubclassOfClassStatic->selector = "isSubclassOfClass";
        isSubclassOfClassStatic->keywords = {""};
        isSubclassOfClassStatic->parameterTypes = {
            new Type(TypeKind::CLASS_NAME, "java/lang/Class")
        };
        
        auto param = make_unique<LocalVarInfo>(
            "arg0",
            Type(TypeKind::CLASS_NAME, "java/lang/Class"),
            true,
            isSubclassOfClassStatic.get()
        );
        isSubclassOfClassStatic->addParameter(move(param));
        
        nsObjectClass->addMethod(move(isSubclassOfClassStatic));
    }
    
    // className ()Lrtl/NSString;
    {
        auto classNameDynamic = make_unique<MethodInfo>(
            "className",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsObjectClass.get()
        );
        classNameDynamic->selector = "className";
        classNameDynamic->keywords = {};
        classNameDynamic->parameterTypes = {};
        nsObjectClass->addMethod(move(classNameDynamic));
    }
    
    // superclass ()Ljava/lang/Class;
    {
        auto superclassDynamic = make_unique<MethodInfo>(
            "superclass",
            Type(TypeKind::CLASS_NAME, "java/lang/Class"),
            false,
            nsObjectClass.get()
        );
        superclassDynamic->selector = "superclass";
        superclassDynamic->keywords = {};
        superclassDynamic->parameterTypes = {};
        nsObjectClass->addMethod(move(superclassDynamic));
    }
    
    // descriptionDynamic ()Lrtl/NSString;
    {
        auto descriptionDynamic = make_unique<MethodInfo>(
            "description",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsObjectClass.get()
        );
        descriptionDynamic->selector = "description";
        descriptionDynamic->keywords = {};
        descriptionDynamic->parameterTypes = {};
        nsObjectClass->addMethod(move(descriptionDynamic));
    }
    
    // isEqualDynamic (Lrtl/NSObject;)I
    {
        auto isEqualDynamic = make_unique<MethodInfo>(
            "isEqual",
            Type(TypeKind::INT),
            false,
            nsObjectClass.get()
        );
        isEqualDynamic->selector = "isEqual";
        isEqualDynamic->keywords = {""};
        isEqualDynamic->parameterTypes = {
            new Type(TypeKind::CLASS_NAME, "rtl/NSObject")
        };
        
        auto param = make_unique<LocalVarInfo>(
            "arg0",
            Type(TypeKind::CLASS_NAME, "rtl/NSObject"),
            true,
            isEqualDynamic.get()
        );
        isEqualDynamic->addParameter(move(param));
        
        nsObjectClass->addMethod(move(isEqualDynamic));
    }
    
    addClass(move(nsObjectClass));
    
    if (!lookupClass("java/lang/Object")) {
        auto javaObjectClass = make_unique<ClassInfo>("java/lang/Object", nullptr);
        javaObjectClass->markAsImplementation();
        addClass(move(javaObjectClass));
    }
    
    auto nsObject = lookupClass("rtl/NSObject");
    auto javaObject = lookupClass("java/lang/Object");
    if (nsObject && javaObject) {
        nsObject->setSuperclass(javaObject);
    }
}

void SemanticContext::initNSStringClass() {
    if (lookupClass("rtl/NSString")) {
        return;
    }

    auto nsStringClass = make_unique<ClassInfo>("rtl/NSString", nullptr);
    nsStringClass->markAsImplementation();

    // ===============================
    // Статические методы
    // ===============================

    // + (id)string
    {
        auto method = make_unique<MethodInfo>(
            "string",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            true,
            nsStringClass.get()
        );
        method->selector = "string";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // + (id)stringWithCString:(const char*)cstr
    {
        auto method = make_unique<MethodInfo>(
            "stringWithCString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            true,
            nsStringClass.get()
        );
        method->selector = "stringWithCString";
        method->keywords = {""};
        method->parameterTypes = { new Type(TypeKind::CHAR, "", 1) }; // char[]
        nsStringClass->addMethod(move(method));
    }

    // + (id)stringWithString:(NSString*)str
    {
        auto method = make_unique<MethodInfo>(
            "stringWithString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            true,
            nsStringClass.get()
        );
        method->selector = "stringWithString";
        method->keywords = {""};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
        nsStringClass->addMethod(move(method));
    }

    // ===============================
    // Динамические методы
    // ===============================

    // - (const char*)cString
    {
        auto method = make_unique<MethodInfo>(
            "cString",
            Type(TypeKind::CHAR, "", 1),
            false,
            nsStringClass.get()
        );
        method->selector = "cString";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (NSString*)capitalizeString
    {
        auto method = make_unique<MethodInfo>(
            "capitalizeString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsStringClass.get()
        );
        method->selector = "capitalizeString";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (int)length
    {
        auto method = make_unique<MethodInfo>(
            "length",
            Type(TypeKind::INT),
            false,
            nsStringClass.get()
        );
        method->selector = "length";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (NSString*)uppercaseString
    {
        auto method = make_unique<MethodInfo>(
            "uppercaseString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsStringClass.get()
        );
        method->selector = "uppercaseString";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (NSString*)lowercaseString
    {
        auto method = make_unique<MethodInfo>(
            "lowercaseString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsStringClass.get()
        );
        method->selector = "lowercaseString";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (int)isEqual:(NSObject*)other
    {
        auto method = make_unique<MethodInfo>(
            "isEqual",
            Type(TypeKind::INT),
            false,
            nsStringClass.get()
        );
        method->selector = "isEqual";
        method->keywords = {""};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
        nsStringClass->addMethod(move(method));
    }

    // - (NSString*)description
    {
        auto method = make_unique<MethodInfo>(
            "description",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsStringClass.get()
        );
        method->selector = "description";
        method->keywords = {};
        method->parameterTypes = {};
        nsStringClass->addMethod(move(method));
    }

    // - (NSString*)stringByAppendingString:(NSString*)other
    {
        auto method = make_unique<MethodInfo>(
            "stringByAppendingString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsStringClass.get()
        );
        method->selector = "stringByAppendingString";
        method->keywords = {""};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
        nsStringClass->addMethod(move(method));
    }

    addClass(move(nsStringClass));

    auto nsString = lookupClass("rtl/NSString");
    auto nsObject = lookupClass("rtl/NSObject");
    if (nsString && nsObject) {
        nsString->setSuperclass(nsObject);
    }
}

void SemanticContext::initNSArrayClass() {
    if (lookupClass("rtl/NSArray")) return;

    auto nsArrayClass = make_unique<ClassInfo>("rtl/NSArray", nullptr);
    nsArrayClass->markAsImplementation();

    {
        auto constructor = make_unique<MethodInfo>(
            "<init>",
            Type(TypeKind::VOID),
            false,
            nsArrayClass.get()
        );
        constructor->selector = "<init>";
        constructor->keywords = {};
        constructor->parameterTypes = {};
        nsArrayClass->addMethod(move(constructor));
    }

    // array ()Lrtl/NSArray;
    {
        auto arrayStatic = make_unique<MethodInfo>(
            "array",
            Type(TypeKind::CLASS_NAME, "rtl/NSArray"),
            true,
            nsArrayClass.get()
        );
        arrayStatic->selector = "array";
        arrayStatic->keywords = {};
        arrayStatic->parameterTypes = {};
        nsArrayClass->addMethod(move(arrayStatic));
    }

    // arrayWithArray (Lrtl/NSArray;)Lrtl/NSArray;
    {
        auto arrayWithArrayStatic = make_unique<MethodInfo>(
            "arrayWithArray",
            Type(TypeKind::CLASS_NAME, "rtl/NSArray"),
            true,
            nsArrayClass.get()
        );
        arrayWithArrayStatic->selector = "arrayWithArray";
        arrayWithArrayStatic->keywords = {""};
        arrayWithArrayStatic->parameterTypes = {
            new Type(TypeKind::CLASS_NAME, "rtl/NSArray")
        };
        nsArrayClass->addMethod(move(arrayWithArrayStatic));
    }

    // arrayWithObjects ([Lrtl/NSObject;)Lrtl/NSArray;
    {
        auto arrayWithObjectsStatic = make_unique<MethodInfo>(
            "arrayWithObjects",
            Type(TypeKind::CLASS_NAME, "rtl/NSArray"),
            true,
            nsArrayClass.get()
        );
        arrayWithObjectsStatic->selector = "arrayWithObjects";
        arrayWithObjectsStatic->keywords = {""};
        arrayWithObjectsStatic->parameterTypes = {
            new Type(TypeKind::CLASS_NAME, "Lrtl/NSObject;") // массив объектов
        };
        nsArrayClass->addMethod(move(arrayWithObjectsStatic));
    }

    // arrayByAddingObject (Lrtl/NSObject;)Lrtl/NSArray;
    {
        auto arrayByAddingObjectDynamic = make_unique<MethodInfo>(
            "arrayByAddingObject",
            Type(TypeKind::CLASS_NAME, "rtl/NSArray"),
            false,
            nsArrayClass.get()
        );
        arrayByAddingObjectDynamic->selector = "arrayByAddingObject";
        arrayByAddingObjectDynamic->keywords = {""};
        arrayByAddingObjectDynamic->parameterTypes = {
            new Type(TypeKind::CLASS_NAME, "rtl/NSObject")
        };
        nsArrayClass->addMethod(move(arrayByAddingObjectDynamic));
    }

    // objectAtIndex (I)Lrtl/NSObject;
    {
        auto objectAtIndexDynamic = make_unique<MethodInfo>(
            "objectAtIndex",
            Type(TypeKind::CLASS_NAME, "rtl/NSObject"),
            false,
            nsArrayClass.get()
        );
        objectAtIndexDynamic->selector = "objectAtIndex";
        objectAtIndexDynamic->keywords = {""};
        objectAtIndexDynamic->parameterTypes = { new Type(TypeKind::INT) };
        nsArrayClass->addMethod(move(objectAtIndexDynamic));
    }

    // count ()I
    {
        auto countDynamic = make_unique<MethodInfo>(
            "count",
            Type(TypeKind::INT),
            false,
            nsArrayClass.get()
        );
        countDynamic->selector = "count";
        countDynamic->keywords = {};
        countDynamic->parameterTypes = {};
        nsArrayClass->addMethod(move(countDynamic));
    }

    addClass(move(nsArrayClass));

    auto nsArray = lookupClass("rtl/NSArray");
    auto nsObject = lookupClass("rtl/NSObject");
    if (nsArray && nsObject) {
        nsArray->setSuperclass(nsObject);
    }
}

void SemanticContext::initNSNumberClass() {
    if (lookupClass("rtl/NSNumber")) return;

    auto nsNumberClass = make_unique<ClassInfo>("rtl/NSNumber", nullptr);
    nsNumberClass->markAsImplementation();

    {
        auto constructor = make_unique<MethodInfo>(
            "<init>",
            Type(TypeKind::VOID),
            false,
            nsNumberClass.get()
        );
        constructor->selector = "<init>";
        constructor->keywords = {};
        constructor->parameterTypes = {};
        nsNumberClass->addMethod(move(constructor));
    }

    // numberWithInt (I)Lrtl/NSNumber;
    {
        auto numberWithIntStatic = make_unique<MethodInfo>(
            "numberWithInt",
            Type(TypeKind::CLASS_NAME, "rtl/NSNumber"),
            true,
            nsNumberClass.get()
        );
        numberWithIntStatic->selector = "numberWithInt";
        numberWithIntStatic->keywords = {};
        numberWithIntStatic->parameterTypes = { new Type(TypeKind::INT) };
        nsNumberClass->addMethod(move(numberWithIntStatic));
    }

    // numberWithFloatStatic (F)Lrtl/NSNumber;
    {
        auto numberWithFloatStatic = make_unique<MethodInfo>(
            "numberWithFloat",
            Type(TypeKind::CLASS_NAME, "rtl/NSNumber"),
            true,
            nsNumberClass.get()
        );
        numberWithFloatStatic->selector = "numberWithFloat";
        numberWithFloatStatic->keywords = {};
        numberWithFloatStatic->parameterTypes = { new Type(TypeKind::FLOAT) };
        nsNumberClass->addMethod(move(numberWithFloatStatic));
    }

    // intValue ()I
    {
        auto intValueDynamic = make_unique<MethodInfo>(
            "intValue",
            Type(TypeKind::INT),
            false,
            nsNumberClass.get()
        );
        intValueDynamic->selector = "intValue";
        intValueDynamic->keywords = {};
        intValueDynamic->parameterTypes = {};
        nsNumberClass->addMethod(move(intValueDynamic));
    }

    // floatValueDynamic ()F
    {
        auto floatValueDynamic = make_unique<MethodInfo>(
            "floatValue",
            Type(TypeKind::FLOAT),
            false,
            nsNumberClass.get()
        );
        floatValueDynamic->selector = "floatValue";
        floatValueDynamic->keywords = {};
        floatValueDynamic->parameterTypes = {};
        nsNumberClass->addMethod(move(floatValueDynamic));
    }

    // descriptionDynamic ()Lrtl/NSString;
    {
        auto descriptionDynamic = make_unique<MethodInfo>(
            "description",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            false,
            nsNumberClass.get()
        );
        descriptionDynamic->selector = "description";
        descriptionDynamic->keywords = {};
        descriptionDynamic->parameterTypes = {};
        nsNumberClass->addMethod(move(descriptionDynamic));
    }

    addClass(move(nsNumberClass));

    auto nsNumber = lookupClass("rtl/NSNumber");
    auto nsObject = lookupClass("rtl/NSObject");
    if (nsNumber && nsObject) {
        nsNumber->setSuperclass(nsObject);
    }
}

void SemanticContext::initInOutFuncsClass() {
    if (lookupClass("rtl/InOutFuncs")) return;

    auto ioClass = make_unique<ClassInfo>("rtl/InOutFuncs", nullptr);
    ioClass->markAsImplementation();

    // ===============================
    // Print Methods
    // ===============================

    // +printInt:(int)value
    {
        auto method = make_unique<MethodInfo>(
            "printInt",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printInt:";
        method->keywords = {"printInt"};
        method->parameterTypes = { new Type(TypeKind::INT) };
        ioClass->addMethod(move(method));
    }

    // +printFloat:(float)value
    {
        auto method = make_unique<MethodInfo>(
            "printFloat",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printFloat:";
        method->keywords = {"printFloat"};
        method->parameterTypes = { new Type(TypeKind::FLOAT) };
        ioClass->addMethod(move(method));
    }

    // +printChar:(char)value
    {
        auto method = make_unique<MethodInfo>(
            "printChar",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printChar:";
        method->keywords = {"printChar"};
        method->parameterTypes = { new Type(TypeKind::CHAR) };
        ioClass->addMethod(move(method));
    }

    // +printNSString:(NSString*)str
    {
        auto method = make_unique<MethodInfo>(
            "printNSString",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printNSString:";
        method->keywords = {"printNSString"};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
        ioClass->addMethod(move(method));
    }

    // +printNSNumber:(NSNumber*)num
    {
        auto method = make_unique<MethodInfo>(
            "printNSNumber",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printNSNumber:";
        method->keywords = {"printNSNumber"};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSNumber") };
        ioClass->addMethod(move(method));
    }

    // +printNSArray:(NSArray*)arr
    {
        auto method = make_unique<MethodInfo>(
            "printNSArray",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printNSArray:";
        method->keywords = {"printNSArray"};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSArray") };
        ioClass->addMethod(move(method));
    }

    // +printNSObject:(NSObject*)obj
    {
        auto method = make_unique<MethodInfo>(
            "printNSObject",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printNSObject:";
        method->keywords = {"printNSObject"};
        method->parameterTypes = { new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
        ioClass->addMethod(move(method));
    }

    // +printInt:withInt:
    {
        auto method = make_unique<MethodInfo>(
            "printInt:withInt",
            Type(TypeKind::VOID),
            true,
            ioClass.get()
        );
        method->selector = "printInt:withInt:";
        method->keywords = {"printInt", "withInt"};
        method->parameterTypes = { new Type(TypeKind::INT), new Type(TypeKind::INT) };
        ioClass->addMethod(move(method));
    }

    // ===============================
    // Read Methods
    // ===============================

    // +readInt
    {
        auto method = make_unique<MethodInfo>(
            "readInt",
            Type(TypeKind::INT),
            true,
            ioClass.get()
        );
        method->selector = "readInt";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readFloat
    {
        auto method = make_unique<MethodInfo>(
            "readFloat",
            Type(TypeKind::FLOAT),
            true,
            ioClass.get()
        );
        method->selector = "readFloat";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readChar
    {
        auto method = make_unique<MethodInfo>(
            "readChar",
            Type(TypeKind::CHAR),
            true,
            ioClass.get()
        );
        method->selector = "readChar";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readNSString
    {
        auto method = make_unique<MethodInfo>(
            "readNSString",
            Type(TypeKind::CLASS_NAME, "rtl/NSString"),
            true,
            ioClass.get()
        );
        method->selector = "readNSString";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readNSNumberInt
    {
        auto method = make_unique<MethodInfo>(
            "readNSNumberInt",
            Type(TypeKind::CLASS_NAME, "rtl/NSNumber"),
            true,
            ioClass.get()
        );
        method->selector = "readNSNumberInt";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readNSNumberFloat
    {
        auto method = make_unique<MethodInfo>(
            "readNSNumberFloat",
            Type(TypeKind::CLASS_NAME, "rtl/NSNumber"),
            true,
            ioClass.get()
        );
        method->selector = "readNSNumberFloat";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    // +readNSArray
    {
        auto method = make_unique<MethodInfo>(
            "readNSArray",
            Type(TypeKind::CLASS_NAME, "rtl/NSArray"),
            true,
            ioClass.get()
        );
        method->selector = "readNSArray";
        method->keywords = {};
        method->parameterTypes = {};
        ioClass->addMethod(move(method));
    }

    addClass(move(ioClass));

    // ===============================
    // Установка суперкласса
    // ===============================
    auto inOut = lookupClass("rtl/InOutFuncs");
    auto nsObject = lookupClass("rtl/NSObject");
    if (inOut && nsObject) {
        inOut->setSuperclass(nsObject);
    }
}

