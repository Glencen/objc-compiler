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
        case TypeKind::CHAR:        res += "C"; break;
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
    return false; //TODO: Сделать проверку на каст с float и int, bool и float, bool и int, на каст объекта родительского класса в объект класса-наследника
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

//--------------------------------------------------------------SymbolInfo--------------------------------------------------------------

SymbolInfo::SymbolInfo(SymbolKind kind, const string& name, const Type& type)
    : kind(kind), name(name), type(type) {}

unique_ptr<SymbolInfo> SymbolInfo::clone() const {
    return make_unique<SymbolInfo>(*this);
}

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

unique_ptr<SymbolInfo> LocalVarInfo::clone() const {
    return make_unique<LocalVarInfo>(*this);
}

string LocalVarInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (" + string(isParameter ? "parameter" : "local") + ")";
    return base;
}

bool LocalVarInfo::isLocal() const {
    return !isParameter;
}

bool LocalVarInfo::isParam() const {
    return isParameter;
}

//--------------------------------------------------------------FieldInfo--------------------------------------------------------------

FieldInfo::FieldInfo(const string& name, const Type& type, bool isInstance, ClassInfo* declaringClass)
    : SymbolInfo(SymbolKind::FIELD, name, type),
      declaringClass(declaringClass),
      isInstance(isInstance) {}

unique_ptr<SymbolInfo> FieldInfo::clone() const {
    return make_unique<FieldInfo>(*this);
}

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

//--------------------------------------------------------------MethodInfo--------------------------------------------------------------

MethodInfo::MethodInfo(const string& name, const Type& returnType, bool isClassMethod, ClassInfo* declaringClass)
    : SymbolInfo(SymbolKind::METHOD, name, returnType),
      declaringClass(declaringClass),
      isClassMethod(isClassMethod) {}

unique_ptr<SymbolInfo> MethodInfo::clone() const {
    auto copy = make_unique<MethodInfo>(*this);
    copy->declaringClass = this->declaringClass;
    
    deepCopyParameters(copy->parameters, this->parameters);
    
    for (const auto& [name, var] : localVars) {
        copy->localVars[name] = make_unique<LocalVarInfo>(*var);
    }
    
    copy->parameterTypes = this->parameterTypes;
    copy->selector = this->selector;
    copy->keywords = this->keywords;
    
    return copy;
}

string MethodInfo::toString() const {
    string base = SymbolInfo::toString();
    base += " (" + string(isClassMethod ? "class" : "instance") + " method)";
    base += " params: " + to_string(parameters.size());
    return base;
}

bool MethodInfo::matchesSignature(const vector<const Type*>& argTypes) const {
    if (argTypes.size() != parameterTypes.size()) {
        return false;
    }
    
    for (size_t i = 0; i < argTypes.size(); ++i) {
        if (!parameterTypes[i]->equal(argTypes[i])) {
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

void MethodInfo::deepCopyParameters(
    vector<unique_ptr<LocalVarInfo>>& dest,
    const vector<unique_ptr<LocalVarInfo>>& src) const {
    
    dest.reserve(src.size());
    for (const auto& param : src) {
        dest.push_back(make_unique<LocalVarInfo>(*param));
        dest.back()->enclosingMethod = nullptr;
    }
}

//--------------------------------------------------------------ClassInfo--------------------------------------------------------------

ClassInfo::ClassInfo(const string& name, ClassInfo* superclass)
    : SymbolInfo(SymbolKind::CLASS, name, Type(TypeKind::CLASS_NAME, name)),
      superclass(superclass) {}

unique_ptr<SymbolInfo> ClassInfo::clone() const {
    auto copy = make_unique<ClassInfo>(*this);
    copy->superclass = this->superclass;
    
    deepCopyFields(copy->fields, this->fields);
    deepCopyMethods(copy->methods, this->methods);
    
    return copy;
}

string ClassInfo::toString() const {
    string base = SymbolInfo::toString();
    if (superclass) {
        base += " extends " + superclass->name;
    }
    base += " (fields: " + to_string(fields.size()) + ", methods: " + to_string(getMethodCount()) + ")";
    return base;
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

MethodInfo* ClassInfo::lookupMethod(const string& name, const vector<const Type*>& argTypes, bool includeSuper) {
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (const auto& method : it->second) {
            if (method->matchesSignature(argTypes)) {
                return method.get();
            }
        }
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupMethod(name, argTypes, true);
    }
    
    return nullptr;
}

const MethodInfo* ClassInfo::lookupMethod(const string& name, const vector<const Type*>& argTypes, bool includeSuper) const {
    auto it = methods.find(name);
    if (it != methods.end()) {
        for (const auto& method : it->second) {
            if (method->matchesSignature(argTypes)) {
                return method.get();
            }
        }
    }
    
    if (includeSuper && superclass) {
        return superclass->lookupMethod(name, argTypes, true);
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
    if (method) {
        method->declaringClass = this;
        methods[method->name].push_back(move(method));
    }
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

bool ClassInfo::hasSuperclass() const {
    return superclass != nullptr;
}

void ClassInfo::deepCopyFields(map<string, unique_ptr<FieldInfo>>& dest, const map<string, unique_ptr<FieldInfo>>& src) const {
    for (const auto& [name, field] : src) {
        dest[name] = make_unique<FieldInfo>(*field);
        dest[name]->declaringClass = nullptr;  // Исправится при присоединении
    }
}

void ClassInfo::deepCopyMethods(map<string, vector<unique_ptr<MethodInfo>>>& dest, const map<string, vector<unique_ptr<MethodInfo>>>& src) const {
    for (const auto& [name, methodList] : src) {
        auto& destList = dest[name];
        for (const auto& method : methodList) {
            auto cloned = method->clone();
            if (auto methodPtr = dynamic_cast<MethodInfo*>(cloned.get())) {
                cloned.release();
                destList.push_back(unique_ptr<MethodInfo>(methodPtr));
                destList.back()->declaringClass = nullptr;  // Исправится
            }
        }
    }
}

//--------------------------------------------------------------FunctionInfo--------------------------------------------------------------

FunctionInfo::FunctionInfo(const string& name, const Type& returnType)
    : SymbolInfo(SymbolKind::FUNCTION, name, returnType) {}

unique_ptr<SymbolInfo> FunctionInfo::clone() const {
    auto copy = make_unique<FunctionInfo>(*this);
    
    deepCopyParameters(copy->parameters, this->parameters);
    
    for (const auto& [name, var] : localVars) {
        copy->localVars[name] = make_unique<LocalVarInfo>(*var);
    }
    return copy;
}

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

void FunctionInfo::deepCopyParameters(vector<unique_ptr<LocalVarInfo>>& dest, const vector<unique_ptr<LocalVarInfo>>& src) const {
    dest.reserve(src.size());
    for (const auto& param : src) {
        dest.push_back(make_unique<LocalVarInfo>(*param));
    }
}

//--------------------------------------------------------------SemanticContext--------------------------------------------------------------

SemanticContext::Scope::Scope(Scope* parent, ScopeKind kind)
    : parent(parent), kind(kind) {}

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

SemanticContext::SemanticContext() {
    initReservedNames();
    enterScope();
}

SemanticContext::~SemanticContext() = default;

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
    if (!var || var->name.empty()) return false;
    
    if (scopes.empty()) return false;
    
    auto& scope = scopes.top();
    if (scope->locals.find(var->name) != scope->locals.end()) {
        return false;  // Переменная уже существует в этом scope
    }
    
    scope->locals[var->name] = move(var);
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

void SemanticContext::enterScope(Scope::ScopeKind kind) {
    Scope* parent = scopes.empty() ? nullptr : scopes.top().get();
    scopes.push(make_unique<Scope>(parent, kind));
}

void SemanticContext::leaveScope() {
    if (!scopes.empty()) {
        scopes.pop();
    }
}

void SemanticContext::enterClassScope(ClassInfo* cls) {
    setCurrentClass(cls);
    enterScope(Scope::CLASS_SCOPE);
}

void SemanticContext::enterMethodScope(MethodInfo* method) {
    setCurrentMethod(method);
    enterScope(Scope::METHOD_SCOPE);
}

void SemanticContext::enterFunctionScope(FunctionInfo* func) {
    setCurrentFunction(func);
    enterScope(Scope::FUNCTION_SCOPE);
}

void SemanticContext::enterLoopScope() {
    enterScope(Scope::LOOP_SCOPE);
}

void SemanticContext::enterConditionalScope() {
    enterScope(Scope::CONDITIONAL_SCOPE);
}

void SemanticContext::enterBlockStmtScope() {
    enterScope(Scope::BLOCK_STMT_SCOPE);
}

SymbolInfo* SemanticContext::lookup(const string& name) const {
    if (!scopes.empty()) {
        const auto& scope = scopes.top();
        if (auto var = scope->lookup(name)) {
            return var;
        }
    }
    
    if (currentClass) {
        if (auto field = currentClass->lookupField(name, false)) {
            return field;
        }
        // Для методов нужна сигнатура, так что не ищем здесь
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
    auto it = classes.find(name);
    return it != classes.end() ? it->second.get() : nullptr;
}

MethodInfo* SemanticContext::lookupMethod(const string& className, const string& methodName, const vector<const Type*>& argTypes) const {
    auto cls = lookupClass(className);
    if (!cls) return nullptr;

    return cls->lookupMethod(methodName, argTypes, true);
}

FieldInfo* SemanticContext::lookupField(const string& className, const string& fieldName) const {
    auto cls = lookupClass(className);
    if (!cls) return nullptr;
    
    return cls->lookupField(fieldName, true);
}

LocalVarInfo* SemanticContext::lookupLocalVar(const string& name) const {
    if (scopes.empty()) return nullptr;
    
    const auto& scope = scopes.top();
    return scope->lookup(name);
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

bool SemanticContext::isAssignable(const Type& from, const Type& to) const {
    if (from.equal(&to)) return true;
    
    if (from.isNumeric() && to.isNumeric()) {
        return true;
    }
    
    if (from.dataType == TypeKind::NONE && (to.dataType == TypeKind::CLASS_NAME || to.isArray())) {
        return true;
    }
    
    if (from.dataType == TypeKind::CLASS_NAME && to.dataType == TypeKind::CLASS_NAME) {
        auto fromClass = lookupClass(from.className);
        auto toClass = lookupClass(to.className);
        
        if (fromClass && toClass) {
            return fromClass->isSubclassOf(toClass);
        }
    }
    
    if (from.isArray() && to.isArray()) {
        if (from.arrayDimension != to.arrayDimension) return false;
        
        Type fromElem(from.dataType, from.className);
        Type toElem(to.dataType, to.className);
        return isAssignable(fromElem, toElem);
    }
    
    return false;
}

bool SemanticContext::isConvertible(const Type& from, const Type& to) const {
    // Конвертация включает явные приведения
    if (isAssignable(from, to)) return true;
    
    // Дополнительные правила конвертации:
    // 1. bool -> int
    if (from.dataType == TypeKind::BOOL && to.dataType == TypeKind::INT) {
        return true;
    }
    
    // 2. String -> char[]
    if (from.dataType == TypeKind::CHAR && to.isArray() && 
        to.dataType == TypeKind::CHAR) {
        return true;
    }
    
    // 3. Любой объект -> Object
    if (from.dataType == TypeKind::CLASS_NAME && 
        to.dataType == TypeKind::CLASS_NAME && 
        to.className == "Object") {
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

bool SemanticContext::checkMethodOverride(const MethodInfo* base, const MethodInfo* derived) const {
    if (!base || !derived) return false;
    
    // 1. Имена должны совпадать
    if (base->name != derived->name) return false;
    
    // 2. Типы параметров должны совпадать
    if (base->parameterTypes.size() != derived->parameterTypes.size()) {
        return false;
    }
    
    for (size_t i = 0; i < base->parameterTypes.size(); ++i) {
        if (!base->parameterTypes[i]->equal(derived->parameterTypes[i])) {
            return false;
        }
    }
    
    // 3. Возвращаемый тип должен быть ковариантным
    if (!isAssignable(derived->getReturnType(), base->getReturnType())) {
        return false;
    }
    
    // 4. Модификаторы доступа (если есть)
    // derived должен быть не более строгим, чем base
    
    return true;
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

bool SemanticContext::validateMethodOverrides() const {
    for (const auto& [name, cls] : classes) {
        if (!cls->superclass) continue;
        
        // Для каждого метода в классе
        for (const auto& [methodName, methodList] : cls->methods) {
            for (const auto& method : methodList) {
                // Ищем метод с такой же сигнатурой в суперклассе
                auto baseMethod = cls->superclass->lookupMethod(
                    methodName, method->parameterTypes, true);
                
                if (baseMethod && !checkMethodOverride(baseMethod, method.get())) {
                    cerr << "Incorrect override of method " << methodName 
                             << " in class " << name << endl;
                    return false;
                }
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

bool SemanticContext::isInClassScope() const {
    return currentClass != nullptr;
}

bool SemanticContext::isInMethodScope() const {
    return currentMethod != nullptr;
}

bool SemanticContext::isInFunctionScope() const {
    return currentFunction != nullptr;
}

bool SemanticContext::isGlobalScope() const {
    return !isInClassScope() && !isInMethodScope() && !isInFunctionScope();
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
    // Преобразуем "fieldName" в "setFieldName:"
    string setter = "set";
    if (!fieldName.empty()) {
        setter += static_cast<char>(toupper(fieldName[0]));
        setter += fieldName.substr(1);
    }
    setter += ":";
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
    if (scopes.empty()) {
        cout << "No active scope" << endl;
        return;
    }
    
    const auto& scope = scopes.top();
    
    cout << "\n=== Current Scope ===" << endl;
    cout << "Scope kind: ";
    switch(scope->kind) {
        case Scope::GLOBAL_SCOPE: cout << "GLOBAL"; break;
        case Scope::CLASS_SCOPE: cout << "CLASS"; break;
        case Scope::METHOD_SCOPE: cout << "METHOD"; break;
        case Scope::FUNCTION_SCOPE: cout << "FUNCTION"; break;
        case Scope::LOOP_SCOPE: cout << "LOOP"; break;
        case Scope::CONDITIONAL_SCOPE: cout << "CONDITIONAL"; break;
        case Scope::BLOCK_STMT_SCOPE: cout << "BLOCK"; break;
    }
    cout << endl;
    
    cout << "Local variables (" << scope->locals.size() << "):" << endl;
    for (const auto& [name, var] : scope->locals) {
        cout << "  " << var->toString() << endl;
    }
}

void SemanticContext::resolveInheritance() {
    for (auto& [name, cls] : classes) {
        if (cls->superclass && !cls->superclass->name.empty()) {
            auto super = lookupClass(cls->superclass->name);
            if (super) {
                cls->superclass = super;
            } else {
                cerr << "Warning: Class " << name 
                         << " inherits from undefined class " 
                         << cls->superclass->name << endl;
                cls->superclass = nullptr;
            }
        }
    }
}

optional<SemanticContext::Scope*> SemanticContext::currentScope() {
    if (scopes.empty()) {
        return nullopt;
    }
    return scopes.top().get();
}

void SemanticContext::initReservedNames() {
    reservedNames = {
        "abstract", "assert", "boolean", "break", "byte",
        "case", "catch", "char", "class", "const",
        "continue", "default", "do", "double", "else",
        "enum", "extends", "final", "finally", "float",
        "for", "goto", "if", "implements", "import",
        "instanceof", "int", "interface", "long", "native",
        "new", "package", "private", "protected", "public",
        "return", "short", "static", "strictfp", "super",
        "switch", "synchronized", "this", "throw", "throws",
        "transient", "try", "void", "volatile", "while"
    };
    
    reservedNames.insert({
        "id", "SEL", "IMP", "BOOL", "YES", "NO",
        "nil", "Nil", "NULL", "self", "super",
        "retain", "release", "autorelease", "copy"
    });
}