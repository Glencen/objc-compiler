#include "context.h"

Type convertTypeNodeToType(TypeNode* typeNode, vector<int> arraySizes = {}) { // TODO: куда впихнуть TYPE_ID ???
    if (!typeNode) return Type(TypeKind::NONE);
    
    TypeKind typeKind = typeNode->getKind();
    string className = typeKind == TypeKind::CLASS_NAME ? typeNode->getClassName()->getClassName() : "";
    
    if (typeNode->isPrimitive()) {
        return Type(typeKind);
    } else if (typeKind == TypeKind::CLASS_NAME) {
        if (!arraySizes.empty()) {
            return Type(TypeKind::CLASS_NAME, className, arraySizes);
        } else {
            return Type(TypeKind::CLASS_NAME, className);
        }
    } else if (!arraySizes.empty()) {
        return Type(typeKind, arraySizes);
    }
    
    return Type(TypeKind::NONE);
}

//--------------------------------------------------------------ValueNode--------------------------------------------------------------

void ValueNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ReceiverNode--------------------------------------------------------------

void ReceiverNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------

void MsgArgNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

void MsgArgListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

void MsgSelectorNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

void ExprListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

void ExprNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------TypeNode--------------------------------------------------------------

void TypeNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------DeclaratorListNode--------------------------------------------------------------

void DeclaratorListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------DeclNode--------------------------------------------------------------

void DeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------StmtListNode--------------------------------------------------------------

void StmtListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

void StmtNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

void ArraySizeSpecNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

void ParamDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

void ParamListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

void FuncDefNode::analyzeSemantics(SemanticContext& context) {
    string funcName = identifier->getIdentifier();
    Type returnType = convertTypeNodeToType(type);

    FunctionInfo* funcToAnalyze = nullptr;
    
    FunctionInfo* existingFunc = context.lookupFunction(funcName);
    
    if (existingFunc) {
        if (!returnType.equal(&existingFunc->getReturnType())) {
            throw function_exception("Function '" + funcName + "' return type mismatch with previous declaration",
                "FuncDefNode::analyzeSemantics", -1, -1, 
                "Expected: " + existingFunc->getReturnType().getDescriptor() + 
                ", Got: " + returnType.getDescriptor());
        }
        
        size_t existingParamCount = existingFunc->getParameterCount();
        size_t currentParamCount = 0;
        
        if (paramList) {
            auto params = paramList->getParamList();
            currentParamCount = params ? params->size() : 0;
        }
        
        if (existingParamCount != currentParamCount) {
            throw function_exception("Function '" + funcName + "' parameter count mismatch with previous declaration",
                "FuncDefNode::analyzeSemantics", -1, -1,
                "Expected: " + to_string(existingParamCount) + 
                ", Got: " + to_string(currentParamCount));
        }
        
        if (paramList && currentParamCount > 0) {
            auto params = paramList->getParamList();
            if (params) {
                size_t i = 0;
                for (auto* paramDecl : *params) {
                    paramDecl->analyzeSemantics(context);
                    
                    Type paramType = convertTypeNodeToType(paramDecl->getType());
                    const LocalVarInfo* existingParam = existingFunc->getParameter(i);
                    
                    if (!existingParam) {
                        throw function_exception("Function '" + funcName + "' parameter mismatch",
                            "FuncDefNode::analyzeSemantics", -1, -1,
                            "Parameter at index " + to_string(i) + " is missing in declaration");
                    }
                    
                    if (!paramType.equal(&existingParam->type)) {
                        throw function_exception("Function '" + funcName + "' parameter type mismatch",
                            "FuncDefNode::analyzeSemantics", -1, -1,
                            "Parameter " + to_string(i + 1) + 
                            ": Expected: " + existingParam->type.getDescriptor() + 
                            ", Got: " + paramType.getDescriptor());
                    }
                    
                    i++;
                }
            }
        }

        if (existingFunc->body) {
            throw function_exception("Function '" + funcName + "' already defined",
                "FuncDefNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
        }

        existingFunc->body = compoundStmt;
        funcToAnalyze = existingFunc;
    } else {
        auto func = make_unique<FunctionInfo>(funcName, returnType);
        FunctionInfo* funcPtr = func.get();
        
        if (paramList) {
            auto params = paramList->getParamList();
            if (params) {
                for (auto* paramDecl : *params) {
                    paramDecl->analyzeSemantics(context);
                    
                    Type paramType = convertTypeNodeToType(paramDecl->getType());
                    
                    auto paramInfo = make_unique<LocalVarInfo>(
                        paramDecl->getIdentifier()->getIdentifier(),
                        paramType,
                        true,
                        nullptr
                    );
                    
                    func->addParameter(move(paramInfo));
                }
            }
        }
        
        func->body = compoundStmt;
        
        if (!context.addFunction(move(func))) {
            throw function_exception("Failed to add function '" + funcName + "' to context",
                "FuncDefNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
        }
        
        funcToAnalyze = funcPtr;
    }
    
    if (context.isReservedName(funcName)) {
        throw function_exception("Function name '" + funcName + "' is a reserved keyword",
            "FuncDefNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
    
    if (compoundStmt) {
        // TODO: проверка именно compound statement, а не какого-то другого
        ClassInfo* savedClass = context.getCurrentClass();
        MethodInfo* savedMethod = context.getCurrentMethod();
        FunctionInfo* savedFunction = context.getCurrentFunction();
        
        try {
            context.enterFunctionScope(funcToAnalyze);
            for (size_t i = 0; i < funcToAnalyze->getParameterCount(); i++) {
                const LocalVarInfo* param = funcToAnalyze->getParameter(i);
                if (param) {
                    auto paramCopy = make_unique<LocalVarInfo>(
                        param->name,
                        param->type,
                        true,
                        nullptr
                    );
                    
                    if (!context.addLocalVar(move(paramCopy))) {
                        throw function_exception("Failed to add parameter '" + param->name + "' to function scope",
                            "FuncDefNode::analyzeSemantics", -1, -1, "Function: '" + funcName + "'");
                    }
                }
            }
            
            compoundStmt->analyzeSemantics(context);
            context.dumpCurrentScope();

            context.leaveScope();
            context.setCurrentClass(savedClass);
            context.setCurrentMethod(savedMethod);
            context.setCurrentFunction(savedFunction);
            
        } catch (...) {
            context.dumpCurrentScope();
            context.leaveScope();
            context.setCurrentClass(savedClass);
            context.setCurrentMethod(savedMethod);
            context.setCurrentFunction(savedFunction);
            throw;
        }
    } else {
        throw function_exception("Function '" + funcName + "' has no body",
            "FuncDefNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
    
    // Проверяем, что функция с типом возврата void не возвращает значение (если это возможно проверить на этом этапе)
}

//--------------------------------------------------------------FuncDeclNode--------------------------------------------------------------

void FuncDeclNode::analyzeSemantics(SemanticContext& context) {
    string funcName = identifier->getIdentifier();
    Type returnType = convertTypeNodeToType(type);
    
    if (context.lookupFunction(funcName)) {
        throw function_exception("Function '" + funcName + "' already declared",
            "FuncDeclNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
    
    auto func = make_unique<FunctionInfo>(funcName, returnType);
    
    if (paramList) {
        auto params = paramList->getParamList();
        if (params) {
            for (auto* paramDecl : *params) {
                paramDecl->analyzeSemantics(context);
                
                Type paramType = convertTypeNodeToType(paramDecl->getType());
                
                auto paramInfo = make_unique<LocalVarInfo>(
                    paramDecl->getIdentifier()->getIdentifier(),
                    paramType,
                    true,
                    nullptr
                );
                
                func->addParameter(move(paramInfo));
            }
        }
    }
    
    if (!context.addFunction(move(func))) {
        throw function_exception("Failed to add function '" + funcName + "' to context",
            "FuncDeclNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
    
    if (context.isReservedName(funcName)) {
        throw function_exception("Function name '" + funcName + "' is a reserved keyword",
            "FuncDeclNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
}

//--------------------------------------------------------------MethodParamNode--------------------------------------------------------------

void MethodParamNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MethodSelNode--------------------------------------------------------------

void MethodSelNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MethodDefNode--------------------------------------------------------------

void MethodDefNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ImplementationDefListNode--------------------------------------------------------------

void ImplementationDefListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MethodDeclNode--------------------------------------------------------------

void MethodDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------PropertyNode--------------------------------------------------------------

void PropertyNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InterfaceDeclListNode--------------------------------------------------------------

void InterfaceDeclListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InitializerListNode--------------------------------------------------------------

void InitializerListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InitializerNode--------------------------------------------------------------

void InitializerNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------DeclaratorNode--------------------------------------------------------------

void DeclaratorNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InitDeclNode--------------------------------------------------------------

void InitDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------AccessModifierNode--------------------------------------------------------------

void AccessModifierNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InstanceVarDeclNode--------------------------------------------------------------

void InstanceVarDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InstanceVarsDeclListNode--------------------------------------------------------------

void InstanceVarsDeclListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InstanceVarsNode--------------------------------------------------------------

void InstanceVarsNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ImplementationNode--------------------------------------------------------------

void ImplementationNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------InterfaceNode--------------------------------------------------------------

void InterfaceNode::processProperties(SemanticContext& context) {
    ClassInfo* cls = context.getCurrentClass();
    if (!cls) return;

    list<PropertyNode*> properties = *interfaceDeclList->getProperties();
    
    for (auto* property : properties) {
        string propertyName = property->getName()->getIdentifier();
        Type propertyType = convertTypeNodeToType(property->getType());
        bool isReadonly = property->getAttribute() == Attribute::READONLY;
        
        string ivarName = "_" + propertyName;
        
        if (!cls->lookupField(ivarName, false)) {
            auto ivar = make_unique<FieldInfo>(ivarName, propertyType, true, cls);
            cls->addField(move(ivar));
        } // нужно ли добавлять исключение при уже объявленном ivar ???
        
        cls->addPropertyMapping(propertyName, ivarName);
        
        string getterName = context.generateGetterName(propertyName);
        if (!cls->lookupMethod(getterName)) {
            auto getter = make_unique<MethodInfo>(getterName, propertyType, false, cls);
            cls->addMethod(move(getter));
        }
        
        if (!isReadonly) {
            string setterName = context.generateSetterName(propertyName);
            if (!cls->lookupMethod(setterName)) {
                Type voidType(TypeKind::VOID);
                auto setter = make_unique<MethodInfo>(setterName, voidType, false, cls);
                auto param = make_unique<LocalVarInfo>("value", propertyType, true, setter.get());
                setter->addParameter(move(param));
                cls->addMethod(move(setter));
            }
        }
    }
}

void InterfaceNode::analyzeSemantics(SemanticContext& context) {
    string classNameStr = className->getClassName();
    string superclassNameStr = superClassName ? superClassName->getClassName() : "";
    
    ClassInfo* cls = context.lookupClass(classNameStr);
    if (!cls) {
        ClassInfo* superclass = nullptr;
        if (!superclassNameStr.empty()) {
            superclass = context.lookupClass(superclassNameStr);
            if (!superclass) {
                throw class_exception("Undefined super class '" + superclassNameStr + "'",
                    "InterfaceNode::analyzeSemantics", -1, -1, "Class: " + classNameStr + "'");
            }
        }
        auto newClass = make_unique<ClassInfo>(classNameStr, superclass);
        cls = newClass.get();
        context.addClass(move(newClass));
    }
    cls->markAsInterface();
    
    ClassInfo* prevClass = context.getCurrentClass();
    context.enterClassScope(cls);
    
    try {
        if (instanceVars) {
            instanceVars->analyzeSemantics(context);
        }
        if (interfaceDeclList) {
            interfaceDeclList->analyzeSemantics(context);
        }

        processProperties(context);
        
        context.leaveScope();
        context.setCurrentClass(prevClass);
        
    } catch (...) {
        context.leaveScope();
        context.setCurrentClass(prevClass);
        throw;
    }
}

//--------------------------------------------------------------ClassNameListNode--------------------------------------------------------------

void ClassNameListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ExternalDeclNode--------------------------------------------------------------

void ExternalDeclNode::analyzeSemantics(SemanticContext& context) {
    switch (kind) {
        case ExternalDeclKind::INTERFACE:
            if (interface) interface->analyzeSemantics(context);
            break;
        case ExternalDeclKind::IMPLEMENTATION:
            if (implementation) implementation->analyzeSemantics(context);
            break;
        case ExternalDeclKind::FUNC_DEF:
            if (funcDef) {
                if (!context.isInGlobalScope()) {
                    throw function_exception("Function can only be declared at global scope",
                        "ExternalDeclNode::analyzeSemantics", -1, -1, "Function name: '" + funcDef->getIdentifier()->getIdentifier() + "'");
                }
                funcDef->analyzeSemantics(context);
            }
            break;
        case ExternalDeclKind::FUNC_DECL:
            if (funcDecl) {
                if (!context.isInGlobalScope()) {
                    throw function_exception("Function can only be declared at global scope",
                        "ExternalDeclNode::analyzeSemantics", -1, -1, "Function name: '" + funcDef->getIdentifier()->getIdentifier() + "'");
                }
                funcDecl->analyzeSemantics(context);
            }
            break;
        case ExternalDeclKind::CLASS_FW_DECL_LIST:
            if (classNames) {
                auto* list = classNames->getClassFwDeclList();
                if (list) {
                    for (auto* classNameNode : *list) {
                        auto className = classNameNode->getClassName();
                        if (!context.lookupClass(className)) {
                            auto cls = make_unique<ClassInfo>(className);
                            context.addClass(move(cls));
                        }
                    }
                }
            }
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ExternalDeclListNode--------------------------------------------------------------

void ExternalDeclListNode::analyzeSemantics(SemanticContext& context) {
    if (!externalDeclList) return;
    
    ClassInfo* savedClass = context.getCurrentClass();
    MethodInfo* savedMethod = context.getCurrentMethod();
    FunctionInfo* savedFunction = context.getCurrentFunction();
    
    try {
        for (auto* decl : *externalDeclList) {
            decl->analyzeSemantics(context);
            if (!context.isInGlobalScope()) {
                while (!context.isInGlobalScope()) {
                    context.leaveScope();
                }
            }
        }
        
        context.setCurrentClass(savedClass);
        context.setCurrentMethod(savedMethod);
        context.setCurrentFunction(savedFunction);
        
    } catch (...) {
        context.setCurrentClass(savedClass);
        context.setCurrentMethod(savedMethod);
        context.setCurrentFunction(savedFunction);
        throw;
    }
}


//--------------------------------------------------------------ProgramNode--------------------------------------------------------------

void ProgramNode::analyzeSemantics(SemanticContext& context) {
    if (!context.isInGlobalScope()) {
        while (!context.isInGlobalScope()) {
            context.leaveScope();
        }
    }
    
    try {
        if (externalDeclList) {
            externalDeclList->analyzeSemantics(context);
        }
        
        if (!context.isInGlobalScope()) {
            context.leaveScope();
        }
        
    } catch (...) {
        while (!context.isInGlobalScope()) {
            context.leaveScope();
        }
        throw;
    }
}
