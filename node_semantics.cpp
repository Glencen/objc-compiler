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

void StmtListNode::analyzeSemantics(SemanticContext& context) {
    if (!stmts) return;
    
    for (StmtNode* stmt : *stmts) {
        if (stmt) {
            stmt->analyzeSemantics(context);
        }
    }
}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

void StmtNode::analyzeSemantics(SemanticContext& context) {
    switch (kind) {
        case StmtKind::EMPTY:
            break;
            
        case StmtKind::EXPR:
            if (expr) {
                expr->analyzeSemantics(context);
            }
            break;
            
        case StmtKind::RETURN:
            analyzeReturnSemantics(context);
            break;
            
        case StmtKind::IF:
            analyzeIfSemantics(context);
            break;
            
        case StmtKind::IF_ELSE:
            analyzeIfElseSemantics(context);
            break;
            
        case StmtKind::FOR_WITH_DECL:
            analyzeForSemantics(context);
            break;
            
        case StmtKind::FOR_IN:
            analyzeForInSemantics(context);
            break;
            
        case StmtKind::WHILE:
            analyzeWhileSemantics(context);
            break;
            
        case StmtKind::DO_WHILE:
            analyzeDoWhileSemantics(context);
            break;
            
        case StmtKind::COMPOUND:
            analyzeCompoundSemantics(context);
            break;
            
        case StmtKind::DECLARATION:
            if (decl) {
                decl->analyzeSemantics(context);
            }
            break;
            
        default:
            throw statement_exception("Unknown statement type",
                "StmtNode::analyzeSemantics", -1, -1, 
                "Statement kind: " + to_string(static_cast<int>(kind)));
    }
}

void StmtNode::analyzeReturnSemantics(SemanticContext& context) {
    // Проверяем, что мы внутри функции или метода
    FunctionInfo* currentFunction = context.getCurrentFunction();
    MethodInfo* currentMethod = context.getCurrentMethod();
    
    if (!currentFunction && !currentMethod) {
        throw statement_exception("Return statement outside of function or method",
            "StmtNode::analyzeReturnSemantics", -1, -1);
    }
    
    Type expectedReturnType(TypeKind::VOID);
    if (currentFunction) {
        expectedReturnType = currentFunction->getReturnType();
    } else if (currentMethod) {
        expectedReturnType = currentMethod->getReturnType();
    }
    
    // Проверяем тип возвращаемого выражения
    if (expr) {
        // Есть возвращаемое значение
        expr->analyzeSemantics(context);
        
        // Получаем тип выражения
        Type exprType = getExpressionType(expr, context);
        
        // Проверяем совместимость типов
        if (!context.isAssignable(exprType, expectedReturnType)) {
            throw statement_exception("Return type mismatch",
                "StmtNode::analyzeReturnSemantics", -1, -1,
                "Expected: " + expectedReturnType.getDescriptor() + 
                ", Got: " + exprType.getDescriptor());
        }
    } else {
        // Нет возвращаемого значения
        // Проверяем, что функция ожидает void
        if (!expectedReturnType.equal(&Type(TypeKind::VOID))) {
            throw statement_exception("Function must return a value",
                "StmtNode::analyzeReturnSemantics", -1, -1,
                "Expected return type: " + expectedReturnType.getDescriptor());
        }
    }
}

void StmtNode::analyzeIfSemantics(SemanticContext& context) {
    if (!condition) {
        throw statement_exception("If statement must have a condition",
            "StmtNode::analyzeIfSemantics", -1, -1);
    }
    
    condition->analyzeSemantics(context);
    
    // Проверяем, что условие имеет булевый тип
    Type conditionType = getExpressionType(condition, context);
    Type boolType(TypeKind::BOOL);
    
    if (!conditionType.equal(&boolType) && !context.isConvertible(conditionType, boolType)) {
        throw statement_exception("If condition must be boolean",
            "StmtNode::analyzeIfSemantics", -1, -1,
            "Got type: " + conditionType.getDescriptor());
    }
    
    if (thenBranch) {
        // Входим в область видимости условия
        context.enterConditionalScope();
        thenBranch->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeIfElseSemantics(SemanticContext& context) {
    analyzeIfSemantics(context);
    
    if (elseBranch) {
        // Входим в область видимости else ветки
        context.enterConditionalScope();
        elseBranch->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeForSemantics(SemanticContext& context) {
    // Обработка инициализации
    if (decl) {
        decl->analyzeSemantics(context);
    } else if (expr) {
        expr->analyzeSemantics(context);
    }
    
    // Обработка условия
    if (condition) {
        condition->analyzeSemantics(context);
        
        // Проверяем, что условие имеет булевый тип
        Type conditionType = getExpressionType(condition, context);
        Type boolType(TypeKind::BOOL);
        
        if (!conditionType.equal(&boolType) && !context.isConvertible(conditionType, boolType)) {
            throw statement_exception("For loop condition must be boolean",
                "StmtNode::analyzeForSemantics", -1, -1,
                "Got type: " + conditionType.getDescriptor());
        }
    }
    
    // Обработка пост-действия
    if (post) {
        post->analyzeSemantics(context);
    }
    
    // Обработка тела цикла
    if (body) {
        context.enterLoopScope();
        body->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeForInSemantics(SemanticContext& context) {
    // TODO: Реализовать семантический анализ for-in
    // Проверяем коллекцию
    if (collection) {
        collection->analyzeSemantics(context);
    }
    
    // Обработка тела цикла
    if (body) {
        context.enterLoopScope();
        body->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeWhileSemantics(SemanticContext& context) {
    if (!condition) {
        throw statement_exception("While statement must have a condition",
            "StmtNode::analyzeWhileSemantics", -1, -1);
    }
    
    condition->analyzeSemantics(context);
    
    // Проверяем, что условие имеет булевый тип
    Type conditionType = getExpressionType(condition, context);
    Type boolType(TypeKind::BOOL);
    
    if (!conditionType.equal(&boolType) && !context.isConvertible(conditionType, boolType)) {
        throw statement_exception("While condition must be boolean",
            "StmtNode::analyzeWhileSemantics", -1, -1,
            "Got type: " + conditionType.getDescriptor());
    }
    
    if (body) {
        context.enterLoopScope();
        body->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeDoWhileSemantics(SemanticContext& context) {
    if (!condition) {
        throw statement_exception("Do-while statement must have a condition",
            "StmtNode::analyzeDoWhileSemantics", -1, -1);
    }
    
    if (body) {
        context.enterLoopScope();
        body->analyzeSemantics(context);
        context.leaveScope();
    }
    
    condition->analyzeSemantics(context);
    
    // Проверяем, что условие имеет булевый тип
    Type conditionType = getExpressionType(condition, context);
    Type boolType(TypeKind::BOOL);
    
    if (!conditionType.equal(&boolType) && !context.isConvertible(conditionType, boolType)) {
        throw statement_exception("Do-while condition must be boolean",
            "StmtNode::analyzeDoWhileSemantics", -1, -1,
            "Got type: " + conditionType.getDescriptor());
    }
}

void StmtNode::analyzeCompoundSemantics(SemanticContext& context) {
    if (!compound) {
        throw statement_exception("Compound statement must have a statement list",
            "StmtNode::analyzeCompoundSemantics", -1, -1);
    }
    
    // Входим в область видимости блока
    context.enterBlockStmtScope();
    compound->analyzeSemantics(context);
    context.leaveScope();
}

Type StmtNode::getExpressionType(ExprNode* expr, SemanticContext& context) {
    // В реальной реализации нужно получить тип выражения из анализа
    // Здесь - заглушка
    return Type(TypeKind::INT);
}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

void ArraySizeSpecNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

void ParamDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

void ParamListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

void FuncDefNode::checkReturnStatements(FunctionInfo* func, SemanticContext& context) {
    // Собираем все return statement из тела функции
    vector<StmtNode*> returnStmts;
    collectReturnStatements(compoundStmt, returnStmts);
    
    Type voidType(TypeKind::VOID);
    Type returnType = func->getReturnType();
    
    if (returnType.equal(&voidType)) {
        // Для void функций: проверяем, что нет return с выражением
        for (StmtNode* stmt : returnStmts) {
            // У return statement выражение хранится в поле expr
            // Если expr != nullptr, значит есть возвращаемое значение
            if (stmt->getKind() == StmtKind::RETURN) {
                // Здесь предполагается, что у StmtNode есть метод для получения выражения
                // В реальной реализации нужно получить доступ к expr через StmtNode
                // Поскольку у нас нет доступа к приватным полям, предполагаем следующую структуру:
                
                // Проверяем, есть ли возвращаемое значение
                // Для этого нужно добавить соответствующий метод в StmtNode
                // Покажем концептуально:
                if (hasReturnExpression(stmt)) {
                    throw function_exception("Void function '" + func->name + "' cannot return a value",
                        "FuncDefNode::checkReturnStatements", -1, -1, "Function: '" + func->name + "'");
                }
            }
        }
    } else {
        // Для не-void функций: должен быть хотя бы один return statement
        if (returnStmts.empty()) {
            throw function_exception("Function '" + func->name + "' must return a value",
                "FuncDefNode::checkReturnStatements", -1, -1, 
                "Return type: " + returnType.getDescriptor());
        }
        
        // Проверяем, что все return statement имеют совместимые типы
        for (StmtNode* stmt : returnStmts) {
            if (stmt->getKind() == StmtKind::RETURN) {
                // Проверяем тип возвращаемого выражения
                // В реальной реализации нужно получить тип выражения и сравнить с returnType
                if (!checkReturnExpressionType(stmt, returnType, context)) {
                    throw function_exception("Function '" + func->name + "' return type mismatch",
                        "FuncDefNode::checkReturnStatements", -1, -1,
                        "Expected: " + returnType.getDescriptor());
                }
            }
        }
    }
}

void FuncDefNode::collectReturnStatements(StmtNode* stmt, vector<StmtNode*>& returnStmts) {
    if (!stmt) return;
    
    if (stmt->getKind() == StmtKind::RETURN) {
        returnStmts.push_back(stmt);
        return;
    }
    
    switch (stmt->getKind()) {
        case StmtKind::COMPOUND: {
            StmtListNode* stmtList = stmt->getCompound();
            if (stmtList) {
                auto stmts = stmtList->getStmtList();
                if (stmts) {
                    for (StmtNode* child : *stmts) {
                        collectReturnStatements(child, returnStmts);
                    }
                }
            }
            break;
        }
        
        case StmtKind::IF:
        case StmtKind::IF_ELSE:
            collectReturnStatements(stmt->getThenBranch(), returnStmts);
            if (stmt->getKind() == StmtKind::IF_ELSE) {
                collectReturnStatements(stmt->getElseBranch(), returnStmts);
            }
            break;
            
        case StmtKind::FOR_WITH_DECL:
        case StmtKind::WHILE:
        case StmtKind::DO_WHILE:
            // В теле цикла
            collectReturnStatements(stmt->getBody(), returnStmts);
            break;
            
        default:
            // для других типов statement-ов не ищем вложенные return
            break;
    }
}

// Вспомогательные методы (нужно добавить в StmtNode или реализовать здесь)
bool FuncDefNode::hasReturnExpression(StmtNode* stmt) {
    // Реализация зависит от структуры StmtNode
    // Предположим, что у StmtNode есть метод getReturnExpression()
    return false; // Заглушка
}

bool FuncDefNode::checkReturnExpressionType(StmtNode* stmt, const Type& expectedType, SemanticContext& context) {
    // Реализация проверки типа возвращаемого выражения
    // Возвращает true, если тип выражения совместим с expectedType
    return true; // Заглушка
}

void FuncDefNode::analyzeSemantics(SemanticContext& context) {
    string funcName = identifier->getIdentifier();
    Type returnType = convertTypeNodeToType(type);
    
    if (!compoundStmt) {
        throw function_exception("Function '" + funcName + "' must have a body",
            "FuncDefNode::analyzeSemantics", -1, -1, "Function name: '" + funcName + "'");
    }
    
    if (compoundStmt->getKind() != StmtKind::COMPOUND) {
        throw function_exception("Function '" + funcName + "' body must be a compound statement",
            "FuncDefNode::analyzeSemantics", -1, -1, "Got statement kind: " + to_string(static_cast<int>(compoundStmt->getKind())));
    }

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
        
        checkReturnStatements(funcToAnalyze, context);
        
        context.leaveScope();
        context.setCurrentClass(savedClass);
        context.setCurrentMethod(savedMethod);
        context.setCurrentFunction(savedFunction);
        
    } catch (...) {
        context.leaveScope();
        context.setCurrentClass(savedClass);
        context.setCurrentMethod(savedMethod);
        context.setCurrentFunction(savedFunction);
        throw;
    }
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
