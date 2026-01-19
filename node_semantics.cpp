#include "tables.h"

Type* convertTypeNodeToType(TypeNode* typeNode) {
    if (!typeNode) return nullptr;
    switch (typeNode->getKind()) {
        case TypeKind::INT:
            return new Type(TypeKind::INT);
        case TypeKind::FLOAT:
            return new Type(TypeKind::FLOAT);
        case TypeKind::BOOL:
            return new Type(TypeKind::BOOL);
        case TypeKind::CHAR:
            return new Type(TypeKind::CHAR);
        case TypeKind::TYPE_ID:
            return new Type(TypeKind::TYPE_ID);
        case TypeKind::CLASS_NAME:
            return new Type(TypeKind::CLASS_NAME, *typeNode->getClassName()->getClassName());
        case TypeKind::VOID:
            return new Type(TypeKind::VOID);
        default:
            return nullptr;
    }
}

Type* createArrayType(Type* baseType, list<ExprNode*>* arraySizes) {
    if (!arraySizes || arraySizes->empty()) {
        return baseType;
    }
    ExprNode* firstSize = arraySizes->front();
    return new Type(baseType->dataType, baseType->className, firstSize);
}

//--------------------------------------------------------------ValueNode--------------------------------------------------------------

void ValueNode::fillLiterals(ConstantsTable* constantTable) {
    switch (valueType) {
        case ValueKind::INT_LIT:
            constantTable->findOrAddConstant(ConstantType::Integer, intValue);
            break;
        case ValueKind::FLOAT_LIT:
            constantTable->findOrAddConstant(ConstantType::Float, floatValue);
            break;
        case ValueKind::STRING_LIT:
            constantTable->findOrAddConstant(ConstantType::Utf8, *stringValue);
            break;
        case ValueKind::OBJC_INT_LIT:
        case ValueKind::OBJC_FLOAT_LIT:
        case ValueKind::OBJC_BOOL_LIT:
        case ValueKind::OBJC_STRING_LIT:
            constantTable->findOrAddConstant(ConstantType::Utf8, *stringValue);
            break;
        default:
            break;
    }
}

void ValueNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (valueType == ValueKind::IDENTIFIER && localVariables) {
        string name = *stringValue;
        if (!localVariables->isContains(name)) {
            throw std::runtime_error("Undefined identifier: " + name);
        }
    }
}

//--------------------------------------------------------------ReceiverNode--------------------------------------------------------------

void ReceiverNode::fillLiterals(ConstantsTable* constantTable) {
    switch (kind) {
        case ReceiverKind::EXPR:
            if (expr) {
                expr->fillLiterals(constantTable);
            }
            break;
        case ReceiverKind::CLASS_NAME:
            break;
        case ReceiverKind::SUPER:
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------



//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

void MsgArgListNode::fillLiterals(ConstantsTable* constantTable) {
    if (msgArgs) {
        for (auto arg : *msgArgs) {
            if (arg->getArg()) {
                arg->getArg()->fillLiterals(constantTable);
            }
        }
    }
}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

void MsgSelectorNode::fillLiterals(ConstantsTable* constantTable) {
    switch (kind) {
        case MsgSelectorKind::SIMPLE_SEL:
            break;
        case MsgSelectorKind::ARGUMENT_LIST:
            if (argList) {
                argList->fillLiterals(constantTable);
            }
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

void ExprListNode::fillLiterals(ConstantsTable* constantTable) {
    if (exprList) {
        for (auto expr : *exprList) {
            expr->fillLiterals(constantTable);
        }
    }
}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

void ExprNode::fillLiterals(ConstantsTable* constantTable) {
    switch (kind) {
        case ExprKind::LITERAL:
            if (literalValue) {
                literalValue->fillLiterals(constantTable);
            }
            break;
        case ExprKind::OBJC_ARRAY_LITERAL:
            if (objcArrayExprList && objcArrayExprList->getExprList()) {
                for (auto expr : *objcArrayExprList->getExprList()) {
                    expr->fillLiterals(constantTable);
                }
            }
            break;
        case ExprKind::OBJC_BOXED_EXPR:
        case ExprKind::BOXED_EXPR:
            if (boxedExpr) {
                boxedExpr->fillLiterals(constantTable);
            }
            break;
        case ExprKind::IDENTIFIER:
            break;
        case ExprKind::UNARY_MINUS:
        case ExprKind::NOT:
        case ExprKind::POST_INC:
        case ExprKind::POST_DEC:
            if (operand) {
                operand->fillLiterals(constantTable);
            }
            break;
        case ExprKind::ADDITION:
        case ExprKind::SUBTRACTION:
        case ExprKind::MULTIPLICATION:
        case ExprKind::DIVISION:
        case ExprKind::EQUAL:
        case ExprKind::NOT_EQUAL:
        case ExprKind::GREATER:
        case ExprKind::LESS:
        case ExprKind::LESS_OR_EQUAL:
        case ExprKind::GREATER_OR_EQUAL:
        case ExprKind::AND:
        case ExprKind::OR:
        case ExprKind::ASSIGN:
            if (left) left->fillLiterals(constantTable);
            if (right) right->fillLiterals(constantTable);
            break;
        case ExprKind::ARRAY_ACCESS:
            if (operand) operand->fillLiterals(constantTable);
            if (index) index->fillLiterals(constantTable);
            break;
        case ExprKind::FUNCTION_CALL:
            if (args && args->getExprList()) {
                for (auto expr : *args->getExprList()) {
                    expr->fillLiterals(constantTable);
                }
            }
            break;
        case ExprKind::DOT:
        case ExprKind::ARROW:
            if (left) left->fillLiterals(constantTable);
            if (right) right->fillLiterals(constantTable);
            break;
        case ExprKind::MESSAGE:
            if (receiver) {
                if (receiver->getExpr()) {
                    receiver->getExpr()->fillLiterals(constantTable);
                }
            }
            if (selector) {
                if (selector->getKind() == MsgSelectorKind::ARGUMENT_LIST &&
                    selector->getMsgArgList() &&
                    selector->getMsgArgList()->getMsgArgList()) {
                    for (auto arg : *selector->getMsgArgList()->getMsgArgList()) {
                        if (arg->getArg()) {
                            arg->getArg()->fillLiterals(constantTable);
                        }
                    }
                }
            }
            break;
        default:
            break;
    }
}

void ExprNode::fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement) {
    switch (kind) {
        case ExprKind::IDENTIFIER: {
            string name = *identifier->getIdentifier();
            if (classTableElement && classTableElement->isContainsField(name)) {
                string descriptor;
                string className;
                FieldsTableElement* field = classTableElement->getFieldForRef(name, &descriptor, &className);
                if (field) {
                    int fieldRef = constantTable->findOrAddFieldRefConstant(
                        className, name, descriptor);
                    setFieldRefConstantId(fieldRef);
                    setIsFieldAccess(true);
                    setClassName(className);
                }
            }
            break;
        }
        case ExprKind::DOT:
        case ExprKind::ARROW:
            if (left) left->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (right) right->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case ExprKind::ASSIGN:
            if (left) left->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (right) right->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        default:
            if (left) left->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (right) right->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (operand) operand->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (index) index->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (args) {
                if (args->getExprList()) {
                    for (auto expr : *args->getExprList()) {
                        expr->fillFieldRefs(constantTable, localVariables, classTableElement);
                    }
                }
            }
            break;
    }
}

void ExprNode::fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance) {
    switch (kind) {
        case ExprKind::FUNCTION_CALL: {
            string methodName = *funcId->getIdentifier();
            if (classTableElement && classTableElement->isContainsMethod(methodName)) {
                string descriptor;
                string className;
                MethodsTableElement* method = classTableElement->getMethodForRef(
                    methodName, &descriptor, &className);
                if (method) {
                    int methodRef = constantTable->findOrAddMethodRefConstant(
                        className, methodName, descriptor);
                    setMethodRefConstantId(methodRef);
                    setIsMethodCall(true);
                    setClassName(className);
                }
            }
            if (args && args->getExprList()) {
                for (auto expr : *args->getExprList()) {
                    expr->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
                }
            }
            break;
        }
        case ExprKind::MESSAGE: {
            if (selector) {
                string selectorName;
                if (selector->getKind() == MsgSelectorKind::SIMPLE_SEL) {
                    selectorName = *selector->getIdentifier()->getIdentifier();
                }
                
                if (classTableElement) {
                    string descriptor;
                    string className;
                    MethodsTableElement* method = classTableElement->getMethodForRef(
                        selectorName, &descriptor, &className);
                    if (method) {
                        int methodRef = constantTable->findOrAddMethodRefConstant(
                            className, selectorName, descriptor);
                        setMethodRefConstantId(methodRef);
                        setIsMethodCall(true);
                        setClassName(className);
                    }
                }
            }
            break;
        }
        default:
            if (left) left->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            if (right) right->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            if (operand) operand->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            if (args && args->getExprList()) {
                for (auto expr : *args->getExprList()) {
                    expr->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
                }
            }
            break;
    }
}

void ExprNode::semanticTransform(LocalVariablesTable* localVariables) {
    switch (kind) {
        case ExprKind::LITERAL: {
            if (literalValue) {
                switch (literalValue->getValueKind()) {
                    case ValueKind::INT_LIT:
                        setType(new Type(TypeKind::INT));
                        break;
                    case ValueKind::FLOAT_LIT:
                        setType(new Type(TypeKind::FLOAT));
                        break;
                    case ValueKind::BOOL_LIT:
                        setType(new Type(TypeKind::BOOL));
                        break;
                    case ValueKind::CHAR_LIT:
                        setType(new Type(TypeKind::CHAR));
                        break;
                    case ValueKind::STRING_LIT:
                        setType(new Type(TypeKind::CLASS_NAME, "java/lang/String"));
                        break;
                    default:
                        break;
                }
            }
            break;
        }
        case ExprKind::IDENTIFIER: {
            string name = *identifier->getIdentifier();
            if (localVariables->isContains(name)) {
                identifier->setIsLocalVar(true);
                identifier->setLocalVarId(localVariables->items[name]->id);
                setType(localVariables->items[name]->type);
            } else {
                // Это может быть поле или что-то еще
                // Тип будет установлен в fillFieldRefs
            }
            break;
        }
        case ExprKind::ADDITION:
        case ExprKind::SUBTRACTION:
        case ExprKind::MULTIPLICATION:
        case ExprKind::DIVISION: {
            if (left) left->semanticTransform(localVariables);
            if (right) right->semanticTransform(localVariables);
            
            Type* leftType = left ? left->getExprType() : nullptr;
            Type* rightType = right ? right->getExprType() : nullptr;
            
            if (leftType && rightType) {
                if (leftType->dataType == TypeKind::INT && rightType->dataType == TypeKind::INT) {
                    setType(new Type(TypeKind::INT));
                } else if ((leftType->dataType == TypeKind::INT || leftType->dataType == TypeKind::FLOAT) &&
                          (rightType->dataType == TypeKind::INT || rightType->dataType == TypeKind::FLOAT)) {
                    setType(new Type(TypeKind::FLOAT));
                } else {
                    throw std::runtime_error("Incompatible types for arithmetic operation");
                }
            }
            break;
        }
        case ExprKind::ASSIGN: {
            if (left) left->semanticTransform(localVariables);
            if (right) right->semanticTransform(localVariables);
            
            Type* leftType = left ? left->getExprType() : nullptr;
            Type* rightType = right ? right->getExprType() : nullptr;
            
            if (leftType && rightType) {
                if (!rightType->isCastableTo(leftType)) {
                    throw std::runtime_error("Incompatible types in assignment");
                }
                setType(leftType);
            }
            break;
        }
        case ExprKind::EQUAL:
        case ExprKind::NOT_EQUAL:
        case ExprKind::GREATER:
        case ExprKind::LESS:
        case ExprKind::LESS_OR_EQUAL:
        case ExprKind::GREATER_OR_EQUAL: {
            if (left) left->semanticTransform(localVariables);
            if (right) right->semanticTransform(localVariables);
            
            Type* leftType = left ? left->getExprType() : nullptr;
            Type* rightType = right ? right->getExprType() : nullptr;
            
            if (leftType && rightType) {
                if (!leftType->isCastableTo(rightType) && !rightType->isCastableTo(leftType)) {
                    throw std::runtime_error("Incompatible types for comparison");
                }
                setType(new Type(TypeKind::BOOL));
            }
            break;
        }
        case ExprKind::AND:
        case ExprKind::OR: {
            if (left) left->semanticTransform(localVariables);
            if (right) right->semanticTransform(localVariables);
            
            Type* leftType = left ? left->getExprType() : nullptr;
            Type* rightType = right ? right->getExprType() : nullptr;
            
            if (leftType && rightType) {
                if (leftType->dataType != TypeKind::BOOL || rightType->dataType != TypeKind::BOOL) {
                    throw std::runtime_error("Logical operations require boolean operands");
                }
                setType(new Type(TypeKind::BOOL));
            }
            break;
        }
        case ExprKind::FUNCTION_CALL: {
            if (args && args->getExprList()) {
                for (auto expr : *args->getExprList()) {
                    expr->semanticTransform(localVariables);
                }
            }
            break;
        }
        case ExprKind::ARRAY_ACCESS: {
            if (operand) operand->semanticTransform(localVariables);
            if (index) index->semanticTransform(localVariables);
            
            Type* indexType = index ? index->getExprType() : nullptr;
            if (indexType && indexType->dataType != TypeKind::INT) {
                throw std::runtime_error("Array index must be integer");
            }
            break;
        }
        default:
            if (left) left->semanticTransform(localVariables);
            if (right) right->semanticTransform(localVariables);
            if (operand) operand->semanticTransform(localVariables);
            if (index) index->semanticTransform(localVariables);
            if (args && args->getExprList()) {
                for (auto expr : *args->getExprList()) {
                    expr->semanticTransform(localVariables);
                }
            }
            break;
    }
}

void ExprNode::processObjcMessage(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance) {
    if (kind != ExprKind::MESSAGE || !receiver || !selector) return;

    if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
        receiver->getExpr()->semanticTransform(localVariables);
        Type* receiverType = receiver->getExpr()->getExprType();
        if (receiverType && receiverType->dataType == TypeKind::CLASS_NAME) {
            string className = receiverType->className;
            ClassesTable::getFullClassName(className);
        }
    }
    
    if (selector->getKind() == MsgSelectorKind::ARGUMENT_LIST && 
        selector->getMsgArgList() && 
        selector->getMsgArgList()->getMsgArgList()) {
        
        for (auto arg : *selector->getMsgArgList()->getMsgArgList()) {
            if (arg->getArg()) {
                arg->getArg()->semanticTransform(localVariables);
            }
        }
    }
}

void ExprNode::checkTypeCompatibility(Type* leftType, Type* rightType, const string& operation) {
    if (!leftType || !rightType) {
        throw std::runtime_error("Type error in " + operation + ": missing type");
    }
    
    if (kind == ExprKind::ADDITION || kind == ExprKind::SUBTRACTION || kind == ExprKind::MULTIPLICATION || kind == ExprKind::DIVISION) {
        bool leftIsNumeric = (leftType->dataType == TypeKind::INT || 
                              leftType->dataType == TypeKind::FLOAT ||
                              leftType->dataType == TypeKind::CHAR);
        bool rightIsNumeric = (rightType->dataType == TypeKind::INT || 
                               rightType->dataType == TypeKind::FLOAT ||
                               rightType->dataType == TypeKind::CHAR);
        
        if (!leftIsNumeric || !rightIsNumeric) {
            throw std::runtime_error("Non-numeric types in arithmetic operation");
        }
    }
    
    if (kind == ExprKind::EQUAL || kind == ExprKind::NOT_EQUAL || kind == ExprKind::GREATER || kind == ExprKind::LESS || kind == ExprKind::GREATER_OR_EQUAL || kind == ExprKind::LESS_OR_EQUAL) {
        if (!leftType->isCastableTo(rightType) && !rightType->isCastableTo(leftType)) {
            throw std::runtime_error("Incomparable types in " + operation);
        }
    }
    
    if (kind == ExprKind::AND || kind == ExprKind::OR) {
        if (leftType->dataType != TypeKind::BOOL || rightType->dataType != TypeKind::BOOL) {
            throw std::runtime_error("Non-boolean types in logical operation");
        }
    }
}

//--------------------------------------------------------------TypeNode--------------------------------------------------------------

void TypeNode::fillLiterals(ConstantsTable* constantTable) {
    if (kind == TypeKind::CLASS_NAME && classNameValue) {
        string className = *classNameValue->getClassName();
        constantTable->findOrAddConstant(ConstantType::Utf8, className);
        constantTable->findOrAddConstant(ConstantType::Class, 0, 
            constantTable->findOrAddConstant(ConstantType::Utf8, className));
    }
}

//--------------------------------------------------------------DeclaratorListNode--------------------------------------------------------------

void DeclaratorListNode::fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, TypeNode* typeNode) {
    if (initDeclList && typeNode) {
        for (auto initDecl : *initDeclList) {
            if (classTableElement) {
                initDecl->fillTables(constantTable, classTableElement, typeNode);
            } else if (localVariables) {
                if (initDecl->getInitializer()) {
                    initDecl->getInitializer()->fillTables(constantTable);
                }
            }
        }
    }
}

void DeclaratorListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (initDeclList) {
        for (auto initDecl : *initDeclList) {
            if (initDecl->getInitializer()) {
                initDecl->getInitializer()->semanticTransform(localVariables);
            }
        }
    }
}

//--------------------------------------------------------------DeclNode--------------------------------------------------------------

void DeclNode::fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement) {
    if (declaratorList && type) {
        if (localVariables) {
            if (declaratorList->getInitDeclList()) {
                for (auto initDecl : *declaratorList->getInitDeclList()) {
                    if (initDecl->getDeclarator() && 
                        initDecl->getDeclarator()->getIdentifier()) {
                        
                        string varName = *initDecl->getDeclarator()->getIdentifier()->getIdentifier();
                        Type* varType = convertTypeNodeToType(type);
                        
                        if (initDecl->getDeclarator()->getArraySizes()) {
                            int dimensionCount = initDecl->getDeclarator()->getArraySizes()->size();
                            if (dimensionCount > 0) {
                                ExprNode* firstSize = initDecl->getDeclarator()->getArraySizes()->front();
                                varType = new Type(varType->dataType, varType->className, firstSize);
                            }
                        }
                        
                        localVariables->findOrAddLocalVariable(varName, varType);
                    }
                }
            }
        }
        declaratorList->fillTables(constantTable, localVariables, classTableElement, type);
    }
}

void DeclNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (declaratorList) {
        declaratorList->semanticTransform(localVariables);
    }
}

//--------------------------------------------------------------StmtListNode--------------------------------------------------------------

void StmtListNode::fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement) {
    if (stmts) {
        for (auto stmt : *stmts) {
            stmt->fillFieldRefs(constantTable, localVariables, classTableElement);
        }
    }
}

void StmtListNode::fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance) {
    if (stmts) {
        for (auto stmt : *stmts) {
            stmt->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
        }
    }
}

void StmtListNode::fillLiterals(ConstantsTable* constantTable) {
    if (stmts) {
        for (auto stmt : *stmts) {
            stmt->fillLiterals(constantTable);
        }
    }
}

void StmtListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (stmts) {
        for (auto stmt : *stmts) {
            stmt->semanticTransform(localVariables);
        }
    }
}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

void StmtNode::fillFieldRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement) {
    switch (kind) {
        case StmtKind::EXPR:
            if (expr) expr->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case StmtKind::RETURN:
            if (expr) expr->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case StmtKind::IF:
        case StmtKind::IF_ELSE:
            if (condition) condition->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (thenBranch) thenBranch->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (elseBranch) elseBranch->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case StmtKind::FOR_WITH_EXPR:
            if (expr) expr->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (condition) condition->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (post) post->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (body) body->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case StmtKind::WHILE:
        case StmtKind::DO_WHILE:
            if (condition) condition->fillFieldRefs(constantTable, localVariables, classTableElement);
            if (body) body->fillFieldRefs(constantTable, localVariables, classTableElement);
            break;
        case StmtKind::COMPOUND:
            if (compound && compound->getStmtList()) {
                for (auto stmt : *compound->getStmtList()) {
                    stmt->fillFieldRefs(constantTable, localVariables, classTableElement);
                }
            }
            break;
        case StmtKind::DECLARATION:
            if (decl && decl->getDeclaratorList() && decl->getDeclaratorList()->getInitDeclList()) {
                for (auto initDecl : *decl->getDeclaratorList()->getInitDeclList()) {
                    if (initDecl->getInitializer()) {
                        // Рекурсивно обрабатываем инициализаторы
                    }
                }
            }
            break;
        default:
            break;
    }
}

void StmtNode::fillMethodRefs(ConstantsTable* constantTable, LocalVariablesTable* localVariables, ClassesTableElement* classTableElement, bool isInstance) {
    switch (kind) {
        case StmtKind::EXPR:
            if (expr) expr->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            break;
        case StmtKind::RETURN:
            if (expr) expr->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            break;
        case StmtKind::IF:
        case StmtKind::IF_ELSE:
            if (condition) condition->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            if (thenBranch) thenBranch->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            if (elseBranch) elseBranch->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
            break;
        case StmtKind::COMPOUND:
            if (compound && compound->getStmtList()) {
                for (auto stmt : *compound->getStmtList()) {
                    stmt->fillMethodRefs(constantTable, localVariables, classTableElement, isInstance);
                }
            }
            break;
        default:
            break;
    }
}

void StmtNode::fillLiterals(ConstantsTable* constantTable) {
    switch (kind) {
        case StmtKind::EXPR:
            if (expr) expr->fillLiterals(constantTable);
            break;
        case StmtKind::RETURN:
            if (expr) expr->fillLiterals(constantTable);
            break;
        case StmtKind::IF:
        case StmtKind::IF_ELSE:
            if (condition) condition->fillLiterals(constantTable);
            if (thenBranch) thenBranch->fillLiterals(constantTable);
            if (elseBranch) elseBranch->fillLiterals(constantTable);
            break;
        case StmtKind::COMPOUND:
            if (compound && compound->getStmtList()) {
                for (auto stmt : *compound->getStmtList()) {
                    stmt->fillLiterals(constantTable);
                }
            }
            break;
        default:
            break;
    }
}

void StmtNode::semanticTransform(LocalVariablesTable* localVariables) {
    switch (kind) {
        case StmtKind::RETURN: {
            if (expr) {
                expr->semanticTransform(localVariables);
            }
            break;
        }
        case StmtKind::IF:
        case StmtKind::IF_ELSE: {
            if (condition) {
                condition->semanticTransform(localVariables);
                Type* condType = condition->getExprType();
                if (condType && condType->dataType != TypeKind::BOOL) {
                    throw std::runtime_error("Condition must be boolean");
                }
            }
            if (thenBranch) thenBranch->semanticTransform(localVariables);
            if (elseBranch) elseBranch->semanticTransform(localVariables);
            break;
        }
        case StmtKind::COMPOUND: {
            if (compound && compound->getStmtList()) {
                for (auto stmt : *compound->getStmtList()) {
                    stmt->semanticTransform(localVariables);
                }
            }
            break;
        }
        case StmtKind::DECLARATION: {
            if (decl) {
                TypeNode* typeNode = decl->getType();
                if (typeNode && decl->getDeclaratorList() && decl->getDeclaratorList()->getInitDeclList()) {
                    for (auto initDecl : *decl->getDeclaratorList()->getInitDeclList()) {
                        if (initDecl->getDeclarator() && initDecl->getDeclarator()->getIdentifier()) {
                            string varName = *initDecl->getDeclarator()->getIdentifier()->getIdentifier();
                            Type* varType = nullptr;
                            switch (typeNode->getKind()) {
                                case TypeKind::INT:
                                    varType = new Type(TypeKind::INT);
                                    break;
                                case TypeKind::FLOAT:
                                    varType = new Type(TypeKind::FLOAT);
                                    break;
                                case TypeKind::BOOL:
                                    varType = new Type(TypeKind::BOOL);
                                    break;
                                case TypeKind::CHAR:
                                    varType = new Type(TypeKind::CHAR);
                                    break;
                                case TypeKind::CLASS_NAME:
                                    varType = new Type(TypeKind::CLASS_NAME, 
                                                     *typeNode->getClassName()->getClassName());
                                    break;
                                default:
                                    break;
                            }
                            
                            if (varType) {
                                localVariables->findOrAddLocalVariable(varName, varType);
                                // Обрабатываем инициализатор, если есть
                                if (initDecl->getInitializer()) {
                                    // Проверка соответствия типов
                                }
                            }
                        }
                    }
                }
            }
            break;
        }
        default:
            break;
    }
}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

void ArraySizeSpecNode::fillLiterals(ConstantsTable* constantTable) {
    if (sizes) {
        for (auto size : *sizes) {
            size->fillLiterals(constantTable);
        }
    }
}

void ArraySizeSpecNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (sizes) {
        for (auto size : *sizes) {
            size->semanticTransform(localVariables);
            Type* sizeType = size->getExprType();
            if (sizeType && sizeType->dataType != TypeKind::INT) {
                throw std::runtime_error("Array size must be an integer");
            }
        }
    }
}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

void ParamDeclNode::fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables) {
    if (identifier) {
        string paramName = *identifier->getIdentifier();
        Type* paramType = convertTypeNodeToType(type);
        
        if (paramType) {
            localVariables->findOrAddLocalVariable(paramName, paramType);
        }
    }
}

void ParamDeclNode::semanticTransform(LocalVariablesTable* localVariables) {
    // Параметры не требуют семантических преобразований
}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

void ParamListNode::fillTables(ConstantsTable* constantTable, LocalVariablesTable* localVariables) {
    if (paramList) {
        for (auto param : *paramList) {
            param->fillTables(constantTable, localVariables);
        }
    }
}

void ParamListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (paramList) {
        for (auto param : *paramList) {
            if (param->getType()) {
                Type* paramType = convertTypeNodeToType(param->getType());
                if (!paramType->isPrimitive() && paramType->dataType != TypeKind::CLASS_NAME) {
                    throw std::runtime_error("Invalid parameter type");
                }
            }
        }
    }
}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

void FuncDefNode::fillTables() {
    string funcName = *identifier->getIdentifier();
    Type* returnType = convertTypeNodeToType(type);
    
    LocalVariablesTable* localVariables = new LocalVariablesTable();
    
    if (paramList && paramList->getParamList()) {
        for (auto param : *paramList->getParamList()) {
            string paramName = *param->getIdentifier()->getIdentifier();
            Type* paramType = convertTypeNodeToType(param->getType());
            if (paramType) {
                localVariables->findOrAddLocalVariable(paramName, paramType);
            }
        }
    }
    
    string descriptor = "(";
    vector<Type*>* paramsTypes = new vector<Type*>();
    
    if (paramList && paramList->getParamList()) {
        for (auto param : *paramList->getParamList()) {
            Type* paramType = convertTypeNodeToType(param->getType());
            if (paramType) {
                paramsTypes->push_back(paramType);
                descriptor += paramType->getDescriptor();
            }
        }
    }
    descriptor += ")" + returnType->getDescriptor();
    
    ClassesTableElement* programClass = ClassesTable::items["rtl/Program"];
    ConstantsTable* constantTable = programClass->constantTable;
    
    FunctionsTableElement* funcElement = FunctionsTable::addFunction(
        funcName,
        descriptor,
        compoundStmt,
        paramsTypes,
        returnType
    );
    
    funcElement->localVariables = localVariables;
    
    if (compoundStmt && constantTable) {
        compoundStmt->fillLiterals(constantTable);
        compoundStmt->fillFieldRefs(constantTable, localVariables, programClass);
        compoundStmt->fillMethodRefs(constantTable, localVariables, programClass, false);
        compoundStmt->semanticTransform(localVariables);
    }
}

void FuncDefNode::semanticTransform() {
    // Семантические преобразования выполняются на уровне FunctionsTable
}

//--------------------------------------------------------------FuncDeclNode--------------------------------------------------------------

void FuncDeclNode::fillTables() {
    // Функциональные объявления не требуют заполнения таблиц в текущей архитектуре
}

//--------------------------------------------------------------MethodParamNode--------------------------------------------------------------

void MethodParamNode::fillLiterals(ConstantsTable* constantTable) {
    if (arraySizeSpec) {
        arraySizeSpec->fillLiterals(constantTable);
    }
}

void MethodParamNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (arraySizeSpec) {
        arraySizeSpec->semanticTransform(localVariables);
    }
}

//--------------------------------------------------------------MethodSelNode--------------------------------------------------------------

void MethodSelNode::fillLiterals(ConstantsTable* constantTable) {
    if (methodParams) {
        for (auto param : *methodParams) {
            param->fillLiterals(constantTable);
        }
    }
}

//--------------------------------------------------------------MethodDefNode--------------------------------------------------------------

void MethodDefNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    LocalVariablesTable* localVariables = new LocalVariablesTable();
    
    if (kind == MethodDefKind::SEL && methodSel && methodSel->getMethodParamList()) {
        for (auto param : *methodSel->getMethodParamList()) {
            string paramName = *param->getParamIdentifier()->getIdentifier();
            Type* paramType = convertTypeNodeToType(param->getType());
            if (paramType) {
                localVariables->findOrAddLocalVariable(paramName, paramType);
            }
        }
    }
    if (compoundStmt) {
        compoundStmt->semanticTransform(localVariables);
    }
}

void MethodDefNode::semanticTransform() {

}

//--------------------------------------------------------------ImplementationDefListNode--------------------------------------------------------------

void ImplementationDefListNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (classMethodDefs) {
        for (auto methodDef : *classMethodDefs) {
            methodDef->fillTables(constantTable, classTableElement);
        }
    }
    
    if (instanceMethodDefs) {
        for (auto methodDef : *instanceMethodDefs) {
            methodDef->fillTables(constantTable, classTableElement);
        }
    }
}

void ImplementationDefListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (classMethodDefs) {
        for (auto methodDef : *classMethodDefs) {
            methodDef->semanticTransform();
        }
    }
    
    if (instanceMethodDefs) {
        for (auto methodDef : *instanceMethodDefs) {
            methodDef->semanticTransform();
        }
    }
}

//--------------------------------------------------------------MethodDeclNode--------------------------------------------------------------

void MethodDeclNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    string methodName;
    string descriptor;
    vector<Type*>* paramsTypes = new vector<Type*>();
    vector<Type*>* keywordsTypes = new vector<Type*>();
    
    if (kind == MethodDeclKind::ID) {
        methodName = *identifier->getIdentifier();
        descriptor = "()" + convertTypeNodeToType(type)->getDescriptor();
    } else if (kind == MethodDeclKind::SEL && methodSel && methodSel->getMethodParamList()) {
        for (auto param : *methodSel->getMethodParamList()) {
            if (methodName.empty()) {
                methodName = *param->getSelectorIdentifier()->getIdentifier();
            } else {
                methodName += ":" + *param->getSelectorIdentifier()->getIdentifier();
            }
            
            Type* paramType = convertTypeNodeToType(param->getType());
            if (paramType) {
                keywordsTypes->push_back(paramType);
                paramsTypes->push_back(paramType);
            }
        }
        
        descriptor = "(";
        for (auto paramType : *paramsTypes) {
            descriptor += paramType->getDescriptor();
        }
        descriptor += ")" + convertTypeNodeToType(type)->getDescriptor();
    }
    
    classTableElement->methods->addMethod(
        constantTable,
        methodName,
        descriptor,
        !isInstanceMethodFlag,
        nullptr,
        convertTypeNodeToType(type),
        paramsTypes,
        keywordsTypes
    );
}

//--------------------------------------------------------------PropertyNode--------------------------------------------------------------

void PropertyNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (name && type) {
        string propName = *name->getIdentifier();
        Type* propType = convertTypeNodeToType(type);
        string descriptor = propType->getDescriptor();
        bool isReadonly = (attribute == Attribute::READONLY);
        
        classTableElement->properties->addProperty(
            constantTable,
            propName,
            descriptor,
            isReadonly,
            propType
        );
        
        string getterName = propName;
        string getterDescriptor = "()" + descriptor;
        
        classTableElement->methods->addMethod(
            constantTable,
            getterName,
            getterDescriptor,
            false,
            nullptr,
            propType,
            new vector<Type*>(),
            new vector<Type*>()
        );
        
        if (attribute != Attribute::READONLY) {
            string setterName = "set";
            if (!propName.empty()) {
                setterName += static_cast<char>(toupper(propName[0]));
                if (propName.length() > 1) {
                    setterName += propName.substr(1);
                }
            }
            string setterDescriptor = "(" + descriptor + ")V";
            vector<Type*>* setterParams = new vector<Type*>{propType};
            
            classTableElement->methods->addMethod(
                constantTable,
                setterName,
                setterDescriptor,
                false,
                nullptr,
                new Type(TypeKind::VOID),
                setterParams,
                new vector<Type*>()
            );
        }
    }
}

//--------------------------------------------------------------InterfaceDeclListNode--------------------------------------------------------------

void InterfaceDeclListNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (properties) {
        for (auto property : *properties) {
            property->fillTables(constantTable, classTableElement);
        }
    }
    
    if (classMethodDecls) {
        for (auto methodDecl : *classMethodDecls) {
            methodDecl->fillTables(constantTable, classTableElement);
        }
    }
    
    if (instanceMethodDecls) {
        for (auto methodDecl : *instanceMethodDecls) {
            methodDecl->fillTables(constantTable, classTableElement);
        }
    }
}

//--------------------------------------------------------------InitializerListNode--------------------------------------------------------------

void InitializerListNode::fillTables(ConstantsTable* constantTable) {
    if (initializers) {
        for (auto init : *initializers) {
            init->fillTables(constantTable);
        }
    }
}

void InitializerListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (initializers) {
        for (auto init : *initializers) {
            init->semanticTransform(localVariables);
        }
    }
}

//--------------------------------------------------------------InitializerNode--------------------------------------------------------------

void InitializerNode::fillTables(ConstantsTable* constantTable) {
    switch (kind) {
        case InitializerKind::EXPR:
            if (expr) {
                expr->fillLiterals(constantTable);
            }
            break;
        case InitializerKind::ARRAY:
            if (initList) {
                initList->fillTables(constantTable);
            }
            break;
        default:
            break;
    }
}

void InitializerNode::semanticTransform(LocalVariablesTable* localVariables) {
    switch (kind) {
        case InitializerKind::EXPR:
            if (expr) {
                expr->semanticTransform(localVariables);
            }
            break;
        case InitializerKind::ARRAY:
            if (initList) {
                initList->semanticTransform(localVariables);
            }
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------DeclaratorNode--------------------------------------------------------------

void DeclaratorNode::fillTables(ConstantsTable* constantTable) {
    if (arraySizes) {
        for (auto size : *arraySizes) {
            size->fillLiterals(constantTable);
        }
    }
}

void DeclaratorNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (arraySizes) {
        for (auto size : *arraySizes) {
            size->semanticTransform(localVariables);
            Type* sizeType = size->getExprType();
            if (sizeType && sizeType->dataType != TypeKind::INT) {
                throw std::runtime_error("Array size must be integer");
            }
        }
    }
}

//--------------------------------------------------------------InitDeclNode--------------------------------------------------------------

void InitDeclNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement, TypeNode* typeNode) {
    if (!declarator || !declarator->getIdentifier()) return;
    
    string fieldName = *declarator->getIdentifier()->getIdentifier();
    Type* fieldType = convertTypeNodeToType(typeNode);
    
    if (declarator->getArraySizes()) {
        for (auto size : *declarator->getArraySizes()) {
            size->fillLiterals(constantTable);
        }
        
        string descriptor = "";
        int dimensionCount = declarator->getArraySizes()->size();
        for (int i = 0; i < dimensionCount; i++) {
            descriptor += "[";
        }
        descriptor += fieldType->getDescriptor();
        
        fieldType = new Type(fieldType->dataType, fieldType->className, 
                           dimensionCount > 0 ? declarator->getArraySizes()->front() : nullptr);
        
        classTableElement->fields->addField(
            constantTable,
            fieldName,
            descriptor,
            true,
            fieldType,
            (initializer && (kind == InitDeclKind::INITIALIZED || kind == InitDeclKind::ARRAY_INITIALIZED)) ? 
                initializer->getExpr() : nullptr
        );
    } else {
        classTableElement->fields->addField(
            constantTable,
            fieldName,
            fieldType->getDescriptor(),
            true,
            fieldType,
            (initializer && kind == InitDeclKind::INITIALIZED) ? initializer->getExpr() : nullptr
        );
    }
    
    if (initializer) {
        initializer->fillTables(constantTable);
    }
}

void InitDeclNode::semanticTransform(LocalVariablesTable* localVariables, TypeNode* typeNode) {
    if (initializer) {
        initializer->semanticTransform(localVariables);
        
        if (initializer->getKind() == InitializerKind::EXPR && initializer->getExpr()) {
            Type* declType = convertTypeNodeToType(typeNode);
            Type* initType = initializer->getExpr()->getExprType();
            
            if (declType && initType && !initType->isCastableTo(declType)) {
                throw std::runtime_error("Type mismatch in initializer for field");
            }
        }
    }
    
    if (declarator && declarator->getArraySizes()) {
        for (auto size : *declarator->getArraySizes()) {
            size->semanticTransform(localVariables);
            Type* sizeType = size->getExprType();
            if (sizeType && sizeType->dataType != TypeKind::INT) {
                throw std::runtime_error("Array size must be integer");
            }
            
            if (size->getKind() == ExprKind::LITERAL) {
                ValueNode* value = size->getLiteral();
                if (value->getValueKind() == ValueKind::INT_LIT && 
                    value->getInt() <= 0) {
                    throw std::runtime_error("Array size must be positive");
                }
            }
        }
    }
}

//--------------------------------------------------------------AccessModifierNode--------------------------------------------------------------



//--------------------------------------------------------------InstanceVarDeclNode--------------------------------------------------------------

void InstanceVarDeclNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (initDecl) {
        initDecl->fillTables(constantTable, classTableElement, type);
    }
}

void InstanceVarDeclNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (initDecl) {
        initDecl->semanticTransform(localVariables, type);
    }
}

//--------------------------------------------------------------InstanceVarsDeclListNode--------------------------------------------------------------

void InstanceVarsDeclListNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (instanceVarDecls) {
        for (auto varDecl : *instanceVarDecls) {
            varDecl->fillTables(constantTable, classTableElement);
        }
    }
}

void InstanceVarsDeclListNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (instanceVarDecls) {
        for (auto varDecl : *instanceVarDecls) {
            varDecl->semanticTransform(localVariables);
        }
    }
}

//--------------------------------------------------------------InstanceVarsNode--------------------------------------------------------------

void InstanceVarsNode::fillTables(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (instanceVarsDeclList) {
        instanceVarsDeclList->fillTables(constantTable, classTableElement);
    }
}

void InstanceVarsNode::semanticTransform(LocalVariablesTable* localVariables) {
    if (instanceVarsDeclList) {
        instanceVarsDeclList->semanticTransform(localVariables);
    }
}

//--------------------------------------------------------------ImplementationNode--------------------------------------------------------------

void ImplementationNode::fillTables() {
    string className = *this->className->getClassName();
    ClassesTableElement* classTableElement = ClassesTable::addClass(
        className,
        superClassName ? superClassName->getClassName() : nullptr,
        true,
        this
    );
    
    if (instanceVars) {
        instanceVars->fillTables(classTableElement->constantTable, classTableElement);
    }
    
    if (implDefList) {
        implDefList->fillTables(classTableElement->constantTable, classTableElement);
    }
}

void ImplementationNode::semanticTransform() {
    string className = *this->className->getClassName();
    string fullClassName = ClassesTable::getFullClassName(className);
    ClassesTableElement* classTableElement = ClassesTable::items[fullClassName];
    
    if (instanceVars) {
        instanceVars->semanticTransform(nullptr);
    }
    
    if (implDefList) {
        implDefList->semanticTransform(nullptr);
    }
    
    classTableElement->semanticTransform();
}

//--------------------------------------------------------------InterfaceNode--------------------------------------------------------------

void InterfaceNode::fillTables() {
    string className = *this->className->getClassName();
    ClassesTableElement* classTableElement = ClassesTable::addClass(
        className,
        superClassName ? superClassName->getClassName() : nullptr,
        false,
        this
    );

    if (instanceVars) {
        instanceVars->fillTables(classTableElement->constantTable, classTableElement);
    }

    if (interfaceDeclList) {
        interfaceDeclList->fillTables(classTableElement->constantTable, classTableElement);
    }
}

//--------------------------------------------------------------ClassNameListNode--------------------------------------------------------------



//--------------------------------------------------------------ExternalDeclNode--------------------------------------------------------------

void ExternalDeclNode::fillTables() {
    switch (kind) {
        case ExternalDeclKind::INTERFACE:
            if (interface) interface->fillTables();
            break;
        case ExternalDeclKind::IMPLEMENTATION:
            if (implementation) implementation->fillTables();
            break;
        case ExternalDeclKind::FUNC_DEF:
            if (funcDef) funcDef->fillTables();
            break;
        case ExternalDeclKind::FUNC_DECL:
            if (funcDecl) funcDecl->fillTables();
            break;
        case ExternalDeclKind::CLASS_FW_DECL_LIST:
            // Forward declarations не требуют заполнения таблиц
            break;
        default:
            break;
    }
}

void ExternalDeclNode::semanticTransform() {
    switch (kind) {
        case ExternalDeclKind::INTERFACE:
            break;
        case ExternalDeclKind::IMPLEMENTATION:
            if (implementation) implementation->semanticTransform();
            break;
        case ExternalDeclKind::FUNC_DEF:
            if (funcDef) funcDef->semanticTransform();
            break;
        case ExternalDeclKind::FUNC_DECL:
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ExternalDeclListNode--------------------------------------------------------------

void ExternalDeclListNode::fillTables() {
    if (externalDeclList) {
        for (auto decl : *externalDeclList) {
            decl->fillTables();
        }
    }
}

void ExternalDeclListNode::semanticTransform() {
    if (externalDeclList) {
        for (auto decl : *externalDeclList) {
            decl->semanticTransform();
        }
    }
}

//--------------------------------------------------------------ProgramNode--------------------------------------------------------------

void ProgramNode::fillTables() {
    if (externalDeclList) {
        externalDeclList->fillTables();
    }
}

void ProgramNode::semanticTransform() {
    if (externalDeclList) {
        externalDeclList->semanticTransform();
    }
}