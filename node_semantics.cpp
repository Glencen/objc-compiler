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

void ReceiverNode::analyzeSemantics(SemanticContext& context) {
    switch (kind) {
        case ReceiverKind::EXPR:
            if (expr) {
                expr->analyzeSemantics(context);
            }
            break;
            
        case ReceiverKind::CLASS_NAME:
            if (className) {
                string classNameStr = className->getIdentifier();
                ClassInfo* cls = context.lookupClass(classNameStr);
                if (!cls) {
                    throw semantic_exception("Unknown class '" + classNameStr + "'",
                        "ReceiverNode::analyzeSemantics", -1, -1);
                }
            }
            break;
            
        case ReceiverKind::SUPER: // TODO: проверка вызова метода родительского класса
            if (!context.getCurrentClass()) {
                throw semantic_exception("'super' can only be used in a method",
                    "ReceiverNode::analyzeSemantics", -1, -1);
            }
            if (!context.getCurrentMethod()) {
                throw semantic_exception("'super' can only be used in a method",
                    "ReceiverNode::analyzeSemantics", -1, -1);
            }
            break;
            
        case ReceiverKind::NONE:
        default:
            throw semantic_exception("Invalid receiver kind",
                "ReceiverNode::analyzeSemantics", -1, -1);
    }
}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------

void MsgArgNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

void MsgArgListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

void MsgSelectorNode::analyzeSemantics(SemanticContext& context) {
    switch (kind) {
        case MsgSelectorKind::SIMPLE_SEL:
            if (!identifier) {
                throw semantic_exception("Simple selector must have an identifier",
                    "MsgSelectorNode::analyzeSemantics", -1, -1);
            }
            string idName = identifier->getIdentifier();
            if (context.isReservedName(idName)) {
                throw semantic_exception("Selector name '" + idName + "' is a reserved keyword",
                    "MsgSelectorNode::analyzeSemantics", -1, -1);
            }
            break;
            
        case MsgSelectorKind::ARGUMENT_LIST:
            if (argList) {
                argList->analyzeSemantics(context);
            }
            break;
            
        default:
            throw semantic_exception("Invalid message selector kind",
                "MsgSelectorNode::analyzeSemantics", -1, -1);
    }
}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

void ExprListNode::analyzeSemantics(SemanticContext& context) {
    if (!exprList) return;
    
    for (ExprNode* expr : *exprList) {
        if (expr) {
            expr->analyzeSemantics(context);
        }
    }
}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

void ExprNode::analyzeSemantics(SemanticContext& context) {
    switch (kind) {
        case ExprKind::IDENTIFIER:
            analyzeIdentifierSemantics(context);
            break;
            
        case ExprKind::LITERAL:
            analyzeLiteralSemantics(context);
            break;
            
        case ExprKind::OBJC_ARRAY_LITERAL:
            analyzeObjcArrayLiteralSemantics(context);
            break;
            
        case ExprKind::OBJC_BOXED_EXPR:
            analyzeObjcBoxedExprSemantics(context);
            break;
            
        case ExprKind::NIL:
            analyzeNilSemantics(context);
            break;
            
        case ExprKind::BOXED_EXPR:
            analyzeBoxedExprSemantics(context);
            break;
            
        case ExprKind::MESSAGE:
            analyzeMessageSemantics(context);
            break;
            
        case ExprKind::SELF:
            analyzeSelfSemantics(context);
            break;
            
        case ExprKind::UNARY_MINUS:
            analyzeUnaryMinusSemantics(context);
            break;
            
        case ExprKind::NOT:
            analyzeNotSemantics(context);
            break;
            
        case ExprKind::POST_INC:
            analyzePostIncSemantics(context);
            break;
            
        case ExprKind::POST_DEC:
            analyzePostDecSemantics(context);
            break;
            
        case ExprKind::ADDITION:
            analyzeAdditionSemantics(context);
            break;
            
        case ExprKind::SUBTRACTION:
            analyzeSubtractionSemantics(context);
            break;
            
        case ExprKind::MULTIPLICATION:
            analyzeMultiplicationSemantics(context);
            break;
            
        case ExprKind::DIVISION:
            analyzeDivisionSemantics(context);
            break;
            
        case ExprKind::EQUAL:
            analyzeEqualSemantics(context);
            break;
            
        case ExprKind::NOT_EQUAL:
            analyzeNotEqualSemantics(context);
            break;
            
        case ExprKind::GREATER:
            analyzeGreaterSemantics(context);
            break;
            
        case ExprKind::LESS:
            analyzeLessSemantics(context);
            break;
            
        case ExprKind::LESS_OR_EQUAL:
            analyzeLessOrEqualSemantics(context);
            break;
            
        case ExprKind::GREATER_OR_EQUAL:
            analyzeGreaterOrEqualSemantics(context);
            break;
            
        case ExprKind::AND:
            analyzeAndSemantics(context);
            break;
            
        case ExprKind::OR:
            analyzeOrSemantics(context);
            break;
            
        case ExprKind::ASSIGN:
            analyzeAssignSemantics(context);
            break;
            
        case ExprKind::ARRAY_ACCESS:
            analyzeArrayAccessSemantics(context);
            break;
            
        case ExprKind::FUNCTION_CALL:
            analyzeFunctionCallSemantics(context);
            break;
            
        case ExprKind::DOT:
            analyzeDotSemantics(context);
            break;
            
        case ExprKind::ARROW:
            analyzeArrowSemantics(context);
            break;
            
        case ExprKind::NONE:
        default:
            throw semantic_exception("Invalid expression kind",
                "ExprNode::analyzeSemantics", -1, -1);
    }
}

void ExprNode::analyzeIdentifierSemantics(SemanticContext& context) {
    if (!identifier) {
        throw semantic_exception("Identifier expression must have an identifier",
            "ExprNode::analyzeIdentifierSemantics", -1, -1);
    }
    
    string idName = identifier->getIdentifier();
    
    // Ищем переменную в текущей области видимости
    LocalVarInfo* localVar = context.lookupLocalVar(idName);
    if (localVar) {
        exprType = new Type(localVar->type);
        return;
    }
    
    // Ищем поле в текущем классе
    if (context.getCurrentClass()) {
        FieldInfo* field = context.getCurrentClass()->lookupField(idName, true);
        if (field) {
            exprType = new Type(field->type);
            isFieldAccess = true;
            className = field->declaringClass->name;
            return;
        }
    }
    
    // Ищем функцию
    FunctionInfo* func = context.lookupFunction(idName);
    if (func) {
        exprType = new Type(func->type);
        return;
    }
    
    // Ищем класс
    ClassInfo* cls = context.lookupClass(idName);
    if (cls) {
        exprType = new Type(TypeKind::CLASS_NAME, cls->name);
        return;
    }
    
    throw semantic_exception("Undeclared identifier '" + idName + "'",
        "ExprNode::analyzeIdentifierSemantics", -1, -1);
}

void ExprNode::analyzeLiteralSemantics(SemanticContext& context) {
    if (!literalValue) {
        throw semantic_exception("Literal expression must have a value",
            "ExprNode::analyzeLiteralSemantics", -1, -1);
    }
    
    // Определяем тип литерала на основе его значения
    // TODO: Вам нужно реализовать метод getLiteralType() в ValueNode
    // или определить тип по содержимому литерала
    string literalStr = literalValue->getIdentifier();
    
    // Простая эвристика для определения типа
    if (literalStr == "true" || literalStr == "false") {
        exprType = new Type(TypeKind::BOOL);
    } else if (literalStr.find('.') != string::npos || 
               literalStr.find('e') != string::npos ||
               literalStr.find('E') != string::npos) {
        // Возможно, float
        exprType = new Type(TypeKind::FLOAT);
    } else if (literalStr.size() == 3 && literalStr[0] == '\'' && literalStr[2] == '\'') {
        // Символьный литерал
        exprType = new Type(TypeKind::CHAR);
    } else if (literalStr[0] == '"') {
        // Строковый литерал
        exprType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
    } else {
        // Целочисленный литерал
        exprType = new Type(TypeKind::INT);
    }
}

void ExprNode::analyzeObjcArrayLiteralSemantics(SemanticContext& context) {
    // TODO: Реализовать анализ Objective-C array literal
    if (objcArrayExprList) {
        objcArrayExprList->analyzeSemantics(context);
    }
    // Тип - NSArray
    exprType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
}

void ExprNode::analyzeObjcBoxedExprSemantics(SemanticContext& context) {
    // TODO: Реализовать анализ Objective-C boxed expression
    if (boxedExpr) {
        boxedExpr->analyzeSemantics(context);
    }
    // Тип - NSNumber или NSValue
    exprType = new Type(TypeKind::CLASS_NAME, "rtl/NSNumber");
}

void ExprNode::analyzeNilSemantics(SemanticContext& context) {
    // nil имеет тип указателя на объект
    exprType = new Type(TypeKind::TYPE_ID);
}

void ExprNode::analyzeBoxedExprSemantics(SemanticContext& context) {
    if (!boxedExpr) {
        throw semantic_exception("Boxed expression must have an inner expression",
            "ExprNode::analyzeBoxedExprSemantics", -1, -1);
    }
    
    boxedExpr->analyzeSemantics(context);
    
    // Тип коробочного выражения зависит от типа внутреннего выражения
    // TODO: Реализовать правильное определение типа
    exprType = new Type(TypeKind::CLASS_NAME, "java/lang/Object");
}

void ExprNode::analyzeMessageSemantics(SemanticContext& context) {
    if (!receiver || !selector) {
        throw semantic_exception("Message expression must have receiver and selector",
            "ExprNode::analyzeMessageSemantics", -1, -1);
    }
    
    receiver->analyzeSemantics(context);
    selector->analyzeSemantics(context);
    
    // TODO: Реализовать полную проверку сообщения Objective-C
    // Пока устанавливаем общий тип
    exprType = new Type(TypeKind::TYPE_ID);
    isMethodCall = true;
}

void ExprNode::analyzeSelfSemantics(SemanticContext& context) {
    if (!context.getCurrentClass()) {
        throw semantic_exception("'self' can only be used in a class context",
            "ExprNode::analyzeSelfSemantics", -1, -1);
    }
    
    // 'self' имеет тип текущего класса
    exprType = new Type(TypeKind::CLASS_NAME, context.getCurrentClass()->name);
}

void ExprNode::analyzeUnaryMinusSemantics(SemanticContext& context) {
    if (!operand) {
        throw semantic_exception("Unary minus must have an operand",
            "ExprNode::analyzeUnaryMinusSemantics", -1, -1);
    }
    
    operand->analyzeSemantics(context);
    
    // Проверяем, что операнд числового типа
    if (!operand->getExprType() || !operand->getExprType()->isNumeric()) {
        throw semantic_exception("Unary minus operand must be numeric",
            "ExprNode::analyzeUnaryMinusSemantics", -1, -1,
            "Got type: " + operand->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*operand->getExprType());
}

void ExprNode::analyzeNotSemantics(SemanticContext& context) {
    if (!operand) {
        throw semantic_exception("Not operator must have an operand",
            "ExprNode::analyzeNotSemantics", -1, -1);
    }
    
    operand->analyzeSemantics(context);
    
    // Проверяем, что операнд логического типа
    Type boolType(TypeKind::BOOL);
    if (!operand->getExprType() || !operand->getExprType()->equal(&boolType)) {
        throw semantic_exception("Not operator operand must be boolean",
            "ExprNode::analyzeNotSemantics", -1, -1,
            "Got type: " + operand->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzePostIncSemantics(SemanticContext& context) {
    if (!operand) {
        throw semantic_exception("Post-increment must have an operand",
            "ExprNode::analyzePostIncSemantics", -1, -1);
    }
    
    operand->analyzeSemantics(context);
    
    // Проверяем, что операнд числового типа и l-value
    if (!operand->getExprType() || !operand->getExprType()->isNumeric()) {
        throw semantic_exception("Post-increment operand must be numeric",
            "ExprNode::analyzePostIncSemantics", -1, -1,
            "Got type: " + operand->getExprType()->getDescriptor());
    }
    
    // TODO: Проверить, что операнд является l-value
    
    exprType = new Type(*operand->getExprType());
}

void ExprNode::analyzePostDecSemantics(SemanticContext& context) {
    if (!operand) {
        throw semantic_exception("Post-decrement must have an operand",
            "ExprNode::analyzePostDecSemantics", -1, -1);
    }
    
    operand->analyzeSemantics(context);
    
    // Проверяем, что операнд числового типа и l-value
    if (!operand->getExprType() || !operand->getExprType()->isNumeric()) {
        throw semantic_exception("Post-decrement operand must be numeric",
            "ExprNode::analyzePostDecSemantics", -1, -1,
            "Got type: " + operand->getExprType()->getDescriptor());
    }
    
    // TODO: Проверить, что операнд является l-value
    
    exprType = new Type(*operand->getExprType());
}

void ExprNode::analyzeAdditionSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Addition must have left and right operands",
            "ExprNode::analyzeAdditionSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of addition must be numeric",
            "ExprNode::analyzeAdditionSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of addition must be numeric",
            "ExprNode::analyzeAdditionSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    // Определяем общий тип для числовой операции
    unique_ptr<Type> commonType = context.commonType(*left->getExprType(), *right->getExprType());
    if (!commonType) {
        throw semantic_exception("Incompatible types in addition",
            "ExprNode::analyzeAdditionSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*commonType);
}

void ExprNode::analyzeSubtractionSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Subtraction must have left and right operands",
            "ExprNode::analyzeSubtractionSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of subtraction must be numeric",
            "ExprNode::analyzeSubtractionSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of subtraction must be numeric",
            "ExprNode::analyzeSubtractionSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    // Определяем общий тип для числовой операции
    unique_ptr<Type> commonType = context.commonType(*left->getExprType(), *right->getExprType());
    if (!commonType) {
        throw semantic_exception("Incompatible types in subtraction",
            "ExprNode::analyzeSubtractionSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*commonType);
}

void ExprNode::analyzeMultiplicationSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Multiplication must have left and right operands",
            "ExprNode::analyzeMultiplicationSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of multiplication must be numeric",
            "ExprNode::analyzeMultiplicationSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of multiplication must be numeric",
            "ExprNode::analyzeMultiplicationSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    // Определяем общий тип для числовой операции
    unique_ptr<Type> commonType = context.commonType(*left->getExprType(), *right->getExprType());
    if (!commonType) {
        throw semantic_exception("Incompatible types in multiplication",
            "ExprNode::analyzeMultiplicationSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*commonType);
}

void ExprNode::analyzeDivisionSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Division must have left and right operands",
            "ExprNode::analyzeDivisionSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of division must be numeric",
            "ExprNode::analyzeDivisionSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of division must be numeric",
            "ExprNode::analyzeDivisionSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    // Определяем общий тип для числовой операции
    unique_ptr<Type> commonType = context.commonType(*left->getExprType(), *right->getExprType());
    if (!commonType) {
        throw semantic_exception("Incompatible types in division",
            "ExprNode::analyzeDivisionSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*commonType);
}

void ExprNode::analyzeEqualSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Equality comparison must have left and right operands",
            "ExprNode::analyzeEqualSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем совместимость типов
    if (!context.isConvertible(*left->getExprType(), *right->getExprType()) &&
        !context.isConvertible(*right->getExprType(), *left->getExprType())) {
        throw semantic_exception("Incompatible types in equality comparison",
            "ExprNode::analyzeEqualSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeNotEqualSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Inequality comparison must have left and right operands",
            "ExprNode::analyzeNotEqualSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем совместимость типов
    if (!context.isConvertible(*left->getExprType(), *right->getExprType()) &&
        !context.isConvertible(*right->getExprType(), *left->getExprType())) {
        throw semantic_exception("Incompatible types in inequality comparison",
            "ExprNode::analyzeNotEqualSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeGreaterSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Greater than comparison must have left and right operands",
            "ExprNode::analyzeGreaterSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of greater than comparison must be numeric",
            "ExprNode::analyzeGreaterSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of greater than comparison must be numeric",
            "ExprNode::analyzeGreaterSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeLessSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Less than comparison must have left and right operands",
            "ExprNode::analyzeLessSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of less than comparison must be numeric",
            "ExprNode::analyzeLessSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of less than comparison must be numeric",
            "ExprNode::analyzeLessSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeLessOrEqualSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Less than or equal comparison must have left and right operands",
            "ExprNode::analyzeLessOrEqualSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of less than or equal comparison must be numeric",
            "ExprNode::analyzeLessOrEqualSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of less than or equal comparison must be numeric",
            "ExprNode::analyzeLessOrEqualSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeGreaterOrEqualSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Greater than or equal comparison must have left and right operands",
            "ExprNode::analyzeGreaterOrEqualSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды числовых типов
    if (!left->getExprType() || !left->getExprType()->isNumeric()) {
        throw semantic_exception("Left operand of greater than or equal comparison must be numeric",
            "ExprNode::analyzeGreaterOrEqualSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->isNumeric()) {
        throw semantic_exception("Right operand of greater than or equal comparison must be numeric",
            "ExprNode::analyzeGreaterOrEqualSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeAndSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Logical AND must have left and right operands",
            "ExprNode::analyzeAndSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды логического типа
    Type boolType(TypeKind::BOOL);
    if (!left->getExprType() || !left->getExprType()->equal(&boolType)) {
        throw semantic_exception("Left operand of logical AND must be boolean",
            "ExprNode::analyzeAndSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->equal(&boolType)) {
        throw semantic_exception("Right operand of logical AND must be boolean",
            "ExprNode::analyzeAndSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeOrSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Logical OR must have left and right operands",
            "ExprNode::analyzeOrSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что операнды логического типа
    Type boolType(TypeKind::BOOL);
    if (!left->getExprType() || !left->getExprType()->equal(&boolType)) {
        throw semantic_exception("Left operand of logical OR must be boolean",
            "ExprNode::analyzeOrSemantics", -1, -1,
            "Got type: " + left->getExprType()->getDescriptor());
    }
    
    if (!right->getExprType() || !right->getExprType()->equal(&boolType)) {
        throw semantic_exception("Right operand of logical OR must be boolean",
            "ExprNode::analyzeOrSemantics", -1, -1,
            "Got type: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(TypeKind::BOOL);
}

void ExprNode::analyzeAssignSemantics(SemanticContext& context) {
    if (!left || !right) {
        throw semantic_exception("Assignment must have left and right operands",
            "ExprNode::analyzeAssignSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Проверяем, что левый операнд является l-value
    // TODO: Проверить, что left является l-value (идентификатор, доступ к полю, доступ к массиву)
    
    // Проверяем совместимость типов
    if (!context.isAssignable(*right->getExprType(), *left->getExprType())) {
        throw semantic_exception("Type mismatch in assignment",
            "ExprNode::analyzeAssignSemantics", -1, -1,
            "Left: " + left->getExprType()->getDescriptor() + 
            ", Right: " + right->getExprType()->getDescriptor());
    }
    
    exprType = new Type(*left->getExprType());
}

void ExprNode::analyzeArrayAccessSemantics(SemanticContext& context) {
    if (!operand || !index) {
        throw semantic_exception("Array access must have array and index expressions",
            "ExprNode::analyzeArrayAccessSemantics", -1, -1);
    }
    
    operand->analyzeSemantics(context);
    index->analyzeSemantics(context);
    
    // Проверяем, что операнд является массивом
    if (!operand->getExprType() || !operand->getExprType()->isArray()) {
        throw semantic_exception("Array access operand must be an array",
            "ExprNode::analyzeArrayAccessSemantics", -1, -1,
            "Got type: " + operand->getExprType()->getDescriptor());
    }
    
    // Проверяем, что индекс целочисленный
    Type intType(TypeKind::INT);
    if (!index->getExprType() || !index->getExprType()->equal(&intType)) {
        throw semantic_exception("Array index must be integer",
            "ExprNode::analyzeArrayAccessSemantics", -1, -1,
            "Got type: " + index->getExprType()->getDescriptor());
    }
    
    // Тип результата - тип элемента массива
    Type elemType(operand->getExprType()->dataType, operand->getExprType()->className);
    exprType = new Type(elemType);
}

void ExprNode::analyzeFunctionCallSemantics(SemanticContext& context) {
    if (!funcId) {
        throw semantic_exception("Function call must have a function identifier",
            "ExprNode::analyzeFunctionCallSemantics", -1, -1);
    }
    
    string funcName = funcId->getIdentifier();
    FunctionInfo* func = context.lookupFunction(funcName);
    
    if (!func) {
        throw semantic_exception("Undefined function '" + funcName + "'",
            "ExprNode::analyzeFunctionCallSemantics", -1, -1);
    }
    
    // Анализируем аргументы
    vector<const Type*> argTypes;
    if (args) {
        args->analyzeSemantics(context);
        
        auto exprList = args->getExprList();
        if (exprList) {
            for (ExprNode* arg : *exprList) {
                if (arg) {
                    arg->analyzeSemantics(context);
                    argTypes.push_back(arg->getExprType());
                }
            }
        }
    }
    
    // Проверяем количество аргументов
    if (argTypes.size() != func->getParameterCount()) {
        throw semantic_exception("Function '" + funcName + "' called with wrong number of arguments",
            "ExprNode::analyzeFunctionCallSemantics", -1, -1,
            "Expected: " + to_string(func->getParameterCount()) + 
            ", Got: " + to_string(argTypes.size()));
    }
    
    // Проверяем типы аргументов
    for (size_t i = 0; i < argTypes.size(); i++) {
        const LocalVarInfo* param = func->getParameter(i);
        if (param && !context.isAssignable(*argTypes[i], param->type)) {
            throw semantic_exception("Type mismatch in function call argument " + to_string(i + 1),
                "ExprNode::analyzeFunctionCallSemantics", -1, -1,
                "Expected: " + param->type.getDescriptor() + 
                ", Got: " + argTypes[i]->getDescriptor());
        }
    }
    
    exprType = new Type(func->getReturnType());
}

void ExprNode::analyzeDotSemantics(SemanticContext& context) {
    // TODO: Реализовать анализ операции доступа через точку (структуры/классы)
    if (!left || !right) {
        throw semantic_exception("Dot operator must have left and right operands",
            "ExprNode::analyzeDotSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Пока устанавливаем тип левого операнда
    exprType = new Type(*left->getExprType());
}

void ExprNode::analyzeArrowSemantics(SemanticContext& context) {
    // TODO: Реализовать анализ операции доступа через стрелку (указатели)
    if (!left || !right) {
        throw semantic_exception("Arrow operator must have left and right operands",
            "ExprNode::analyzeArrowSemantics", -1, -1);
    }
    
    left->analyzeSemantics(context);
    right->analyzeSemantics(context);
    
    // Пока устанавливаем тип левого операнда
    exprType = new Type(*left->getExprType());
}

Type* ExprNode::getExprType() const {
    return exprType;
}

void ExprNode::setType(Type* type) {
    if (exprType) {
        delete exprType;
    }
    exprType = type;
}

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
            analyzeExprSemantics(context);
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
            
        case StmtKind::FOR_WITH_EXPR:
            analyzeForWithExprSemantics(context);
            break;
            
        case StmtKind::FOR_WITH_DECL:
            analyzeForWithDeclSemantics(context);
            break;
            
        case StmtKind::FOR_IN:
            analyzeForInSemantics(context);
            break;
            
        case StmtKind::TYPED_FOR_IN:
            analyzeTypedForInSemantics(context);
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
            analyzeDeclarationSemantics(context);
            break;
            
        case StmtKind::NONE:
        default:
            throw statement_exception("Unknown statement type",
                "StmtNode::analyzeSemantics", -1, -1, 
                "Statement kind: " + to_string(static_cast<int>(kind)));
    }
}

void StmtNode::analyzeReturnSemantics(SemanticContext& context) {
    FunctionInfo* currentFunction = context.getCurrentFunction();
    MethodInfo* currentMethod = context.getCurrentMethod();
    
    if (!currentFunction && !currentMethod) {
        throw statement_exception("Return statement outside of function or method",
            "StmtNode::analyzeReturnSemantics", -1, -1);
    }
    
    if (expr) {
        // Есть возвращаемое значение
        expr->analyzeSemantics(context);
        
        // Тип проверяется в FuncDefNode::checkReturnStatements
    } else {
        // Нет возвращаемого значения - void return
        // Проверка будет в FuncDefNode::checkReturnStatements
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
        context.enterConditionalScope();
        elseBranch->analyzeSemantics(context);
        context.leaveScope();
    }
}

void StmtNode::analyzeForWithExprSemantics(SemanticContext& context) {
    if (expr) {
        expr->analyzeSemantics(context);
    }
    
    context.enterLoopScope();
    
    if (condition) {
        condition->analyzeSemantics(context);
        
        // Проверяем, что условие имеет булевый тип
        // TODO: Добавить проверку типа condition
    }
    
    if (post) {
        post->analyzeSemantics(context);
    }
    
    if (body) {
        body->analyzeSemantics(context);
    }
    
    context.leaveScope();
}

void StmtNode::analyzeForWithDeclSemantics(SemanticContext& context) {
    context.enterLoopScope();
    
    if (decl) {
        decl->analyzeSemantics(context);
    }
    
    if (condition) {
        condition->analyzeSemantics(context);
        
        // Проверяем, что условие имеет булевый тип
        // TODO: Добавить проверку типа condition
    }
    
    if (post) {
        post->analyzeSemantics(context);
    }
    
    if (body) {
        body->analyzeSemantics(context);
    }
    
    context.leaveScope();
}

void StmtNode::analyzeForInSemantics(SemanticContext& context) {
    if (!collection) {
        throw statement_exception("For-in statement must have a collection",
            "StmtNode::analyzeForInSemantics", -1, -1);
    }
    
    if (!forInId) {
        throw statement_exception("For-in statement must have an identifier",
            "StmtNode::analyzeForInSemantics", -1, -1);
    }
    
    collection->analyzeSemantics(context);
    
    context.enterLoopScope();
    
    // Добавляем переменную итератора в область видимости
    // TODO: Определить тип переменной итератора на основе типа коллекции
    
    if (body) {
        body->analyzeSemantics(context);
    }
    
    context.leaveScope();
}

void StmtNode::analyzeTypedForInSemantics(SemanticContext& context) {
    if (!collection) {
        throw statement_exception("Typed for-in statement must have a collection",
            "StmtNode::analyzeTypedForInSemantics", -1, -1);
    }
    
    if (!forInId) {
        throw statement_exception("Typed for-in statement must have an identifier",
            "StmtNode::analyzeTypedForInSemantics", -1, -1);
    }
    
    if (!forInType) {
        throw statement_exception("Typed for-in statement must have a type",
            "StmtNode::analyzeTypedForInSemantics", -1, -1);
    }
    
    collection->analyzeSemantics(context);
    
    context.enterLoopScope();
    
    string varName = forInId->getIdentifier();
    Type varType = convertTypeNodeToType(forInType);
    
    auto varInfo = make_unique<LocalVarInfo>(varName, varType, false, nullptr);
    if (!context.addLocalVar(move(varInfo))) {
        throw statement_exception("Failed to add for-in iterator variable '" + varName + "' to scope",
            "StmtNode::analyzeTypedForInSemantics", -1, -1, "Variable: '" + varName + "'");
    }
    
    if (body) {
        body->analyzeSemantics(context);
    }
    
    context.leaveScope();
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
    
    context.enterBlockStmtScope();
    compound->analyzeSemantics(context);
    context.leaveScope();
}

void StmtNode::analyzeExprSemantics(SemanticContext& context) {
    if (expr) {
        expr->analyzeSemantics(context);
    }
}

void StmtNode::analyzeDeclarationSemantics(SemanticContext& context) {
    if (decl) {
        decl->analyzeSemantics(context);
    }
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
    vector<StmtNode*> returnStmts;
    collectReturnStatements(compoundStmt, returnStmts);
    
    Type voidType(TypeKind::VOID);
    Type returnType = func->getReturnType();
    
    if (returnType.equal(&voidType)) {
        for (StmtNode* stmt : returnStmts) {
            if (stmt->getKind() == StmtKind::RETURN) {
                if (stmt->getExpr() != nullptr) {
                    throw function_exception("Void function '" + func->name + "' cannot return a value",
                        "FuncDefNode::checkReturnStatements", -1, -1, "Function: '" + func->name + "'");
                }
            }
        }
    } else {
        if (returnStmts.empty()) {
            throw function_exception("Function '" + func->name + "' must return a value",
                "FuncDefNode::checkReturnStatements", -1, -1, 
                "Return type: " + returnType.getDescriptor());
        }
        
        for (StmtNode* stmt : returnStmts) {
            if (stmt->getKind() == StmtKind::RETURN) {
                if (stmt->getExpr() == nullptr) {
                    throw function_exception("Function '" + func->name + "' must return a value, not void",
                        "FuncDefNode::checkReturnStatements", -1, -1,
                        "Return type: " + returnType.getDescriptor());
                }
                
                // Проверяем тип возвращаемого выражения
                // Для этого нужно сначала проанализировать выражение, если это еще не сделано
                // Но в данном случае выражение уже должно быть проанализировано
                // Проверяем совместимость типов
                if (!context.isAssignable(returnType, returnType)) {
                    // TODO: Здесь нужно получить фактический тип выражения
                    // Для этого нужно добавить метод getType() в ExprNode
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
            collectReturnStatements(stmt->getThenBranch(), returnStmts);
            break;
            
        case StmtKind::IF_ELSE:
            collectReturnStatements(stmt->getThenBranch(), returnStmts);
            collectReturnStatements(stmt->getElseBranch(), returnStmts);
            break;
            
        case StmtKind::FOR_WITH_EXPR:
        case StmtKind::FOR_WITH_DECL:
        case StmtKind::FOR_IN:
        case StmtKind::TYPED_FOR_IN:
        case StmtKind::WHILE:
        case StmtKind::DO_WHILE:
            collectReturnStatements(stmt->getBody(), returnStmts);
            break;
            
        default:
            break;
    }
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
