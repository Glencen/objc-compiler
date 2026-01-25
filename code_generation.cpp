#include "bytecode_context.h"
#include "context.h"

namespace {
TypeKind normalizeKind(TypeKind kind) {
    if (kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        return TypeKind::INT;
    }
    return kind;
}

bool isFloatType(const Type* type) {
    return type && normalizeKind(type->dataType) == TypeKind::FLOAT;
}

bool isIntLikeType(const Type* type) {
    if (!type) return true;
    TypeKind kind = normalizeKind(type->dataType);
    return kind == TypeKind::INT;
}

bool isObjectLike(const Type* type) {
    if (!type) return false;
    return type->dataType == TypeKind::CLASS_NAME || type->dataType == TypeKind::TYPE_ID;
}

void emitJumpIfFalse(BytecodeContext& context, ExprNode* expr, BytecodeContext::Label* falseLabel) {
    if (!expr) return;
    expr->emitBytecode(context);
    if (isObjectLike(expr->getExprType())) {
        context.emitJump(0xc6, falseLabel); // ifnull
    } else {
        context.emitJump(0x99, falseLabel); // ifeq
    }
}

void emitBoxIfNeeded(BytecodeContext& context, const Type* type) {
    if (!type) return;
    if (type->dataType == TypeKind::CLASS_NAME || type->dataType == TypeKind::TYPE_ID) {
        return;
    }
    if (type->dataType == TypeKind::FLOAT) {
        context.emitInvokeStatic("rtl/NSNumber", "numberWithFloatStatic", "(F)Lrtl/NSNumber;");
        return;
    }
    if (type->dataType == TypeKind::INT || type->dataType == TypeKind::BOOL || type->dataType == TypeKind::CHAR) {
        context.emitInvokeStatic("rtl/NSNumber", "numberWithIntStatic", "(I)Lrtl/NSNumber;");
    }
}

std::string mapRuntimeClassName(const std::string& name) {
    if (name.find('/') != std::string::npos) {
        return name;
    }
    if (name == "NSString" || name == "NSNumber" || name == "NSArray" ||
        name == "NSObject" || name == "InOutFuncs") {
        return "rtl/" + name;
    }
    return name;
}

uint16_t mapAccessToFlags(AccessModifier access) {
    switch (access) {
        case AccessModifier::PUBLIC: return 0x0001;
        case AccessModifier::PRIVATE: return 0x0002;
        case AccessModifier::PROTECTED: return 0x0004;
        default: return 0;
    }
}

Type mapRuntimeType(const Type& type) {
    Type mapped = type;
    if (mapped.dataType == TypeKind::CLASS_NAME) {
        mapped.className = mapRuntimeClassName(mapped.className);
    }
    return mapped;
}

std::string buildMethodNameFromDef(MethodDefNode* methodDef) {
    if (!methodDef) return "";
    if (methodDef->getIdentifier()) {
        return methodDef->getIdentifier()->getIdentifier();
    }
    MethodSelNode* sel = methodDef->getMethodSel();
    if (!sel || !sel->getMethodParamList()) return "";
    std::string methodName;
    auto* list = sel->getMethodParamList();
    for (auto* param : *list) {
        if (!param || !param->getSelectorIdentifier()) continue;
        methodName += param->getSelectorIdentifier()->getIdentifier();
        methodName += ":";
    }
    if (!methodName.empty() && methodName.back() == ':') {
        methodName.pop_back();
        methodName += ":";
    }
    return methodName;
}

std::string mangleJvmMethodName(const std::string& name) {
    std::string result = name;
    for (auto& ch : result) {
        if (ch == ':') ch = '$';
    }
    return result;
}

std::string mapRtlMethodName(const std::string& owner, const std::string& name, bool isStatic) {
    if (owner != "rtl/InOutFuncs") {
        if (name.size() >= 7 && name.compare(name.size() - 7, 7, "Dynamic") == 0) {
            return name;
        }
        if (name.size() >= 6 && name.compare(name.size() - 6, 6, "Static") == 0) {
            return name;
        }
        return name + (isStatic ? "Static" : "Dynamic");
    }
    return name;
}

void emitConditionJumpFalse(BytecodeContext& context, ExprNode* condition, BytecodeContext::Label* falseLabel) {
    if (!condition || !falseLabel) return;
    ExprKind kind = condition->getKind();
    if (kind == ExprKind::NOT && condition->getOperand()) {
        ExprNode* operand = condition->getOperand();
        operand->emitBytecode(context);
        if (isObjectLike(operand->getExprType())) {
            context.emitJump(0xc7, falseLabel); // ifnonnull
        } else {
            context.emitJump(0x9a, falseLabel); // ifne
        }
        return;
    }
    if (kind == ExprKind::AND && condition->getLeft() && condition->getRight()) {
        emitJumpIfFalse(context, condition->getLeft(), falseLabel);
        emitJumpIfFalse(context, condition->getRight(), falseLabel);
        return;
    }
    if (kind == ExprKind::OR && condition->getLeft() && condition->getRight()) {
        auto* labelTrue = context.createLabel();
        ExprNode* left = condition->getLeft();
        ExprNode* right = condition->getRight();
        left->emitBytecode(context);
        if (isObjectLike(left->getExprType())) {
            context.emitJump(0xc7, labelTrue); // ifnonnull
        } else {
            context.emitJump(0x9a, labelTrue); // ifne
        }
        emitJumpIfFalse(context, right, falseLabel);
        context.markLabel(labelTrue);
        return;
    }
    if (kind == ExprKind::EQUAL || kind == ExprKind::NOT_EQUAL ||
        kind == ExprKind::GREATER || kind == ExprKind::LESS ||
        kind == ExprKind::LESS_OR_EQUAL || kind == ExprKind::GREATER_OR_EQUAL) {
        ExprNode* left = condition->getLeft();
        ExprNode* right = condition->getRight();
        if (left && right) {
            left->emitBytecode(context);
            right->emitBytecode(context);
            bool leftObj = isObjectLike(left->getExprType());
            bool rightObj = isObjectLike(right->getExprType());
            bool isFloat = isFloatType(left->getExprType()) || isFloatType(right->getExprType());
            if (leftObj || rightObj) {
                uint8_t op = 0xa6; // if_acmpne
                if (kind == ExprKind::NOT_EQUAL) {
                    op = 0xa5; // if_acmpeq => false when equal
                }
                context.emitJump(op, falseLabel);
            } else if (isFloat) {
                context.emitFcmpl();
                uint8_t op = 0x99; // ifeq
                switch (kind) {
                    case ExprKind::EQUAL: op = 0x9a; break; // ifne => false
                    case ExprKind::NOT_EQUAL: op = 0x99; break; // ifeq => false
                    case ExprKind::GREATER: op = 0x9e; break; // ifle => false
                    case ExprKind::LESS: op = 0x9c; break; // ifge => false
                    case ExprKind::LESS_OR_EQUAL: op = 0x9d; break; // ifgt => false
                    case ExprKind::GREATER_OR_EQUAL: op = 0x9b; break; // iflt => false
                    default: break;
                }
                context.emitJump(op, falseLabel);
            } else {
                uint8_t op = 0xa0; // if_icmpne
                switch (kind) {
                    case ExprKind::EQUAL: op = 0xa0; break; // != => false
                    case ExprKind::NOT_EQUAL: op = 0x9f; break; // == => false
                    case ExprKind::GREATER: op = 0xa4; break; // <= => false
                    case ExprKind::LESS: op = 0xa2; break; // >= => false
                    case ExprKind::LESS_OR_EQUAL: op = 0xa3; break; // > => false
                    case ExprKind::GREATER_OR_EQUAL: op = 0xa1; break; // < => false
                    default: break;
                }
                context.emitJump(op, falseLabel);
            }
            return;
        }
    }
    condition->emitBytecode(context);
    if (isObjectLike(condition->getExprType())) {
        context.emitJump(0xc6, falseLabel); // ifnull
    } else {
        context.emitJump(0x99, falseLabel); // ifeq
    }
}

bool emitPostIncDecAsStatement(BytecodeContext& context, ExprNode* expr) {
    if (!expr) return false;
    ExprKind kind = expr->getKind();
    if (kind != ExprKind::POST_INC && kind != ExprKind::POST_DEC) return false;
    ExprNode* operand = expr->getOperand();
    if (!operand || operand->getKind() != ExprKind::IDENTIFIER || !operand->getIdentifier()) {
        return false;
    }
    std::string name = operand->getIdentifier()->getIdentifier();
    const auto* local = context.getLocal(name);
    if (!local || !isIntLikeType(operand->getExprType())) {
        return false;
    }
    int delta = (kind == ExprKind::POST_INC) ? 1 : -1;
    context.emitIInc(local->index, delta);
    return true;
}

Type convertTypeNodeToType(TypeNode* typeNode, const std::vector<int>& arraySizes = {}) {
    if (!typeNode) return Type(TypeKind::NONE);

    TypeKind typeKind = typeNode->getKind();
    std::string className = typeKind == TypeKind::CLASS_NAME ? mapRuntimeClassName(typeNode->getClassName()->getClassName()) : "";

    if (typeNode->isPrimitive()) {
        if (!arraySizes.empty()) {
            return Type(typeKind, arraySizes);
        }
        return Type(typeKind);
    }
    if (typeKind == TypeKind::CLASS_NAME) {
        if (!arraySizes.empty()) {
            return Type(TypeKind::CLASS_NAME, className, arraySizes);
        }
        return Type(TypeKind::CLASS_NAME, className);
    }
    if (typeKind == TypeKind::TYPE_ID) {
        if (!arraySizes.empty()) {
            return Type(TypeKind::TYPE_ID, arraySizes);
        }
        return Type(TypeKind::TYPE_ID);
    }
    if (!arraySizes.empty()) {
        return Type(typeKind, arraySizes);
    }
    return Type(TypeKind::NONE);
}

std::string buildFunctionDescriptor(TypeNode* returnTypeNode, ParamListNode* params) {
    std::string desc = "(";
    if (params && params->getParamList()) {
        for (auto* param : *params->getParamList()) {
            if (!param) continue;
            std::vector<int> sizes = param->getArraySizes();
            Type paramType = convertTypeNodeToType(param->getType(), sizes);
            desc += paramType.getDescriptor();
        }
    }
    desc += ")";
    Type retType = convertTypeNodeToType(returnTypeNode);
    desc += retType.getDescriptor();
    return desc;
}

std::string buildFunctionDescriptor(const FunctionInfo* func) {
    if (!func) return "()V";
    std::string desc = "(";
    for (size_t i = 0; i < func->getParameterCount(); ++i) {
        const LocalVarInfo* param = func->getParameter(i);
        if (!param) continue;
        desc += param->type.getDescriptor();
    }
    desc += ")";
    desc += func->getReturnType().getDescriptor();
    return desc;
}

std::string buildMethodDescriptor(const std::vector<const Type*>& argTypes, const Type& returnType) {
    std::string desc = "(";
    for (const auto* arg : argTypes) {
        if (!arg) continue;
        desc += arg->getDescriptor();
    }
    desc += ")";
    desc += returnType.getDescriptor();
    return desc;
}

void emitDefaultValue(BytecodeContext& context, const Type& type) {
    TypeKind kind = normalizeKind(type.dataType);
    if (kind == TypeKind::FLOAT) {
        context.emitFConst(0.0f);
        return;
    }
    if (kind == TypeKind::INT || kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        context.emitIConst(0);
        return;
    }
    context.emitAConstNull();
}
} // namespace

//--------------------------------------------------------------ValueNode--------------------------------------------------------------

void ValueNode::emitBytecode(BytecodeContext& context) {
    switch (valueType) {
        case ValueKind::INT_LIT:
            context.emitIConst(intValue);
            break;
        case ValueKind::FLOAT_LIT:
            context.emitFConst(floatValue);
            break;
        case ValueKind::BOOL_LIT:
            context.emitIConst(boolValue ? 1 : 0);
            break;
        case ValueKind::CHAR_LIT:
            context.emitIConst(static_cast<int>(charValue));
            break;
        case ValueKind::STRING_LIT:
        case ValueKind::OBJC_STRING_LIT: {
            context.emitLdcString(stringValue);
            context.emitInvokeStatic("rtl/NSString", "stringWithCStringStatic", "(Ljava/lang/String;)Lrtl/NSString;");
            break;
        }
        case ValueKind::OBJC_INT_LIT:
            context.emitIConst(intValue);
            context.emitInvokeStatic("rtl/NSNumber", "numberWithIntStatic", "(I)Lrtl/NSNumber;");
            break;
        case ValueKind::OBJC_FLOAT_LIT:
            context.emitFConst(floatValue);
            context.emitInvokeStatic("rtl/NSNumber", "numberWithFloatStatic", "(F)Lrtl/NSNumber;");
            break;
        case ValueKind::OBJC_BOOL_LIT:
            context.emitIConst(boolValue ? 1 : 0);
            context.emitInvokeStatic("rtl/NSNumber", "numberWithIntStatic", "(I)Lrtl/NSNumber;");
            break;
        case ValueKind::NIL:
            context.emitAConstNull();
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ReceiverNode--------------------------------------------------------------

void ReceiverNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------

void MsgArgNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

void MsgArgListNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

void MsgSelectorNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

void ExprListNode::emitBytecode(BytecodeContext& context) {
    if (!exprList) return;
    for (auto* expr : *exprList) {
        if (expr) {
            expr->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

void ExprNode::emitBytecode(BytecodeContext& context) {
    SemanticContext& sem = SemanticContext::getInstance();

    switch (kind) {
        case ExprKind::IDENTIFIER: {
            std::string name = identifier ? identifier->getIdentifier() : "";
            const auto* local = context.getLocal(name);
            if (local) {
                context.emitLoad(local->type, local->index);
                break;
            }
            std::string owner = className.empty() ? context.getClassName() : mapRuntimeClassName(className);
            FieldInfo* field = sem.lookupField(owner, name);
            if (field) {
                std::string desc = field->type.getDescriptor();
                if (field->isInstance) {
                    if (!context.isCurrentMethodStatic()) {
                        context.emitLoad(Type(TypeKind::CLASS_NAME, owner), 0);
                        context.emitGetField(owner, name, desc);
                    }
                } else {
                    context.emitGetStatic(owner, name, desc);
                }
            }
            break;
        }
        case ExprKind::LITERAL:
            if (literalValue) {
                literalValue->emitBytecode(context);
            }
            break;
        case ExprKind::OBJC_ARRAY_LITERAL: {
            ExprListNode* list = getObjcArrayExprList();
            std::vector<ExprNode*> elements;
            if (list && list->getExprList()) {
                for (auto* expr : *list->getExprList()) {
                    if (expr) elements.push_back(expr);
                }
            }
            context.emitIConst(static_cast<int>(elements.size()));
            context.emitANewArray("rtl/NSObject");
            for (size_t i = 0; i < elements.size(); ++i) {
                context.emitDup();
                context.emitIConst(static_cast<int>(i));
                elements[i]->emitBytecode(context);
                emitBoxIfNeeded(context, elements[i]->getExprType());
                context.emitAAStore();
            }
            context.emitInvokeStatic("rtl/NSArray", "arrayWithObjectsStatic", "([Lrtl/NSObject;)Lrtl/NSArray;");
            break;
        }
        case ExprKind::NIL:
            context.emitAConstNull();
            break;
        case ExprKind::SELF:
            context.emitLoad(Type(TypeKind::CLASS_NAME, context.getClassName()), 0);
            break;
        case ExprKind::UNARY_MINUS:
            if (operand) {
                operand->emitBytecode(context);
                if (operand->getExprType()) {
                    context.emitNeg(*operand->getExprType());
                }
            }
            break;
        case ExprKind::NOT: {
            if (!operand) break;
            operand->emitBytecode(context);
            auto* labelTrue = context.createLabel();
            auto* labelEnd = context.createLabel();
            context.emitJump(0x99, labelTrue); // ifeq
            context.emitIConst(0);
            context.emitJump(0xa7, labelEnd);
            context.markLabel(labelTrue);
            context.emitIConst(1);
            context.markLabel(labelEnd);
            break;
        }
        case ExprKind::POST_INC:
        case ExprKind::POST_DEC: {
            if (!operand) break;
            if (operand->getKind() == ExprKind::IDENTIFIER && operand->getIdentifier()) {
                std::string name = operand->getIdentifier()->getIdentifier();
                const auto* local = context.getLocal(name);
                if (local && isIntLikeType(operand->getExprType())) {
                    context.emitLoad(local->type, local->index);
                    context.emitDup();
                    int delta = (kind == ExprKind::POST_INC) ? 1 : -1;
                    context.emitIInc(local->index, delta);
                }
            }
            break;
        }
        case ExprKind::ADDITION:
        case ExprKind::SUBTRACTION:
        case ExprKind::MULTIPLICATION:
        case ExprKind::DIVISION: {
            if (!left || !right) break;
            left->emitBytecode(context);
            right->emitBytecode(context);
            bool isFloat = isFloatType(getExprType());
            if (isFloat) {
                switch (kind) {
                    case ExprKind::ADDITION: context.emitBinaryFloat(0x62); break;
                    case ExprKind::SUBTRACTION: context.emitBinaryFloat(0x66); break;
                    case ExprKind::MULTIPLICATION: context.emitBinaryFloat(0x6a); break;
                    case ExprKind::DIVISION: context.emitBinaryFloat(0x6e); break;
                    default: break;
                }
            } else {
                switch (kind) {
                    case ExprKind::ADDITION: context.emitBinaryInt(0x60); break;
                    case ExprKind::SUBTRACTION: context.emitBinaryInt(0x64); break;
                    case ExprKind::MULTIPLICATION: context.emitBinaryInt(0x68); break;
                    case ExprKind::DIVISION: context.emitBinaryInt(0x6c); break;
                    default: break;
                }
            }
            break;
        }
        case ExprKind::EQUAL:
        case ExprKind::NOT_EQUAL:
        case ExprKind::GREATER:
        case ExprKind::LESS:
        case ExprKind::LESS_OR_EQUAL:
        case ExprKind::GREATER_OR_EQUAL: {
            if (!left || !right) break;
            left->emitBytecode(context);
            right->emitBytecode(context);
            bool leftObj = isObjectLike(left->getExprType());
            bool rightObj = isObjectLike(right->getExprType());
            bool isFloat = isFloatType(left->getExprType()) || isFloatType(right->getExprType());
            auto* labelTrue = context.createLabel();
            auto* labelEnd = context.createLabel();
            if (leftObj || rightObj) {
                uint8_t op = 0xa5; // if_acmpeq
                switch (kind) {
                    case ExprKind::EQUAL: op = 0xa5; break;
                    case ExprKind::NOT_EQUAL: op = 0xa6; break;
                    default: op = 0xa5; break;
                }
                context.emitJump(op, labelTrue);
            } else if (isFloat) {
                context.emitFcmpl();
                uint8_t op = 0x99;
                switch (kind) {
                    case ExprKind::EQUAL: op = 0x99; break;
                    case ExprKind::NOT_EQUAL: op = 0x9a; break;
                    case ExprKind::LESS: op = 0x9b; break;
                    case ExprKind::GREATER_OR_EQUAL: op = 0x9c; break;
                    case ExprKind::GREATER: op = 0x9d; break;
                    case ExprKind::LESS_OR_EQUAL: op = 0x9e; break;
                    default: break;
                }
                context.emitJump(op, labelTrue);
            } else {
                uint8_t op = 0x9f;
                switch (kind) {
                    case ExprKind::EQUAL: op = 0x9f; break;
                    case ExprKind::NOT_EQUAL: op = 0xa0; break;
                    case ExprKind::LESS: op = 0xa1; break;
                    case ExprKind::GREATER_OR_EQUAL: op = 0xa2; break;
                    case ExprKind::GREATER: op = 0xa3; break;
                    case ExprKind::LESS_OR_EQUAL: op = 0xa4; break;
                    default: break;
                }
                context.emitJump(op, labelTrue);
            }
            context.emitIConst(0);
            context.emitJump(0xa7, labelEnd);
            context.markLabel(labelTrue);
            context.emitIConst(1);
            context.markLabel(labelEnd);
            break;
        }
        case ExprKind::AND:
        case ExprKind::OR: {
            if (!left || !right) break;
            auto* labelTrue = context.createLabel();
            auto* labelFalse = context.createLabel();
            auto* labelEnd = context.createLabel();
            if (kind == ExprKind::AND) {
                emitJumpIfFalse(context, left, labelFalse);
                emitJumpIfFalse(context, right, labelFalse);
                context.emitIConst(1);
                context.emitJump(0xa7, labelEnd);
                context.markLabel(labelFalse);
                context.emitIConst(0);
                context.markLabel(labelEnd);
            } else {
                left->emitBytecode(context);
                if (isObjectLike(left->getExprType())) {
                    context.emitJump(0xc7, labelTrue); // ifnonnull
                } else {
                    context.emitJump(0x9a, labelTrue); // ifne
                }
                right->emitBytecode(context);
                if (isObjectLike(right->getExprType())) {
                    context.emitJump(0xc7, labelTrue); // ifnonnull
                } else {
                    context.emitJump(0x9a, labelTrue); // ifne
                }
                context.emitIConst(0);
                context.emitJump(0xa7, labelEnd);
                context.markLabel(labelTrue);
                context.emitIConst(1);
                context.markLabel(labelEnd);
            }
            break;
        }
        case ExprKind::ASSIGN: {
            if (!left || !right) break;
            if (left->getKind() == ExprKind::IDENTIFIER && left->getIdentifier()) {
                std::string name = left->getIdentifier()->getIdentifier();
                const auto* local = context.getLocal(name);
                if (local) {
                    right->emitBytecode(context);
                    context.emitDup();
                    context.emitStore(local->type, local->index);
                    break;
                }
                std::string owner = left->getClassName().empty() ? context.getClassName() : mapRuntimeClassName(left->getClassName());
                FieldInfo* field = sem.lookupField(owner, name);
                if (field) {
                    std::string desc = field->type.getDescriptor();
                    if (field->isInstance) {
                        context.emitLoad(Type(TypeKind::CLASS_NAME, owner), 0);
                        right->emitBytecode(context);
                        context.emitDupX1();
                        context.emitPutField(owner, name, desc);
                    } else {
                        right->emitBytecode(context);
                        context.emitDup();
                        context.emitPutStatic(owner, name, desc);
                    }
                }
            }
            if (left->getKind() == ExprKind::DOT || left->getKind() == ExprKind::ARROW) {
                ExprNode* base = left->getLeft();
                ExprNode* member = left->getRight();
                if (base && member && member->getKind() == ExprKind::IDENTIFIER) {
                    std::string fieldName = member->getIdentifier()->getIdentifier();
                    const Type* baseType = base->getExprType();
                    std::string owner = baseType ? mapRuntimeClassName(baseType->className) : "";
                    FieldInfo* field = sem.lookupField(owner, fieldName);
                    if (field && field->isInstance) {
                        base->emitBytecode(context);
                        right->emitBytecode(context);
                        context.emitDupX1();
                        context.emitPutField(owner, fieldName, field->type.getDescriptor());
                    }
                }
                break;
            }
            if (left->getKind() == ExprKind::ARRAY_ACCESS) {
                ExprNode* arr = left->getOperand();
                ExprNode* idx = left->getIndex();
                if (arr && idx) {
                    arr->emitBytecode(context);
                    idx->emitBytecode(context);
                    right->emitBytecode(context);
                    const Type* arrType = arr->getExprType();
                    if (arrType && arrType->isArray()) {
                        context.emitDupX2();
                        TypeKind storeKind = arrType->arrayDimension > 1 ? TypeKind::CLASS_NAME : arrType->dataType;
                        context.emitArrayStore(storeKind);
                    }
                }
                break;
            }
            break;
        }
        case ExprKind::DOT:
        case ExprKind::ARROW: {
            if (!left || !right) break;
            if (right->getKind() != ExprKind::IDENTIFIER || !right->getIdentifier()) break;
            const Type* baseType = left->getExprType();
            std::string owner = baseType ? mapRuntimeClassName(baseType->className) : "";
            FieldInfo* field = sem.lookupField(owner, right->getIdentifier()->getIdentifier());
            if (field && field->isInstance) {
                left->emitBytecode(context);
                context.emitGetField(owner, field->name, field->type.getDescriptor());
            }
            break;
        }
        case ExprKind::ARRAY_ACCESS: {
            if (!operand || !index) break;
            operand->emitBytecode(context);
            index->emitBytecode(context);
            const Type* opType = operand->getExprType();
            if (opType && opType->dataType == TypeKind::CLASS_NAME &&
                (opType->className == "rtl/NSArray" || opType->className == "NSArray")) {
                context.emitInvokeVirtual("rtl/NSArray", "objectAtIndexDynamic", "(I)Lrtl/NSObject;");
                exprType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
            } else if (opType && opType->isArray()) {
                if (opType->arrayDimension > 1) {
                    context.emitArrayLoad(TypeKind::CLASS_NAME);
                    std::vector<int> subSizes;
                    if (!opType->arraySizes.empty() && opType->arraySizes.size() > 1) {
                        subSizes.assign(opType->arraySizes.begin() + 1, opType->arraySizes.end());
                    } else {
                        subSizes.assign(opType->arrayDimension - 1, 0);
                    }
                    exprType = new Type(opType->dataType, opType->className, subSizes);
                } else {
                    context.emitArrayLoad(opType->dataType);
                }
            }
            break;
        }
        case ExprKind::MESSAGE: {
            if (!receiver || !selector) break;

            SemanticContext& semCtx = SemanticContext::getInstance();
            Type* receiverType = nullptr;
            bool isSuperCall = false;
            bool isStaticCall = false;
            ClassInfo* receiverClass = nullptr;

            if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                receiverType = receiver->getExpr()->getExprType();
                if (receiver->getExpr()->getKind() == ExprKind::IDENTIFIER) {
                    std::string idName = receiver->getExpr()->getIdentifier()->getIdentifier();
                    ClassInfo* possibleClass = semCtx.lookupClass(idName);
                    if (possibleClass) {
                        receiverClass = possibleClass;
                        isStaticCall = true;
                        receiverType = new Type(TypeKind::CLASS_NAME, idName);
                    }
                }
            } else if (receiver->getKind() == ReceiverKind::CLASS_NAME) {
                std::string className = receiver->getClassName()->getIdentifier();
                receiverClass = semCtx.lookupClass(className);
                if (receiverClass) {
                    receiverType = new Type(TypeKind::CLASS_NAME, receiverClass->name);
                    isStaticCall = true;
                }
            } else if (receiver->getKind() == ReceiverKind::SUPER) {
                receiverClass = semCtx.lookupClass(context.getClassName());
                if (receiverClass && receiverClass->superclass) {
                    receiverClass = receiverClass->superclass;
                    receiverType = new Type(TypeKind::CLASS_NAME, receiverClass->name);
                    isSuperCall = true;
                    isStaticCall = context.isCurrentMethodStatic();
                }
            }

            if (!receiverType && receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                receiverType = receiver->getExpr()->getExprType();
            }

            if (!receiverClass && receiverType && receiverType->dataType == TypeKind::CLASS_NAME) {
                receiverClass = semCtx.lookupClass(receiverType->className);
            }

            std::vector<std::string> keywords;
            std::vector<const Type*> argTypes;
            std::vector<ExprNode*> argExprs;
            std::string methodName;

            if (selector->getKind() == MsgSelectorKind::SIMPLE_SEL) {
                std::string idName = selector->getIdentifier()->getIdentifier();
                methodName = idName;
            } else if (selector->getKind() == MsgSelectorKind::ARGUMENT_LIST) {
                MsgArgListNode* argList = selector->getMsgArgList();
                if (argList) {
                    auto args = argList->getMsgArgList();
                    if (args) {
                        for (MsgArgNode* argNode : *args) {
                            if (!argNode || !argNode->getIdentifier() || !argNode->getArg()) continue;
                            std::string keyword = argNode->getIdentifier()->getIdentifier();
                            keywords.push_back(keyword);
                            argTypes.push_back(argNode->getArg()->getExprType());
                            argExprs.push_back(argNode->getArg());
                        }
                    }
                }
                for (size_t i = 0; i < keywords.size(); i++) {
                    methodName += keywords[i];
                    if (i < keywords.size() - 1) {
                        methodName += ":";
                    }
                }
            }

            MethodInfo* method = nullptr;
            if (receiverClass) {
                method = receiverClass->lookupMethod(methodName, argTypes, keywords, true, isStaticCall);
            }
            if (!method && receiverClass) {
                auto it = receiverClass->methods.find(methodName);
                if (it != receiverClass->methods.end()) {
                    for (const auto& candidate : it->second) {
                        if (!candidate) continue;
                        if (candidate->isClassMethod != isStaticCall) continue;
                        if (candidate->parameterTypes.size() != argTypes.size()) continue;
                        method = candidate.get();
                        break;
                    }
                }
            }

            std::string owner = receiverClass ? mapRuntimeClassName(receiverClass->name) : context.getClassName();
            if (method) {
                owner = mapRuntimeClassName(method->declaringClass->name);
            }

            std::vector<const Type*> finalArgTypes;
            Type returnType(TypeKind::TYPE_ID);
            if (method) {
                finalArgTypes = method->parameterTypes;
                bool needsFallback = finalArgTypes.empty();
                for (const auto* t : finalArgTypes) {
                    if (!t) {
                        needsFallback = true;
                        break;
                    }
                }
                if (needsFallback) {
                    finalArgTypes.clear();
                    for (size_t i = 0; i < method->getParameterCount(); ++i) {
                        const LocalVarInfo* param = method->getParameter(i);
                        if (param) finalArgTypes.push_back(&param->type);
                    }
                    if (finalArgTypes.empty() && !argTypes.empty()) {
                        finalArgTypes = argTypes;
                    }
                }
                returnType = method->getReturnType();
                exprType = new Type(returnType);
            } else {
                finalArgTypes = argTypes;
                if (getExprType()) {
                    returnType = *getExprType();
                }
            }

            if (isStaticCall && methodName == "alloc" && argExprs.empty()) {
                std::string allocOwner = receiverClass ? mapRuntimeClassName(receiverClass->name) : owner;
                context.emitNewObject(allocOwner);
                context.emitDup();
                context.emitInvokeSpecial(allocOwner, "<init>", "()V");
                exprType = new Type(TypeKind::CLASS_NAME, allocOwner);
                break;
            }

            if (isStaticCall && methodName == "new" && argExprs.empty()) {
                std::string allocOwner = receiverClass ? mapRuntimeClassName(receiverClass->name) : owner;
                context.emitNewObject(allocOwner);
                context.emitDup();
                context.emitInvokeSpecial(allocOwner, "<init>", "()V");
                exprType = new Type(TypeKind::CLASS_NAME, allocOwner);
                break;
            }

            if (!isStaticCall && methodName == "init" && argExprs.empty()) {
                bool skipInit = false;
                if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                    ExprNode* recvExpr = receiver->getExpr();
                    if (recvExpr->getKind() == ExprKind::MESSAGE) {
                        MsgSelectorNode* recvSel = recvExpr->getSelector();
                        if (recvSel && recvSel->getKind() == MsgSelectorKind::SIMPLE_SEL &&
                            recvSel->getIdentifier()) {
                            std::string recvName = recvSel->getIdentifier()->getIdentifier();
                            if (recvName == "alloc" || recvName == "new") {
                                skipInit = true;
                            }
                        }
                    }
                }
                if (skipInit) {
                    if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                        receiver->getExpr()->emitBytecode(context);
                    }
                    if (receiverClass) {
                        exprType = new Type(TypeKind::CLASS_NAME, mapRuntimeClassName(receiverClass->name));
                    } else {
                        exprType = new Type(TypeKind::CLASS_NAME, owner);
                    }
                    break;
                }
            }

            if (!isStaticCall) {
                if (receiver->getKind() == ReceiverKind::SUPER) {
                    context.emitLoad(Type(TypeKind::CLASS_NAME, context.getClassName()), 0);
                } else if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                    receiver->getExpr()->emitBytecode(context);
                }
                if (method && !owner.empty() && !isSuperCall) {
                    // Cast to the method owner to satisfy JVM verifier
                    // (e.g., NSArray returns NSObject, but we call BaseClass methods).
                    context.emitCheckCast(owner);
                }
            }

            for (auto* argExpr : argExprs) {
                if (argExpr) {
                    argExpr->emitBytecode(context);
                }
            }

            std::vector<Type> mappedArgs;
            std::vector<const Type*> mappedArgPtrs;
            mappedArgs.reserve(finalArgTypes.size());
            mappedArgPtrs.reserve(finalArgTypes.size());
            for (const auto* t : finalArgTypes) {
                if (t) {
                    mappedArgs.push_back(mapRuntimeType(*t));
                    mappedArgPtrs.push_back(&mappedArgs.back());
                }
            }
            Type mappedReturn = mapRuntimeType(returnType);
            std::string descriptor = buildMethodDescriptor(mappedArgPtrs, mappedReturn);
            std::string jvmMethodName = mangleJvmMethodName(methodName);
            if (owner.rfind("rtl/", 0) == 0 && method) {
                jvmMethodName = mapRtlMethodName(owner, jvmMethodName, isStaticCall);
            }
            if (isStaticCall) {
                context.emitInvokeStatic(owner, jvmMethodName, descriptor);
            } else if (isSuperCall) {
                context.emitInvokeSpecial(owner, jvmMethodName, descriptor);
            } else {
                context.emitInvokeVirtual(owner, jvmMethodName, descriptor);
            }
            break;
        }
        case ExprKind::FUNCTION_CALL: {
            if (!funcId) break;
            std::string funcName = funcId->getIdentifier();
            if (args) {
                args->emitBytecode(context);
            }
            FunctionInfo* funcInfo = sem.lookupFunction(funcName);
            std::string desc = buildFunctionDescriptor(funcInfo);
            context.emitInvokeStatic(context.getClassName(), funcName, desc);
            break;
        }
        default:
            break;
    }
}

//--------------------------------------------------------------TypeNode--------------------------------------------------------------

void TypeNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------DeclaratorListNode--------------------------------------------------------------

void DeclaratorListNode::emitBytecode(BytecodeContext& context) {
    if (!initDeclList) return;
    for (auto* initDecl : *initDeclList) {
        if (initDecl) {
            initDecl->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------DeclNode--------------------------------------------------------------

void DeclNode::emitBytecode(BytecodeContext& context) {
    if (!type || !declaratorList) return;
    Type baseType = convertTypeNodeToType(type);
    if (auto* list = declaratorList->getInitDeclList()) {
        for (auto* initDecl : *list) {
            if (!initDecl) continue;
            DeclaratorNode* decl = initDecl->getDeclarator();
            if (!decl || !decl->getIdentifier()) continue;
            std::string name = decl->getIdentifier()->getIdentifier();
            Type varType = baseType;
            std::vector<int> sizes;
            if (auto* arraySizes = decl->getArraySizes()) {
                sizes.assign(arraySizes->size(), 0);
            }
            if (!sizes.empty()) {
                varType = convertTypeNodeToType(type, sizes);
            }
            int localIndex = context.defineLocal(name, varType);
            InitializerNode* init = initDecl->getInitializer();
            if (varType.isArray() && (!init || init->getKind() != InitializerKind::EXPR || !init->getExpr())) {
                std::vector<ExprNode*> dimExprs;
                if (auto* arraySizes = decl->getArraySizes()) {
                    for (auto* sizeExpr : *arraySizes) {
                        if (sizeExpr) dimExprs.push_back(sizeExpr);
                    }
                }
                if (dimExprs.size() > 1) {
                    for (auto* sizeExpr : dimExprs) {
                        if (sizeExpr) {
                            sizeExpr->emitBytecode(context);
                        } else {
                            context.emitIConst(0);
                        }
                    }
                    context.emitMultiANewArray(varType.getDescriptor(), static_cast<uint8_t>(dimExprs.size()));
                } else {
                    ExprNode* sizeExpr = nullptr;
                    if (!dimExprs.empty()) sizeExpr = dimExprs.front();
                    if (sizeExpr) {
                        sizeExpr->emitBytecode(context);
                    } else {
                        context.emitIConst(0);
                    }
                    if (varType.dataType == TypeKind::CLASS_NAME) {
                        std::string elemClass = mapRuntimeClassName(varType.className);
                        context.emitANewArray(elemClass);
                    } else if (varType.dataType == TypeKind::TYPE_ID) {
                        context.emitANewArray("java/lang/Object");
                    } else {
                        context.emitNewArray(varType.dataType);
                    }
                }
                context.emitStore(varType, localIndex);
            } else if (init && init->getKind() == InitializerKind::EXPR && init->getExpr()) {
                init->getExpr()->emitBytecode(context);
                context.emitStore(varType, localIndex);
            } else {
                emitDefaultValue(context, varType);
                context.emitStore(varType, localIndex);
            }
        }
    }
}

//--------------------------------------------------------------StmtListNode--------------------------------------------------------------

void StmtListNode::emitBytecode(BytecodeContext& context) {
    if (!stmts) return;
    for (auto* stmt : *stmts) {
        if (stmt) {
            stmt->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

void StmtNode::emitBytecode(BytecodeContext& context) {
    switch (kind) {
        case StmtKind::EMPTY:
            break;
        case StmtKind::EXPR:
            if (expr) {
                if (!emitPostIncDecAsStatement(context, expr)) {
                    expr->emitBytecode(context);
                    if (expr->getExprType() && expr->getExprType()->dataType != TypeKind::VOID) {
                        context.emitPop();
                    }
                }
            }
            break;
        case StmtKind::RETURN:
            if (expr) {
                expr->emitBytecode(context);
                if (expr->getExprType()) {
                    context.emitReturn(*expr->getExprType());
                } else {
                    context.emitReturn(Type(TypeKind::VOID));
                }
            } else {
                context.emitReturn(Type(TypeKind::VOID));
            }
            break;
        case StmtKind::IF: {
            if (!condition || !thenBranch) break;
            auto* labelEnd = context.createLabel();
            emitConditionJumpFalse(context, condition, labelEnd);
            thenBranch->emitBytecode(context);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::IF_ELSE: {
            if (!condition || !thenBranch || !elseBranch) break;
            auto* labelElse = context.createLabel();
            auto* labelEnd = context.createLabel();
            emitConditionJumpFalse(context, condition, labelElse);
            thenBranch->emitBytecode(context);
            context.emitJump(0xa7, labelEnd); // goto end
            context.markLabel(labelElse);
            elseBranch->emitBytecode(context);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::WHILE: {
            if (!condition || !body) break;
            auto* labelStart = context.createLabel();
            auto* labelEnd = context.createLabel();
            context.markLabel(labelStart);
            emitConditionJumpFalse(context, condition, labelEnd);
            body->emitBytecode(context);
            context.emitJump(0xa7, labelStart);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::DO_WHILE: {
            if (!condition || !body) break;
            auto* labelStart = context.createLabel();
            context.markLabel(labelStart);
            body->emitBytecode(context);
            condition->emitBytecode(context);
            context.emitJump(0x9a, labelStart); // ifne
            break;
        }
        case StmtKind::FOR_WITH_EXPR: {
            auto* labelStart = context.createLabel();
            auto* labelEnd = context.createLabel();
            if (expr) {
                if (!emitPostIncDecAsStatement(context, expr)) {
                    expr->emitBytecode(context);
                    if (expr->getExprType() && expr->getExprType()->dataType != TypeKind::VOID) {
                        context.emitPop();
                    }
                }
            }
            context.markLabel(labelStart);
            if (condition) {
                emitConditionJumpFalse(context, condition, labelEnd);
            }
            if (body) {
                body->emitBytecode(context);
            }
            if (post) {
                if (!emitPostIncDecAsStatement(context, post)) {
                    post->emitBytecode(context);
                    if (post->getExprType() && post->getExprType()->dataType != TypeKind::VOID) {
                        context.emitPop();
                    }
                }
            }
            context.emitJump(0xa7, labelStart);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::FOR_IN:
        case StmtKind::TYPED_FOR_IN: {
            if (!collection || !body || !forInId) break;
            Type collType(TypeKind::CLASS_NAME, "rtl/NSArray");
            if (collection->getExprType() && collection->getExprType()->dataType == TypeKind::CLASS_NAME) {
                collType = Type(TypeKind::CLASS_NAME, mapRuntimeClassName(collection->getExprType()->className));
            }
            std::string collTmp = "__forin_coll_" + std::to_string(getId());
            int collIndex = context.defineLocal(collTmp, collType);
            collection->emitBytecode(context);
            context.emitStore(collType, collIndex);

            std::string countTmp = "__forin_count_" + std::to_string(getId());
            int countIndex = context.defineLocal(countTmp, Type(TypeKind::INT));
            context.emitLoad(collType, collIndex);
            context.emitInvokeVirtual("rtl/NSArray", "countDynamic", "()I");
            context.emitStore(Type(TypeKind::INT), countIndex);

            std::string idxTmp = "__forin_idx_" + std::to_string(getId());
            int idxIndex = context.defineLocal(idxTmp, Type(TypeKind::INT));
            context.emitIConst(0);
            context.emitStore(Type(TypeKind::INT), idxIndex);

            Type iterType(TypeKind::TYPE_ID);
            if (kind == StmtKind::TYPED_FOR_IN && forInType) {
                iterType = convertTypeNodeToType(forInType);
                iterType = mapRuntimeType(iterType);
            }
            std::string iterName = forInId->getIdentifier();
            int iterIndex = -1;
            if (const auto* local = context.getLocal(iterName)) {
                iterIndex = local->index;
            } else {
                iterIndex = context.defineLocal(iterName, iterType);
            }

            auto* labelStart = context.createLabel();
            auto* labelEnd = context.createLabel();
            context.markLabel(labelStart);
            context.emitLoad(Type(TypeKind::INT), idxIndex);
            context.emitLoad(Type(TypeKind::INT), countIndex);
            context.emitJump(0xa2, labelEnd); // if_icmpge

            context.emitLoad(collType, collIndex);
            context.emitLoad(Type(TypeKind::INT), idxIndex);
            context.emitInvokeVirtual("rtl/NSArray", "objectAtIndexDynamic", "(I)Lrtl/NSObject;");
            context.emitStore(iterType, iterIndex);

            body->emitBytecode(context);
            context.emitIInc(idxIndex, 1);
            context.emitJump(0xa7, labelStart);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::FOR_WITH_DECL: {
            auto* labelStart = context.createLabel();
            auto* labelEnd = context.createLabel();
            if (decl) {
                decl->emitBytecode(context);
            }
            context.markLabel(labelStart);
            if (condition) {
                emitConditionJumpFalse(context, condition, labelEnd);
            }
            if (body) {
                body->emitBytecode(context);
            }
            if (post) {
                if (!emitPostIncDecAsStatement(context, post)) {
                    post->emitBytecode(context);
                    if (post->getExprType() && post->getExprType()->dataType != TypeKind::VOID) {
                        context.emitPop();
                    }
                }
            }
            context.emitJump(0xa7, labelStart);
            context.markLabel(labelEnd);
            break;
        }
        case StmtKind::COMPOUND:
            if (compound) {
                compound->emitBytecode(context);
            }
            break;
        case StmtKind::DECLARATION:
            if (decl) {
                decl->emitBytecode(context);
            }
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

void ArraySizeSpecNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

void ParamDeclNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

void ParamListNode::emitBytecode(BytecodeContext& context) {
    if (!paramList) return;
    for (auto* param : *paramList) {
        if (param) {
            param->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

void FuncDefNode::emitBytecode(BytecodeContext& context) {
    if (!type || !identifier) return;
    std::string name = identifier->getIdentifier();
    std::string descriptor = buildFunctionDescriptor(type, paramList);
    uint16_t accessFlags = 0x0001 | 0x0008; // public static
    context.beginMethod(name, descriptor, accessFlags);
    context.setCurrentMethodInfo(nullptr, true);

    if (paramList && paramList->getParamList()) {
        for (auto* param : *paramList->getParamList()) {
            if (!param || !param->getIdentifier()) continue;
            std::string paramName = param->getIdentifier()->getIdentifier();
            std::vector<int> sizes = param->getArraySizes();
            Type paramType = convertTypeNodeToType(param->getType(), sizes);
            context.defineLocal(paramName, paramType);
        }
    }

    if (compoundStmt) {
        compoundStmt->emitBytecode(context);
    }

    Type retType = convertTypeNodeToType(type);
    if (retType.dataType == TypeKind::VOID) {
        context.emitReturn(retType);
    } else {
        emitDefaultValue(context, retType);
        context.emitReturn(retType);
    }
    context.endMethod();
}

//--------------------------------------------------------------FuncDeclNode--------------------------------------------------------------

void FuncDeclNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MethodParamNode--------------------------------------------------------------

void MethodParamNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MethodSelNode--------------------------------------------------------------

void MethodSelNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------MethodDefNode--------------------------------------------------------------

void MethodDefNode::emitBytecode(BytecodeContext& context) {
    if (!type) return;
    std::string methodName;
    std::vector<MethodParamNode*> params;

    if (identifier) {
        methodName = identifier->getIdentifier();
    } else if (methodSel && methodSel->getMethodParamList()) {
        auto* list = methodSel->getMethodParamList();
        for (auto* param : *list) {
            if (param) {
                params.push_back(param);
            }
        }
        for (size_t i = 0; i < params.size(); i++) {
            std::string keyword = params[i]->getSelectorIdentifier()
                ? params[i]->getSelectorIdentifier()->getIdentifier()
                : "";
            methodName += keyword;
            if (i < params.size() - 1) {
                methodName += ":";
            }
        }
    }

    Type returnType = convertTypeNodeToType(type);
    std::string descriptor = "(";
    for (auto* param : params) {
        if (!param || !param->getType()) continue;
        Type paramType = convertTypeNodeToType(param->getType(), param->getArraySizes());
        descriptor += paramType.getDescriptor();
    }
    descriptor += ")";
    descriptor += returnType.getDescriptor();

    AccessModifier access = getAccessModifier();
    if (access == AccessModifier::NONE) {
        access = AccessModifier::PUBLIC;
    }
    uint16_t accessFlags = mapAccessToFlags(access);
    if (isClassMethod()) {
        accessFlags |= 0x0008;
    }

    context.beginMethod(mangleJvmMethodName(methodName), descriptor, accessFlags);
    context.setCurrentMethodInfo(nullptr, isClassMethod());

    for (auto* param : params) {
        if (!param || !param->getParamIdentifier() || !param->getType()) continue;
        std::string paramName = param->getParamIdentifier()->getIdentifier();
        Type paramType = convertTypeNodeToType(param->getType(), param->getArraySizes());
        context.defineLocal(paramName, paramType);
    }

    if (compoundStmt) {
        compoundStmt->emitBytecode(context);
    }

    if (returnType.dataType == TypeKind::VOID) {
        context.emitReturn(returnType);
    } else {
        emitDefaultValue(context, returnType);
        context.emitReturn(returnType);
    }
    context.endMethod();
}

//--------------------------------------------------------------ImplementationDefListNode--------------------------------------------------------------

void ImplementationDefListNode::emitBytecode(BytecodeContext& context) {
    if (classMethodDefs) {
        for (auto* methodDef : *classMethodDefs) {
            if (methodDef) methodDef->emitBytecode(context);
        }
    }
    if (instanceMethodDefs) {
        for (auto* methodDef : *instanceMethodDefs) {
            if (methodDef) methodDef->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------MethodDeclNode--------------------------------------------------------------

void MethodDeclNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------PropertyNode--------------------------------------------------------------

void PropertyNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InterfaceDeclListNode--------------------------------------------------------------

void InterfaceDeclListNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InitializerListNode--------------------------------------------------------------

void InitializerListNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InitializerNode--------------------------------------------------------------

void InitializerNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------DeclaratorNode--------------------------------------------------------------

void DeclaratorNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InitDeclNode--------------------------------------------------------------

void InitDeclNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------AccessModifierNode--------------------------------------------------------------

void AccessModifierNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InstanceVarDeclNode--------------------------------------------------------------

void InstanceVarDeclNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InstanceVarsDeclListNode--------------------------------------------------------------

void InstanceVarsDeclListNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------InstanceVarsNode--------------------------------------------------------------

void InstanceVarsNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ImplementationNode--------------------------------------------------------------

void ImplementationNode::emitBytecode(BytecodeContext& context) {
    if (!className) return;
    std::string classNameStr = className->getIdentifier();
    std::string superName;
    if (superClassName) {
        superName = superClassName->getIdentifier();
    } else {
        SemanticContext& semCtx = SemanticContext::getInstance();
        ClassInfo* cls = semCtx.lookupClass(classNameStr);
        if (cls && cls->superclass) {
            superName = cls->superclass->name;
        } else {
            superName = "rtl/NSObject";
        }
    }
    context.pushClassState();
    context.beginClass(classNameStr, context.makeClassOutputPath(classNameStr));
    context.setSuperClassName(mapRuntimeClassName(superName));

    SemanticContext& semCtx = SemanticContext::getInstance();
    ClassInfo* cls = semCtx.lookupClass(classNameStr);
    if (cls) {
        for (const auto& [fieldName, field] : cls->fields) {
            if (!field) continue;
            Type fieldType = mapRuntimeType(field->type);
            uint16_t flags = mapAccessToFlags(field->accessModifier);
            if (!field->isInstance) {
                flags |= 0x0008;
            }
            context.addField(fieldName, fieldType.getDescriptor(), flags);
        }
    }

    std::unordered_set<std::string> implementedInstanceMethods;
    std::unordered_set<std::string> implementedClassMethods;
    if (implDefList && implDefList->getInstanceMethodDefs()) {
        for (auto* methodDef : *implDefList->getInstanceMethodDefs()) {
            implementedInstanceMethods.insert(buildMethodNameFromDef(methodDef));
        }
    }
    if (implDefList && implDefList->getClassMethodDefs()) {
        for (auto* methodDef : *implDefList->getClassMethodDefs()) {
            implementedClassMethods.insert(buildMethodNameFromDef(methodDef));
        }
    }

    if (cls) {
        std::unordered_map<std::string, std::pair<Type, bool>> propertyMeta;
        std::unordered_map<std::string, std::pair<Type, bool>> classPropertyMeta;

        if (cls->interface && cls->interface->getInterfaceDeclList()) {
            auto* props = cls->interface->getInterfaceDeclList()->getProperties();
            if (props) {
                for (auto* prop : *props) {
                    if (!prop || !prop->getName() || !prop->getType()) continue;
                    std::string name = prop->getName()->getIdentifier();
                    Type t = mapRuntimeType(convertTypeNodeToType(prop->getType()));
                    bool isClassProperty = prop->getAttribute() == Attribute::CLASS;
                    bool readonly = !isClassProperty && prop->getAttribute() == Attribute::READONLY;
                    if (isClassProperty) {
                        classPropertyMeta.insert_or_assign(name, std::make_pair(t, readonly));
                    } else {
                        propertyMeta.insert_or_assign(name, std::make_pair(t, readonly));
                    }
                }
            }
        }

        if (implDefList && implDefList->getproperties()) {
            for (auto* prop : *implDefList->getproperties()) {
                if (!prop || !prop->getName() || !prop->getType()) continue;
                std::string name = prop->getName()->getIdentifier();
                Type t = mapRuntimeType(convertTypeNodeToType(prop->getType()));
                bool isClassProperty = prop->getAttribute() == Attribute::CLASS;
                bool readonly = !isClassProperty && prop->getAttribute() == Attribute::READONLY;
                if (isClassProperty) {
                    classPropertyMeta.insert_or_assign(name, std::make_pair(t, readonly));
                } else {
                    propertyMeta.insert_or_assign(name, std::make_pair(t, readonly));
                }
            }
        }

        if (!cls->propertyIvarMapping.empty()) {
            for (const auto& [propName, ivarName] : cls->propertyIvarMapping) {
                auto metaIt = propertyMeta.find(propName);
                if (metaIt == propertyMeta.end()) continue;
                const Type propType = metaIt->second.first;
                const bool isReadonly = metaIt->second.second;

                std::string getterName = semCtx.generateGetterName(propName);
                if (implementedInstanceMethods.find(getterName) == implementedInstanceMethods.end()) {
                    std::string desc = "()" + propType.getDescriptor();
                    context.beginMethod(mangleJvmMethodName(getterName), desc, 0x0001);
                    context.setCurrentMethodInfo(nullptr, false);
                    context.emitLoad(Type(TypeKind::CLASS_NAME, classNameStr), 0);
                    context.emitGetField(classNameStr, ivarName, propType.getDescriptor());
                    context.emitReturn(propType);
                    context.endMethod();
                }

                if (!isReadonly) {
                    std::string setterName = semCtx.generateSetterName(propName);
                    if (implementedInstanceMethods.find(setterName) == implementedInstanceMethods.end()) {
                        std::string desc = "(" + propType.getDescriptor() + ")V";
                        context.beginMethod(mangleJvmMethodName(setterName), desc, 0x0001);
                        context.setCurrentMethodInfo(nullptr, false);
                        context.defineLocal("value", propType);
                        context.emitLoad(Type(TypeKind::CLASS_NAME, classNameStr), 0);
                        context.emitLoad(propType, 1);
                        context.emitPutField(classNameStr, ivarName, propType.getDescriptor());
                        context.emitReturn(Type(TypeKind::VOID));
                        context.endMethod();
                    }
                }
            }
        }

        for (const auto& [propName, meta] : classPropertyMeta) {
            const Type propType = meta.first;
            const bool isReadonly = meta.second;
            std::string fieldName = "__class_" + propName;

            std::string getterName = semCtx.generateGetterName(propName);
            if (implementedClassMethods.find(getterName) == implementedClassMethods.end()) {
                std::string desc = "()" + propType.getDescriptor();
                context.beginMethod(mangleJvmMethodName(getterName), desc, 0x0001 | 0x0008);
                context.setCurrentMethodInfo(nullptr, true);
                context.emitGetStatic(classNameStr, fieldName, propType.getDescriptor());
                context.emitReturn(propType);
                context.endMethod();
            }

            if (!isReadonly) {
                std::string setterName = semCtx.generateSetterName(propName);
                if (implementedClassMethods.find(setterName) == implementedClassMethods.end()) {
                    std::string desc = "(" + propType.getDescriptor() + ")V";
                    context.beginMethod(mangleJvmMethodName(setterName), desc, 0x0001 | 0x0008);
                    context.setCurrentMethodInfo(nullptr, true);
                    context.defineLocal("value", propType);
                    context.emitLoad(propType, 0);
                    context.emitPutStatic(classNameStr, fieldName, propType.getDescriptor());
                    context.emitReturn(Type(TypeKind::VOID));
                    context.endMethod();
                }
            }
        }
    }

    bool hasFieldInit = false;
    if (cls) {
        for (const auto& [fieldName, field] : cls->fields) {
            if (field && field->initialValue && field->isInstance) {
                hasFieldInit = true;
                break;
            }
        }
    }

    if (hasFieldInit) {
        context.beginMethod("<init>", "()V", 0x0001);
        context.setCurrentMethodInfo(nullptr, false);
        const std::string superOwner = mapRuntimeClassName(superName);
        context.emitLoad(Type(TypeKind::CLASS_NAME, classNameStr), 0);
        context.emitInvokeSpecial(superOwner, "<init>", "()V");
        for (const auto& [fieldName, field] : cls->fields) {
            if (!field || !field->initialValue || !field->isInstance) continue;
            if (field->declaringClass && field->declaringClass->name != classNameStr) continue;
            Type fieldType = field->type;
            if (fieldType.dataType == TypeKind::CLASS_NAME) {
                fieldType.className = mapRuntimeClassName(fieldType.className);
            }
            context.emitLoad(Type(TypeKind::CLASS_NAME, classNameStr), 0);
            field->initialValue->emitBytecode(context);
            context.emitPutField(classNameStr, fieldName, fieldType.getDescriptor());
        }
        context.emitReturn(Type(TypeKind::VOID));
        context.endMethod();
    }

    if (implDefList) {
        implDefList->emitBytecode(context);
    }
    context.endClass();
    context.popClassState();
}

//--------------------------------------------------------------InterfaceNode--------------------------------------------------------------

void InterfaceNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ClassNameListNode--------------------------------------------------------------

void ClassNameListNode::emitBytecode(BytecodeContext& context) {}

//--------------------------------------------------------------ExternalDeclNode--------------------------------------------------------------

void ExternalDeclNode::emitBytecode(BytecodeContext& context) {
    switch (kind) {
        case ExternalDeclKind::IMPLEMENTATION:
            if (implementation) implementation->emitBytecode(context);
            break;
        case ExternalDeclKind::FUNC_DEF:
            if (funcDef) funcDef->emitBytecode(context);
            break;
        default:
            break;
    }
}

//--------------------------------------------------------------ExternalDeclListNode--------------------------------------------------------------

void ExternalDeclListNode::emitBytecode(BytecodeContext& context) {
    if (!externalDeclList) return;
    for (auto* decl : *externalDeclList) {
        if (decl) {
            decl->emitBytecode(context);
        }
    }
}

//--------------------------------------------------------------ProgramNode--------------------------------------------------------------

void ProgramNode::emitBytecode(BytecodeContext& context) {
    if (externalDeclList) {
        externalDeclList->emitBytecode(context);
    }
    SemanticContext& sem = SemanticContext::getInstance();
    FunctionInfo* mainFunc = sem.lookupFunction("main");
    if (mainFunc) {
        std::string mainDesc = buildFunctionDescriptor(mainFunc);
        if (mainDesc == "()I" || mainDesc == "()V") {
            context.beginMethod("main", "([Ljava/lang/String;)V", 0x0001 | 0x0008);
            Type argsType(TypeKind::CLASS_NAME, "java/lang/String", std::vector<int>{0});
            context.defineLocal("args", argsType);
            context.emitInvokeStatic(context.getClassName(), "main", mainDesc);
            if (mainDesc != "()V") {
                context.emitPop();
            }
            context.emitReturn(Type(TypeKind::VOID));
            context.endMethod();
        }
    }
    context.endClass();
}
