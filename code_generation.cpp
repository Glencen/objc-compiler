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
        condition->getOperand()->emitBytecode(context);
        context.emitJump(0x9a, falseLabel); // ifne
        return;
    }
    if (kind == ExprKind::AND && condition->getLeft() && condition->getRight()) {
        condition->getLeft()->emitBytecode(context);
        context.emitJump(0x99, falseLabel); // left == 0
        condition->getRight()->emitBytecode(context);
        context.emitJump(0x99, falseLabel); // right == 0
        return;
    }
    if (kind == ExprKind::OR && condition->getLeft() && condition->getRight()) {
        auto* labelTrue = context.createLabel();
        condition->getLeft()->emitBytecode(context);
        context.emitJump(0x9a, labelTrue); // left != 0
        condition->getRight()->emitBytecode(context);
        context.emitJump(0x99, falseLabel); // right == 0
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
            bool isFloat = isFloatType(left->getExprType()) || isFloatType(right->getExprType());
            if (isFloat) {
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
    context.emitJump(0x99, falseLabel); // ifeq
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
        return Type(typeKind);
    }
    if (typeKind == TypeKind::CLASS_NAME) {
        if (!arraySizes.empty()) {
            return Type(TypeKind::CLASS_NAME, className, arraySizes);
        }
        return Type(TypeKind::CLASS_NAME, className);
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
            bool isFloat = isFloatType(left->getExprType()) || isFloatType(right->getExprType());
            auto* labelTrue = context.createLabel();
            auto* labelEnd = context.createLabel();
            if (isFloat) {
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
                left->emitBytecode(context);
                context.emitJump(0x99, labelFalse);
                right->emitBytecode(context);
                context.emitJump(0x99, labelFalse);
                context.emitIConst(1);
                context.emitJump(0xa7, labelEnd);
                context.markLabel(labelFalse);
                context.emitIConst(0);
                context.markLabel(labelEnd);
            } else {
                left->emitBytecode(context);
                context.emitJump(0x9a, labelTrue);
                right->emitBytecode(context);
                context.emitJump(0x9a, labelTrue);
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
                returnType = method->getReturnType();
                exprType = new Type(returnType);
            } else {
                finalArgTypes = argTypes;
                if (getExprType()) {
                    returnType = *getExprType();
                }
            }

            if (isStaticCall && methodName == "new" && argExprs.empty()) {
                std::string allocOwner = receiverClass ? mapRuntimeClassName(receiverClass->name) : owner;
                context.emitNewObject(allocOwner);
                context.emitDup();
                context.emitInvokeSpecial(allocOwner, "<init>", "()V");
                exprType = new Type(TypeKind::CLASS_NAME, allocOwner);
                break;
            }

            if (!isStaticCall) {
                if (receiver->getKind() == ReceiverKind::SUPER) {
                    context.emitLoad(Type(TypeKind::CLASS_NAME, context.getClassName()), 0);
                } else if (receiver->getKind() == ReceiverKind::EXPR && receiver->getExpr()) {
                    receiver->getExpr()->emitBytecode(context);
                }
            }

            for (auto* argExpr : argExprs) {
                if (argExpr) {
                    argExpr->emitBytecode(context);
                }
            }

            std::string descriptor = buildMethodDescriptor(finalArgTypes, returnType);
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
            int localIndex = context.defineLocal(name, varType);
            InitializerNode* init = initDecl->getInitializer();
            if (init && init->getKind() == InitializerKind::EXPR && init->getExpr()) {
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

    uint16_t accessFlags = 0x0001;
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
    std::string superName = superClassName ? superClassName->getIdentifier() : "rtl/NSObject";
    context.pushClassState();
    context.beginClass(classNameStr, context.makeClassOutputPath(classNameStr));
    context.setSuperClassName(mapRuntimeClassName(superName));

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
