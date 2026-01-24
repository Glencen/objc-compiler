#include "classes.h"

//--------------------------------------------------------------AstNode--------------------------------------------------------------

unsigned int AstNode::maxId = 0;

void AstNode::appendDotNode(string &res) const {
    res += "node" + to_string(id) + " [label=\"" + getDotLabel() + "\"];\n";
}

void AstNode::appendDotEdge(string &res, const AstNode *child, const string &edgeLabel) const {
    if (!child) return;

    res += "node" + to_string(id) + " -> node" + to_string(child->getId());

    if (!edgeLabel.empty()) {
        res += " [label=\"" + edgeLabel + "\"]";
    }

    res += ";\n" + child->toDot();
}

//--------------------------------------------------------------ValueNode--------------------------------------------------------------

ValueNode::ValueNode() : AstNode() {
    valueType = ValueKind::NONE;
    intValue = 0;
    floatValue = 0;
    boolValue = false;
    charValue = 0;
    stringValue = "";
}

ValueNode* ValueNode::createInt(int value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::INT_LIT;
    node->intValue = value;
    return node;
}

ValueNode* ValueNode::createFloat(float value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::FLOAT_LIT;
    node->floatValue = value;
    return node;
}

ValueNode* ValueNode::createBool(bool value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::BOOL_LIT;
    node->boolValue = value;
    return node;
}

ValueNode* ValueNode::createChar(char value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::CHAR_LIT;
    node->charValue = value;
    return node;
}

ValueNode* ValueNode::createString(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::STRING_LIT;
    node->stringValue = *value;
    return node;
}

ValueNode* ValueNode::createNil() {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::NIL;
    return node;
}

ValueNode* ValueNode::createObjcInt(int value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::OBJC_INT_LIT;
    node->intValue = value;
    return node;
}

ValueNode* ValueNode::createObjcFloat(float value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::OBJC_FLOAT_LIT;
    node->floatValue = value;
    return node;
}

ValueNode* ValueNode::createObjcBool(bool value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::OBJC_BOOL_LIT;
    node->boolValue = value;
    return node;
}

ValueNode* ValueNode::createObjcString(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::OBJC_STRING_LIT;
    node->stringValue = *value;
    return node;
}

ValueNode* ValueNode::createIdentifier(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::IDENTIFIER;
    node->stringValue = *value;
    return node;
}

ValueNode* ValueNode::createClassName(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = ValueKind::CLASS_NAME;
    node->stringValue = *value;
    return node;
}

ValueKind ValueNode::getValueKind() const {
    return valueType;
}

int ValueNode::getInt() const {
    return intValue;
}

float ValueNode::getFloat() const {
    return floatValue;
}

bool ValueNode::getBool() const {
    return boolValue;
}

char ValueNode::getChar() const {
    return charValue;
}

string ValueNode::getString() const {
    return stringValue;
}

string ValueNode::getObjcInt() const {
    return stringValue;
}

string ValueNode::getObjcFloat() const {
    return stringValue;
}

string ValueNode::getObjcBool() const {
    return stringValue;
}

string ValueNode::getObjcString() const {
    return stringValue;
}

string ValueNode::getIdentifier() const {
    return stringValue;
}

string ValueNode::getClassName() const {
    return stringValue;
}

void ValueNode::setClassName(string className) {
    stringValue = className;
}

void ValueNode::setLocalVarId(int id) {
    localVarId = id;
}

int ValueNode::getLocalVarId() const {
    return localVarId;
}

void ValueNode::setIsLocalVar(bool val) {
    isLocalVar = val;
}

bool ValueNode::getIsLocalVar() const {
    return isLocalVar;
}

string ValueNode::getDotLabel() const {
    auto escapeString = [](const string& src) {
        string out;
        out.reserve(src.size());
        for (char c : src) {
            switch (c) {
                case '\\': out += "\\\\"; break;
                case '\"': out += "\\\""; break;
                case '\n': out += "\\n"; break;
                case '\t': out += "\\t"; break;
                default: out.push_back(c); break;
            }
        }
        return out;
    };

    switch (valueType) {
        case ValueKind::INT_LIT:            return "int: " + to_string(intValue);
        case ValueKind::FLOAT_LIT:          return "float: " + to_string(floatValue);
        case ValueKind::BOOL_LIT:           return string("bool: ") + (boolValue ? "true" : "false");
        case ValueKind::CHAR_LIT:           return "char: '" + string(1, charValue) + "'";
        case ValueKind::STRING_LIT:         return "string: " + escapeString(stringValue);
        case ValueKind::NIL:                return "nil";
        case ValueKind::OBJC_INT_LIT:       return "OBJ-C int: " + to_string(intValue);
        case ValueKind::OBJC_FLOAT_LIT:     return "OBJ-C float: " + to_string(floatValue);
        case ValueKind::OBJC_BOOL_LIT:      return "OBJ-C bool: " + ((boolValue) ? string("true") : string("false"));
        case ValueKind::OBJC_STRING_LIT:    return "OBJ-C string: " + stringValue;
        case ValueKind::IDENTIFIER:         return "Identifier: " + stringValue;
        case ValueKind::CLASS_NAME:         return "Class name: " + stringValue;
        default:                            return "UNKNOWN_VALUE";
    }
}

string ValueNode::toDot() const {
    string result;
    appendDotNode(result);
    return result;
}

//--------------------------------------------------------------ReceiverNode--------------------------------------------------------------

ReceiverNode::ReceiverNode() : AstNode() {
    kind = ReceiverKind::NONE;
    className = nullptr;
    expr = nullptr;
}

ReceiverNode* ReceiverNode::createExpr(ExprNode *expr) {
    ReceiverNode *node = new ReceiverNode();
    node->kind = ReceiverKind::EXPR;
    node->expr = expr;
    return node;
}

ReceiverNode* ReceiverNode::createClassName(ValueNode *className) {
    ReceiverNode *node = new ReceiverNode();
    node->kind = ReceiverKind::CLASS_NAME;
    node->className = className;
    return node;
}

ReceiverNode* ReceiverNode::createSuper() {
    ReceiverNode *node = new ReceiverNode();
    node->kind = ReceiverKind::SUPER;
    return node;
}

ReceiverKind ReceiverNode::getKind() const {
    return kind;
}

ValueNode* ReceiverNode::getClassName() const {
    return className;
}

ExprNode* ReceiverNode::getExpr() const {
    return expr;
}

string ReceiverNode::getDotLabel() const {
    switch (kind) {
        case ReceiverKind::EXPR:          return "EXPR_RECEIVER";
        case ReceiverKind::CLASS_NAME:    return "CLASS_RECEIVER";
        case ReceiverKind::SUPER:         return "SUPER_RECEIVER";
        default:            return "RECEIVER";
    }
}

string ReceiverNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, className, "class_name");
    appendDotEdge(result, expr, "expr");
    return result;
}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------

MsgArgNode::MsgArgNode() : AstNode() {
    identifier = nullptr;
    arg = nullptr;
}

MsgArgNode* MsgArgNode::createMsgArg(ValueNode *identifier, ExprNode *arg) {
    MsgArgNode *node = new MsgArgNode();
    node->identifier = identifier;
    node->arg = arg;
    return node;
}

ValueNode* MsgArgNode::getIdentifier() const {
    return identifier;
}

ExprNode* MsgArgNode::getArg() const {
    return arg;
}

string MsgArgNode::getDotLabel() const {
    return "MESSAGE_ARG";
}

string MsgArgNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, identifier, "id");
    appendDotEdge(result, arg, "arg");
    return result;
}

//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

MsgArgListNode::MsgArgListNode() : AstNode() {
    msgArgs = nullptr;
}

MsgArgListNode* MsgArgListNode::createMsgArgList(MsgArgNode *arg) {
    MsgArgListNode *node = new MsgArgListNode();
    node->msgArgs = new list<MsgArgNode*>{arg};
    return node;
}

MsgArgListNode* MsgArgListNode::addMsgArg(MsgArgListNode *list, MsgArgNode *arg) {
    if (!list->msgArgs) {
        list->msgArgs = new std::list<MsgArgNode*>();
    }
    list->msgArgs->push_back(arg);
    return list;
}

list<MsgArgNode*>* MsgArgListNode::getMsgArgList() const {
    return msgArgs;
}

string MsgArgListNode::getDotLabel() const {
    return "MESSAGE_ARG_LIST";
}

string MsgArgListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (msgArgs) {
        int i = 0;
        for (MsgArgNode *arg : *msgArgs) {
            appendDotEdge(result, arg, "arg_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

MsgSelectorNode::MsgSelectorNode() : AstNode() {
    kind = MsgSelectorKind::NONE;
    identifier = nullptr;
    argList = nullptr;
}

MsgSelectorNode* MsgSelectorNode::createSimpleSel(ValueNode *identifier) {
    MsgSelectorNode *node = new MsgSelectorNode();
    node->kind = MsgSelectorKind::SIMPLE_SEL;
    node->identifier = identifier;
    return node;
}

MsgSelectorNode* MsgSelectorNode::createArgumentList(MsgArgListNode *list) {
    MsgSelectorNode *node = new MsgSelectorNode();
    node->kind = MsgSelectorKind::ARGUMENT_LIST;
    node->argList = list;
    return node;
}

MsgSelectorKind MsgSelectorNode::getKind() const {
    return kind;
}

ValueNode* MsgSelectorNode::getIdentifier() const {
    return identifier;
}

MsgArgListNode* MsgSelectorNode::getMsgArgList() const {
    return argList;
}

string MsgSelectorNode::getDotLabel() const {
    switch(kind) {
        case MsgSelectorKind::SIMPLE_SEL:       return "SIMPLE_SELECTOR";
        case MsgSelectorKind::ARGUMENT_LIST:    return "ARGUMENT_LIST";
        default:                                return "UNKNOWN_SELECTOR";
    }
}

string MsgSelectorNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, identifier, "id");
    appendDotEdge(result, argList, "arg_list");
    return result;
}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

ExprListNode::ExprListNode() : AstNode() {
    exprList = nullptr;
}

ExprListNode* ExprListNode::createExprList(ExprNode *expr) {
    ExprListNode *node = new ExprListNode();
    node->exprList = new list<ExprNode*>{expr};
    return node;
}

ExprListNode* ExprListNode::addExprToList(ExprListNode *exprList, ExprNode *expr) {
    if (!exprList->exprList) {
        exprList->exprList = new std::list<ExprNode*>();
    }
    exprList->exprList->push_back(expr);
    return exprList;
}

list<ExprNode*>* ExprListNode::getExprList() const {
    return exprList;
}

string ExprListNode::getDotLabel() const {
    return "EXPR_LIST";
}

string ExprListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (exprList) {
        int i = 0;
        for (ExprNode *expr : *exprList) {
            appendDotEdge(result, expr, "expr_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

ExprNode::ExprNode() : AstNode() {
    kind = ExprKind::NONE;
    identifier = nullptr;
    literalValue = nullptr;
    left = nullptr;
    right = nullptr;
    operand = nullptr;
    index = nullptr;
    funcId = nullptr;
    args = nullptr;
    receiver = nullptr;
    selector = nullptr;
    objcArrayExprList = nullptr;
    boxedExpr = nullptr;
}

ExprNode* ExprNode::createIdentifier(ValueNode *value) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::IDENTIFIER;
    node->identifier = value;
    return node;
}

ExprNode* ExprNode::createLiteral(ValueNode *value) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::LITERAL;
    node->literalValue = value;
    return node;
}

ExprNode* ExprNode::createObjcArrayLiteral(ExprListNode *exprList) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::OBJC_ARRAY_LITERAL;
    node->objcArrayExprList = exprList;
    return node;
}

ExprNode* ExprNode::createObjcBoxedExpr(ExprNode *expr) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::OBJC_BOXED_EXPR;
    node->boxedExpr = expr;
    return node;
}

ExprNode* ExprNode::createNil() {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::NIL;
    return node;
}

ExprNode* ExprNode::createBoxedExpr(ExprNode *expr) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::BOXED_EXPR;
    node->boxedExpr = expr;
    return node;
}

ExprNode* ExprNode::createMessageSend(ReceiverNode *receiver, MsgSelectorNode *selector) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::MESSAGE;
    node->receiver = receiver;
    node->selector = selector;
    return node;
}

ExprNode* ExprNode::createSelf() {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::SELF;
    return node;
}

ExprNode* ExprNode::createUnaryMinus(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::UNARY_MINUS;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createNot(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::NOT;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createPostInc(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::POST_INC;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createPostDec(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::POST_DEC;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createAddition(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::ADDITION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createSubtraction(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::SUBTRACTION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createMultiplication(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::MULTIPLICATION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createDivision(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::DIVISION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createNotEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::NOT_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createGreater(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::GREATER;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createLess(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::LESS;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createLessOrEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::LESS_OR_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createGreaterOrEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::GREATER_OR_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createAnd(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::AND;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createOr(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::OR;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createAssign(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::ASSIGN;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createArrayAccess(ExprNode *operand, ExprNode *index) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::ARRAY_ACCESS;
    node->operand = operand;
    node->index = index;
    return node;
}

ExprNode* ExprNode::createFunctionCall(ValueNode *funcId, ExprListNode *args) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::FUNCTION_CALL;
    node->funcId = funcId;
    node->args = args;
    return node;
}

ExprNode* ExprNode::createDot(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::DOT;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createArrow(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->kind = ExprKind::ARROW;
    node->left = left;
    node->right = right;
    return node;
}

ExprKind ExprNode::getKind() const {
    return kind;
}

ValueNode* ExprNode::getIdentifier() const {
    return identifier;
}

ValueNode* ExprNode::getLiteral() const {
    return literalValue;
}

ExprNode* ExprNode::getLeft() const {
    return left;
}

ExprNode* ExprNode::getRight() const {
    return right;
}

ExprNode* ExprNode::getOperand() const {
    return operand;
}

ExprNode* ExprNode::getIndex() const {
    return index;
}

ValueNode* ExprNode::getFuncId() const {
    return funcId;
}

ExprListNode* ExprNode::getArgs() const {
    return args;
}

ReceiverNode* ExprNode::getReceiver() const {
    return receiver;
}

MsgSelectorNode* ExprNode::getSelector() const {
    return selector;
}

ExprListNode* ExprNode::getObjcArrayExprList() const {
    return objcArrayExprList;
}

ExprNode* ExprNode::getBoxedExpr() const {
    return boxedExpr;
}

void ExprNode::setFieldRefConstantId(int id) {
    fieldRefConstantId = id;
}

int ExprNode::getFieldRefConstantId() const {
    return fieldRefConstantId;
}

void ExprNode::setMethodRefConstantId(int id) {
    methodRefConstantId = id;
}

int ExprNode::getMethodRefConstantId() const {
    return methodRefConstantId;
}

void ExprNode::setIsFieldAccess(bool val) {
    isFieldAccess = val;
}

bool ExprNode::getIsFieldAccess() const {
    return isFieldAccess;
}

void ExprNode::setIsMethodCall(bool val) {
    isMethodCall = val;
}

bool ExprNode::getIsMethodCall() const {
    return isMethodCall;
}

void ExprNode::setClassName(const string& name) {
    className = name;
}

string ExprNode::getClassName() const {
    return className;
}

string ExprNode::getDotLabel() const {
    switch (kind) {
        case ExprKind::IDENTIFIER:                return "IDENTIFIER";
        case ExprKind::LITERAL:                   return "LITERAL";
        case ExprKind::OBJC_ARRAY_LITERAL:        return "@[]";
        case ExprKind::OBJC_BOXED_EXPR:           return "@()";
        case ExprKind::NIL:                       return "NIL";
        case ExprKind::BOXED_EXPR:                return "BOXED_EXPR";
        case ExprKind::MESSAGE:                   return "MESSAGE";
        case ExprKind::SELF:                      return "SELF";
        case ExprKind::UNARY_MINUS:               return "UNARY_MINUS";
        case ExprKind::NOT:                       return "!";
        case ExprKind::POST_INC:                  return "POST_INC";
        case ExprKind::POST_DEC:                  return "POST_DEC";
        case ExprKind::ADDITION:                  return "+";
        case ExprKind::SUBTRACTION:               return "-";
        case ExprKind::MULTIPLICATION:            return "*";
        case ExprKind::DIVISION:                  return "/";
        case ExprKind::EQUAL:                     return "==";
        case ExprKind::NOT_EQUAL:                 return "!=";
        case ExprKind::GREATER:                   return ">";
        case ExprKind::LESS:                      return "<";
        case ExprKind::LESS_OR_EQUAL:             return "<=";
        case ExprKind::GREATER_OR_EQUAL:          return ">=";
        case ExprKind::AND:                       return "&&";
        case ExprKind::OR:                        return "||";
        case ExprKind::ASSIGN:                    return "=";
        case ExprKind::ARRAY_ACCESS:              return "[]";
        case ExprKind::FUNCTION_CALL:             return "FUNC_CALL";
        case ExprKind::DOT:                       return ".";
        case ExprKind::ARROW:                     return "->";
        default:                                  return "UNKNOWN_EXPR";
    }
}

string ExprNode::toDot() const {
    string result;
    appendDotNode(result);

    appendDotEdge(result, identifier, "id");
    appendDotEdge(result, literalValue, "literal");
    appendDotEdge(result, left, "left");
    appendDotEdge(result, right, "right");
    appendDotEdge(result, operand, "operand");
    appendDotEdge(result, index, "index");
    appendDotEdge(result, funcId, "func");
    appendDotEdge(result, args, "args");
    appendDotEdge(result, receiver, "receiver");
    appendDotEdge(result, selector, "selector");
    appendDotEdge(result, objcArrayExprList, "array_exprs");
    appendDotEdge(result, boxedExpr, "boxed_expr");

    return result;
}

//--------------------------------------------------------------TypeNode--------------------------------------------------------------

TypeNode::TypeNode() : AstNode() {
    kind = TypeKind::NONE;
    classNameValue = nullptr;
}

TypeNode* TypeNode::createIntType() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::INT;
    return node;
}

TypeNode* TypeNode::createCharType() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::CHAR;
    return node;
}

TypeNode* TypeNode::createFloatType() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::FLOAT;
    return node;
}

TypeNode* TypeNode::createBoolType() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::BOOL;
    return node;
}

TypeNode* TypeNode::createIdType() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::TYPE_ID;
    return node;
}

TypeNode* TypeNode::createClassNameType(ValueNode *classNameValue) {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::CLASS_NAME;
    node->classNameValue = classNameValue;
    return node;
}

TypeNode* TypeNode::createVoid() {
    TypeNode *node = new TypeNode();
    node->kind = TypeKind::VOID;
    return node;
}

bool TypeNode::isPrimitive() const {
    return  kind == TypeKind::INT ||
            kind == TypeKind::FLOAT ||
            kind == TypeKind::CHAR ||
            kind == TypeKind::BOOL ||
            kind == TypeKind::VOID;
}

bool TypeNode::isNumeric() const {
    return kind == TypeKind::INT || kind == TypeKind::FLOAT || kind == TypeKind::BOOL;
}

TypeKind TypeNode::getKind() const {
    return kind;
}

ValueNode* TypeNode::getClassName() const {
    return classNameValue;
}

string TypeNode::getDotLabel() const {
    switch (kind) {
        case TypeKind::INT:         return "INT";
        case TypeKind::FLOAT:       return "FLOAT";
        case TypeKind::BOOL:        return "BOOL";
        case TypeKind::CHAR:        return "CHAR";
        case TypeKind::TYPE_ID:     return "TYPE_ID";
        case TypeKind::CLASS_NAME:  return "CLASS_NAME";
        case TypeKind::VOID:        return "VOID";
        default:                    return "UNKNOWN_TYPE";
    }
}

string TypeNode::toDot() const {
    string res;
    appendDotNode(res);
    appendDotEdge(res, classNameValue, "class_name");
    return res;
}

//--------------------------------------------------------------DeclaratorListNode--------------------------------------------------------------

DeclaratorListNode::DeclaratorListNode() : AstNode() {
    initDeclList = nullptr;
}

DeclaratorListNode* DeclaratorListNode::createDeclaratorList(InitDeclNode *initDecl) {
    DeclaratorListNode *node = new DeclaratorListNode();
    node->initDeclList = new list<InitDeclNode*>{initDecl};
    return node;
}

DeclaratorListNode* DeclaratorListNode::addInitDecl(DeclaratorListNode *declaratorList, InitDeclNode *initDecl) {
    if (!declaratorList->initDeclList) {
        declaratorList->initDeclList = new std::list<InitDeclNode*>();
    }
    declaratorList->initDeclList->push_back(initDecl);
    return declaratorList;
}

list<InitDeclNode*>* DeclaratorListNode::getInitDeclList() const {
    return initDeclList;
}

string DeclaratorListNode::getDotLabel() const {
    return "DECLARATOR_LIST";
}

string DeclaratorListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (initDeclList) {
        int i = 0;
        for (InitDeclNode *decl : *initDeclList) {
            appendDotEdge(result, decl, "decl_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------DeclNode--------------------------------------------------------------

DeclNode::DeclNode() : AstNode() {
    type = nullptr;
    declaratorList = nullptr;
}

DeclNode* DeclNode::createDecl(TypeNode *type, DeclaratorListNode *declaratorList) {
    DeclNode *node = new DeclNode();
    node->type = type;
    node->declaratorList = declaratorList;
    return node;
}

TypeNode* DeclNode::getType() const {
    return type;
}

DeclaratorListNode* DeclNode::getDeclaratorList() const {
    return declaratorList;
}

string DeclNode::getDotLabel() const {
    return "DECLARATION";
}

string DeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, declaratorList, "declarators");
    return result;
}

//--------------------------------------------------------------StmtListNode--------------------------------------------------------------

StmtListNode::StmtListNode() : AstNode() {
    stmts = nullptr;
}

StmtListNode* StmtListNode::createStmtList() {
    StmtListNode *node = new StmtListNode();
    node->stmts = new list<StmtNode*>();
    return node;
}

StmtListNode* StmtListNode::createStmtList(StmtNode *stmt) {
    StmtListNode *node = new StmtListNode();
    node->stmts = new list<StmtNode*>{stmt};
    return node;
}

StmtListNode* StmtListNode::addStmtToList(StmtListNode *list, StmtNode *stmt) {
    if (!list->stmts) {
        list->stmts = new std::list<StmtNode*>();
    }
    list->stmts->push_back(stmt);
    return list;
}

list<StmtNode*>* StmtListNode::getStmtList() const {
    return stmts;
}

string StmtListNode::getDotLabel() const {
    return "STMT_LIST";
}

string StmtListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (stmts) {
        int i = 0;
        for (StmtNode *stmt : *stmts) {
            appendDotEdge(result, stmt, "stmt_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

StmtNode::StmtNode() : AstNode() {
    kind = StmtKind::NONE;
    expr = nullptr;
    condition = nullptr;
    thenBranch = nullptr;
    elseBranch = nullptr;
    post = nullptr;
    forInId = nullptr;
    forInType = nullptr;
    collection = nullptr;
    body = nullptr;
    compound = nullptr;
    decl = nullptr;
}

StmtNode* StmtNode::createEmpty() {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::EMPTY;
    return node;
}

StmtNode* StmtNode::createExpr(ExprNode *expr) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::EXPR;
    node->expr = expr;
    return node;
}

StmtNode* StmtNode::createReturn(ExprNode *expr) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::RETURN;
    node->expr = expr;
    return node;
}

StmtNode* StmtNode::createIf(ExprNode *condition, StmtNode *thenBranch) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::IF;
    node->condition = condition;
    node->thenBranch = thenBranch;
    return node;
}

StmtNode* StmtNode::createIfElse(ExprNode *condition, StmtNode *thenBranch, StmtNode *elseBranch) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::IF_ELSE;
    node->condition = condition;
    node->thenBranch = thenBranch;
    node->elseBranch = elseBranch;
    return node;
}

StmtNode* StmtNode::createFor(ExprNode *expr, ExprNode *condition, ExprNode *post, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::FOR_WITH_EXPR;
    node->expr = expr;
    node->condition = condition;
    node->post = post;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createFor(DeclNode *decl, ExprNode *condition, ExprNode *post, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::FOR_WITH_DECL;
    node->decl = decl;
    node->condition = condition;
    node->post = post;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createForIn(ValueNode *id, ExprNode *collection, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::FOR_IN;
    node->forInId = id;
    node->collection = collection;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createTypedForIn(TypeNode *type, ValueNode *id, ExprNode *collection, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::TYPED_FOR_IN;
    node->forInType = type;
    node->forInId = id;
    node->collection = collection;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createWhile(ExprNode *condition, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::WHILE;
    node->condition = condition;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createDoWhile(StmtNode *body, ExprNode *condition) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::DO_WHILE;
    node->body = body;
    node->condition = condition;
    return node;
}

StmtNode* StmtNode::createCompound(StmtListNode *compound) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::COMPOUND;
    node->compound = compound;
    return node;
}

StmtNode* StmtNode::createDeclaration(DeclNode *decl) {
    StmtNode *node = new StmtNode();
    node->kind = StmtKind::DECLARATION;
    node->decl = decl;
    return node;
}

StmtKind StmtNode::getKind() const {
    return kind;
}

ExprNode* StmtNode::getExpr() const {
    return expr;
}

ExprNode* StmtNode::getCondition() const {
    return condition;
}

StmtNode* StmtNode::getThenBranch() const {
    return thenBranch;
}

StmtNode* StmtNode::getElseBranch() const {
    return elseBranch;
}

ExprNode* StmtNode::getPost() const {
    return post;
}

ValueNode* StmtNode::getForInId() const {
    return forInId;
}

TypeNode* StmtNode::getForInType() const {
    return forInType;
}

ExprNode* StmtNode::getCollection() const {
    return collection;
}

StmtNode* StmtNode::getBody() const {
    return body;
}


StmtListNode* StmtNode::getCompound() const {
    return compound;
}

DeclNode* StmtNode::getDecl() const {
    return decl;
}


string StmtNode::getDotLabel() const {
    switch (kind) {
        case StmtKind::EMPTY:         return "EMPTY_STMT";
        case StmtKind::EXPR:          return "EXPR_STMT";
        case StmtKind::RETURN:        return "RETURN";
        case StmtKind::IF:            return "IF";
        case StmtKind::IF_ELSE:       return "IF_ELSE";
        case StmtKind::FOR_WITH_EXPR: return "FOR";
        case StmtKind::FOR_WITH_DECL: return "FOR";
        case StmtKind::FOR_IN:        return "FOR_IN";
        case StmtKind::TYPED_FOR_IN:  return "TYPED_FOR_IN";
        case StmtKind::WHILE:         return "WHILE";
        case StmtKind::DO_WHILE:      return "DO_WHILE";
        case StmtKind::COMPOUND:      return "COMPOUND";
        case StmtKind::DECLARATION:   return "DECLARATION";
        default:            return "UNKNOWN_STMT";
    }
}

string StmtNode::toDot() const {
    string result;
    appendDotNode(result);

    appendDotEdge(result, expr, "expr");
    appendDotEdge(result, condition, "condition");
    appendDotEdge(result, thenBranch, "then");
    appendDotEdge(result, elseBranch, "else");
    appendDotEdge(result, post, "post");
    appendDotEdge(result, forInId, "for_in_id");
    appendDotEdge(result, forInType, "for_in_type");
    appendDotEdge(result, collection, "collection");
    appendDotEdge(result, body, "body");
    appendDotEdge(result, compound, "compound");
    appendDotEdge(result, decl, "decl");

    return result;
}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

ArraySizeSpecNode::ArraySizeSpecNode() : AstNode() {
    sizes = nullptr;
}

ArraySizeSpecNode* ArraySizeSpecNode::createArraySizeSpec(ExprNode *size) {
    ArraySizeSpecNode *node = new ArraySizeSpecNode();
    node->sizes = new list<ExprNode*>{size};
    return node;
}

ArraySizeSpecNode* ArraySizeSpecNode::addDimension(ArraySizeSpecNode *spec, ExprNode *size) {
    if (!spec->sizes) {
        spec->sizes = new std::list<ExprNode*>();
    }
    spec->sizes->push_back(size);
    return spec;
}

list<ExprNode*>* ArraySizeSpecNode::getSizes() const {
    return sizes;
}

string ArraySizeSpecNode::getDotLabel() const {
    return "ARRAY_SIZE_SPEC";
}

string ArraySizeSpecNode::toDot() const {
    string result;
    appendDotNode(result);

    if (sizes) {
        int i = 0;
        for (ExprNode *size : *sizes) {
            appendDotEdge(result, size, "size_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

ParamDeclNode::ParamDeclNode() : AstNode() {
    kind = ParamDeclKind::NONE;
    type = nullptr;
    identifier = nullptr;
    arraySizeSpec = nullptr;
}

ParamDeclNode* ParamDeclNode::createParamDecl(TypeNode *type, ValueNode *identifier) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ParamDeclKind::IDENTIFIER;
    node->type = type;
    node->identifier = identifier;
    return node;
}

ParamDeclNode* ParamDeclNode::createArrayParamDecl(TypeNode *type, ValueNode *identifier) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ParamDeclKind::ARRAY;
    node->type = type;
    node->identifier = identifier;
    return node;
}

ParamDeclNode* ParamDeclNode::createSizedArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ParamDeclKind::SIZED_ARRAY;
    node->type = type;
    node->identifier = identifier;
    node->arraySizeSpec = arraySizeSpec;
    return node;
}

ParamDeclNode* ParamDeclNode::createFlexibleArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ParamDeclKind::FLEXIBLE_ARRAY;
    node->type = type;
    node->identifier = identifier;
    node->arraySizeSpec = arraySizeSpec;
    return node;
}

bool ParamDeclNode::isArray() const {
    return arraySizeSpec != nullptr &&
    (kind == ParamDeclKind::ARRAY ||
    kind == ParamDeclKind::SIZED_ARRAY ||
    kind == ParamDeclKind::FLEXIBLE_ARRAY);
}

ParamDeclKind ParamDeclNode::getKind() const {
    return kind;
}

TypeNode* ParamDeclNode::getType() const {
    return type;
}

ValueNode* ParamDeclNode::getIdentifier() const {
    return identifier;
}

ArraySizeSpecNode* ParamDeclNode::getSizeSpec() const {
    return arraySizeSpec;
}

vector<int> ParamDeclNode::getArraySizes() const {
    vector<int> sizeList;

    if (kind == ParamDeclKind::ARRAY || kind == ParamDeclKind::FLEXIBLE_ARRAY) {
        sizeList.push_back(0);
    }

    if (kind == ParamDeclKind::SIZED_ARRAY && arraySizeSpec) {
        list<ExprNode*> sizes = *arraySizeSpec->getSizes();
        for (auto size : sizes) {
            if (!size || !size->getLiteral() || size->getKind() != ExprKind::LITERAL) {
                throw runtime_error("Invalid size expression: null pointer. Source: ParamDeclNode::getArraySizes()");
            }
            if (!(size->getLiteral()->getValueKind() == ValueKind::INT_LIT)) {
                throw runtime_error("Invalid array size: size must be integer. Source: ParamDeclNode::getArraySizes()");
            }

            int val = size->getLiteral()->getInt();

            if (val <= 0) {
                throw runtime_error("Array size must be positive, got: " + std::to_string(val) + ". Source: ParamDeclNode::getArraySizes()");
            }
            sizeList.push_back(val);
        }
    }
    return sizeList;
}

string ParamDeclNode::getDotLabel() const {
    switch (kind) {
        case ParamDeclKind::IDENTIFIER:         return "PARAM_DECL";
        case ParamDeclKind::ARRAY:              return "ARRAY_PARAM_DECL";
        case ParamDeclKind::SIZED_ARRAY:        return "SIZED_ARRAY_PARAM_DECL";
        case ParamDeclKind::FLEXIBLE_ARRAY:     return "FLEXIBLE_ARRAY_PARAM_DECL";
        default:                                return "UNKNOWN_PARAM_DECL";
    }
}

string ParamDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "id");
    appendDotEdge(result, arraySizeSpec, "size_spec");
    return result;
}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

ParamListNode::ParamListNode() : AstNode() {
    paramList = nullptr;
}

ParamListNode* ParamListNode::createParamList() {
    ParamListNode *node = new ParamListNode();
    node->paramList = new list<ParamDeclNode*>();
    return node;
}

ParamListNode* ParamListNode::createParamList(ParamDeclNode *paramDecl) {
    ParamListNode *node = new ParamListNode();
    node->paramList = new list<ParamDeclNode*>{paramDecl};
    return node;
}

ParamListNode* ParamListNode::addParamDecl(ParamListNode *paramList, ParamDeclNode *paramDecl) {
    if (!paramList->paramList) {
        paramList->paramList = new std::list<ParamDeclNode*>();
    }
    paramList->paramList->push_back(paramDecl);
    return paramList;
}

list<ParamDeclNode*>* ParamListNode::getParamList() const {
    return paramList;
}

string ParamListNode::getDotLabel() const {
    return "PARAM_LIST";
}

string ParamListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (paramList) {
        int i = 0;
        for (ParamDeclNode *param : *paramList) {
            appendDotEdge(result, param, "param_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

FuncDefNode::FuncDefNode() : AstNode() {
    type = nullptr;
    identifier = nullptr;
    paramList = nullptr;
    compoundStmt = nullptr;
}

FuncDefNode* FuncDefNode::createFuncDef(TypeNode *type, ValueNode *identifier, ParamListNode *paramList, StmtNode *compoundStmt) {
    FuncDefNode *node = new FuncDefNode();
    node->type = type;
    node->identifier = identifier;
    node->paramList = paramList;
    node->compoundStmt = compoundStmt;
    return node;
}

TypeNode* FuncDefNode::getType() const {
    return type;
}

ValueNode* FuncDefNode::getIdentifier() const {
    return identifier;
}

ParamListNode* FuncDefNode::getParamList() const {
    return paramList;
}

StmtNode* FuncDefNode::getCompoundStmt() const {
    return compoundStmt;
}

string FuncDefNode::getDotLabel() const {
    return "FUNC_DEF";
}

string FuncDefNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "return_type");
    appendDotEdge(result, identifier, "name");
    appendDotEdge(result, paramList, "param_list");
    appendDotEdge(result, compoundStmt, "compound_stmt");
    return result;
}

//--------------------------------------------------------------FuncDeclNode--------------------------------------------------------------

FuncDeclNode::FuncDeclNode() : AstNode() {
    type = nullptr;
    identifier = nullptr;
    paramList = nullptr;
}

FuncDeclNode* FuncDeclNode::createFuncDecl(TypeNode *type, ValueNode *identifier, ParamListNode *paramList) {
    FuncDeclNode *node = new FuncDeclNode();
    node->type = type;
    node->identifier = identifier;
    node->paramList = paramList;
    return node;
}

TypeNode* FuncDeclNode::getType() const {
    return type;
}

ValueNode* FuncDeclNode::getIdentifier() const {
    return identifier;
}

ParamListNode* FuncDeclNode::getParamList() const {
    return paramList;
}

string FuncDeclNode::getDotLabel() const {
    return "FUNC_DECL";
}

string FuncDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "return_type");
    appendDotEdge(result, identifier, "name");
    appendDotEdge(result, paramList, "params");
    return result;
}

//--------------------------------------------------------------MethodParamNode--------------------------------------------------------------

MethodParamNode::MethodParamNode() : AstNode() {
    kind = MethodParamKind::NONE;
    selectorIdentifier = nullptr;
    type = nullptr;
    paramIdentifier = nullptr;
    arraySizeSpec = nullptr;
}

MethodParamNode* MethodParamNode::createMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = MethodParamKind::IDENTIFIER;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = MethodParamKind::ARRAY;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createSizedArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = MethodParamKind::SIZED_ARRAY;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->arraySizeSpec = sizeSpec;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createFlexibleArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = MethodParamKind::FLEXIBLE_ARRAY;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->arraySizeSpec = sizeSpec;
    node->paramIdentifier = paramIdentifier;
    return node;
}

bool MethodParamNode::isArray() const {
    return arraySizeSpec != nullptr &&
    (kind == MethodParamKind::ARRAY ||
    kind == MethodParamKind::SIZED_ARRAY ||
    kind == MethodParamKind::FLEXIBLE_ARRAY);
}

MethodParamKind MethodParamNode::getKind() const {
    return kind;
}

ValueNode* MethodParamNode::getSelectorIdentifier() const {
    return selectorIdentifier;
}

TypeNode* MethodParamNode::getType() const {
    return type;
}

ValueNode* MethodParamNode::getParamIdentifier() const {
    return paramIdentifier;
}

ArraySizeSpecNode* MethodParamNode::getArraySizeSpec() const {
    return arraySizeSpec;
}

vector<int> MethodParamNode::getArraySizes() const {
    vector<int> sizeList;

    if (kind == MethodParamKind::ARRAY || kind == MethodParamKind::FLEXIBLE_ARRAY) {
        sizeList.push_back(0);
    }

    if (kind == MethodParamKind::SIZED_ARRAY && arraySizeSpec) {
        list<ExprNode*> sizes = *arraySizeSpec->getSizes();
        for (auto size : sizes) {
            if (!size || !size->getLiteral() || size->getKind() != ExprKind::LITERAL) {
                throw runtime_error("Invalid size expression: null pointer. Source: ParamDeclNode::getArraySizes()");
            }
            if (!(size->getLiteral()->getValueKind() == ValueKind::INT_LIT)) {
                throw runtime_error("Invalid array size: size must be integer. Source: ParamDeclNode::getArraySizes()");
            }

            int val = size->getLiteral()->getInt();

            if (val <= 0) {
                throw runtime_error("Array size must be positive, got: " + std::to_string(val));
            }
            sizeList.push_back(val);
        }
    }
    return sizeList;
}

string MethodParamNode::getDotLabel() const {
    switch (kind) {
        case MethodParamKind::IDENTIFIER:           return "METHOD_PARAM";
        case MethodParamKind::ARRAY:                return "ARRAY_METHOD_PARAM";
        case MethodParamKind::SIZED_ARRAY:          return "SIZED_ARRAY_METHOD_PARAM";
        case MethodParamKind::FLEXIBLE_ARRAY:       return "FLEXIBLE_ARRAY_METHOD_PARAM";
        default:                                    return "UNKNOWN_METHOD_PARAM";
    }
}

string MethodParamNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, selectorIdentifier, "selector");
    appendDotEdge(result, type, "type");
    appendDotEdge(result, paramIdentifier, "param_id");
    appendDotEdge(result, arraySizeSpec, "size_spec");
    return result;
}

//--------------------------------------------------------------MethodSelNode--------------------------------------------------------------

MethodSelNode::MethodSelNode(): AstNode() {
    methodParams = nullptr;
}

MethodSelNode* MethodSelNode::createMethodSel(MethodParamNode *methodParam) {
    MethodSelNode *node = new MethodSelNode();
    node->methodParams = new list<MethodParamNode*>{methodParam};
    return node;
}

MethodSelNode* MethodSelNode::addMethodParam(MethodSelNode *methodSel, MethodParamNode *methodParam) {
    if (!methodSel->methodParams) {
        methodSel->methodParams = new list<MethodParamNode*>();
    }
    methodSel->methodParams->push_back(methodParam);
    return methodSel;
}

list<MethodParamNode*>* MethodSelNode::getMethodParamList() const {
    return methodParams;
}

string MethodSelNode::getDotLabel() const {
    return "METHOD_SELECTOR";
}

string MethodSelNode::toDot() const {
    string result;
    appendDotNode(result);

    if (methodParams) {
        int i = 0;
        for (MethodParamNode *param : *methodParams) {
            appendDotEdge(result, param, "param_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------MethodDefNode--------------------------------------------------------------

MethodDefNode::MethodDefNode() : AstNode() {
    kind = MethodDefKind::NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
    compoundStmt = nullptr;
}

MethodDefNode* MethodDefNode::createInstanceMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt) {
    MethodDefNode *node = new MethodDefNode();
    node->kind = MethodDefKind::ID;
    node->type = type;
    node->identifier = identifier;
    node->compoundStmt = compoundStmt;
    node->isInstanceMethodFlag = true;
    return node;
}

MethodDefNode* MethodDefNode::createInstanceMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt) {
    MethodDefNode *node = new MethodDefNode();
    node->kind = MethodDefKind::SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->compoundStmt = compoundStmt;
    node->isInstanceMethodFlag = true;
    return node;
}

MethodDefNode* MethodDefNode::createClassMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt) {
    MethodDefNode *node = new MethodDefNode();
    node->kind = MethodDefKind::ID;
    node->type = type;
    node->identifier = identifier;
    node->compoundStmt = compoundStmt;
    node->isInstanceMethodFlag = false;
    return node;
}

MethodDefNode* MethodDefNode::createClassMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt) {
    MethodDefNode *node = new MethodDefNode();
    node->kind = MethodDefKind::SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->compoundStmt = compoundStmt;
    node->isInstanceMethodFlag = false;
    return node;
}

MethodDefKind MethodDefNode::getKind() const {
    return kind;
}

TypeNode* MethodDefNode::getType() const {
    return type;
}

ValueNode* MethodDefNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* MethodDefNode::getMethodSel() const {
    return methodSel;
}

StmtNode* MethodDefNode::getCompoundStmt() const {
    return compoundStmt;
}

bool MethodDefNode::isInstanceMethod() const {
    return isInstanceMethodFlag;
}

bool MethodDefNode::isClassMethod() const {
    return !isInstanceMethodFlag;
}

string MethodDefNode::getDotLabel() const {
    if (isInstanceMethodFlag) {
        switch (kind) {
            case MethodDefKind::ID:     return "INST_METHOD_DEF_NO_ARGS";
            case MethodDefKind::SEL:    return "INST_METHOD_DEF_HAS_ARGS";
            default:                    return "UNKNOWN_METHOD_DEF";
        }
    }
    else {
        switch (kind) {
            case MethodDefKind::ID:     return "CLASS_METHOD_DEF_NO_ARGS";
            case MethodDefKind::SEL:    return "CLASS_METHOD_DEF_HAS_ARGS";
            default:                    return "UNKNOWN_METHOD_DEF";
        }
    }
}

string MethodDefNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "identifier");
    appendDotEdge(result, methodSel, "method_sel");
    appendDotEdge(result, compoundStmt, "compound_stmt");
    return result;
}

//--------------------------------------------------------------ImplementationDefListNode--------------------------------------------------------------

ImplementationDefListNode::ImplementationDefListNode() : AstNode() {
    classMethodDefs = nullptr;
    instanceMethodDefs = nullptr;
}

ImplementationDefListNode* ImplementationDefListNode::createImplementationDefListWithProperty(PropertyNode *property) {
    ImplementationDefListNode *node = new ImplementationDefListNode();
    node->properties = new list<PropertyNode*>{property};
    node->classMethodDefs = new list<MethodDefNode*>();
    node->instanceMethodDefs = new list<MethodDefNode*>();
    return node;
}

ImplementationDefListNode* ImplementationDefListNode::createImplementationDefListWithClassMethod(MethodDefNode *classMethodDef) {
    ImplementationDefListNode *node = new ImplementationDefListNode();
    node->properties = new list<PropertyNode*>();
    node->classMethodDefs = new list<MethodDefNode*>{classMethodDef};
    node->instanceMethodDefs = new list<MethodDefNode*>();
    return node;
}

ImplementationDefListNode* ImplementationDefListNode::createImplementationDefListWithInstMethod(MethodDefNode *instanceMethodDef) {
    ImplementationDefListNode *node = new ImplementationDefListNode();
    node->properties = new list<PropertyNode*>();
    node->classMethodDefs = new list<MethodDefNode*>();
    node->instanceMethodDefs = new list<MethodDefNode*>{instanceMethodDef};
    return node;
}

ImplementationDefListNode* ImplementationDefListNode::addProperty(ImplementationDefListNode *implementationDefList, PropertyNode* property) {
    if (!implementationDefList->properties) {
        implementationDefList->properties = new std::list<PropertyNode*>();
    }
    implementationDefList->properties->push_back(property);
    return implementationDefList;
}

ImplementationDefListNode* ImplementationDefListNode::addClassMethodDef(ImplementationDefListNode *implementationDefList, MethodDefNode *classMethodDef) {
    if (!implementationDefList->classMethodDefs) {
        implementationDefList->classMethodDefs = new std::list<MethodDefNode*>();
    }
    implementationDefList->classMethodDefs->push_back(classMethodDef);
    return implementationDefList;
}

ImplementationDefListNode* ImplementationDefListNode::addInstanceMethodDef(ImplementationDefListNode *implementationDefList, MethodDefNode *instanceMethodDef) {
    if (!implementationDefList->instanceMethodDefs) {
        implementationDefList->instanceMethodDefs = new std::list<MethodDefNode*>();
    }
    implementationDefList->instanceMethodDefs->push_back(instanceMethodDef);
    return implementationDefList;
}

list<PropertyNode*>* ImplementationDefListNode::getproperties() const {
    return properties;
}

list<MethodDefNode*>* ImplementationDefListNode::getClassMethodDefs() const {
    return classMethodDefs;
}

list<MethodDefNode*>* ImplementationDefListNode::getInstanceMethodDefs() const {
    return instanceMethodDefs;
}

string ImplementationDefListNode::getDotLabel() const {
    return "IMPLEMENTATION_DEF_LIST";
}

string ImplementationDefListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (classMethodDefs) {
        int i = 0;
        for (MethodDefNode *method : *classMethodDefs) {
            appendDotEdge(result, method, "class_method_" + to_string(i++));
        }
    }

    if (instanceMethodDefs) {
        int i = 0;
        for (MethodDefNode *method : *instanceMethodDefs) {
            appendDotEdge(result, method, "instance_method_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------MethodDeclNode--------------------------------------------------------------

MethodDeclNode::MethodDeclNode() : AstNode() {
    kind = MethodDeclKind::NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
}

MethodDeclNode* MethodDeclNode::createInstanceMethodDecl(TypeNode *type, ValueNode *identifier) {
    MethodDeclNode *node = new MethodDeclNode();
    node->kind = MethodDeclKind::ID;
    node->type = type;
    node->identifier = identifier;
    node->isInstanceMethodFlag = true;
    return node;
}

MethodDeclNode* MethodDeclNode::createInstanceMethodDecl(TypeNode *type, MethodSelNode *methodSel) {
    MethodDeclNode *node = new MethodDeclNode();
    node->kind = MethodDeclKind::SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->isInstanceMethodFlag = true;
    return node;
}

MethodDeclNode* MethodDeclNode::createClassMethodDecl(TypeNode *type, ValueNode *identifier) {
    MethodDeclNode *node = new MethodDeclNode();
    node->kind = MethodDeclKind::ID;
    node->type = type;
    node->identifier = identifier;
    node->isInstanceMethodFlag = false;
    return node;
}

MethodDeclNode* MethodDeclNode::createClassMethodDecl(TypeNode *type, MethodSelNode *methodSel) {
    MethodDeclNode *node = new MethodDeclNode();
    node->kind = MethodDeclKind::SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->isInstanceMethodFlag = false;
    return node;
}

MethodDeclKind MethodDeclNode::getKind() const {
    return kind;
}

TypeNode* MethodDeclNode::getType() const {
    return type;
}

ValueNode* MethodDeclNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* MethodDeclNode::getMethodSel() const {
    return methodSel;
}

bool MethodDeclNode::isInstanceMethod() const {
    return isInstanceMethodFlag;
}

bool MethodDeclNode::isClassMethod() const {
    return !isInstanceMethodFlag;
}

string MethodDeclNode::getDotLabel() const {
    if (isInstanceMethodFlag) {
        switch (kind) {
            case MethodDeclKind::ID:    return "INST_METHOD_DECL_NO_ARGS";
            case MethodDeclKind::SEL:   return "INST_METHOD_DECL_HAS_ARGS";
            default:                    return "UNKNOWN_METHOD_DECL";
        }
    }
    else {
        switch (kind) {
            case MethodDeclKind::ID:    return "CLASS_METHOD_DECL_NO_ARGS";
            case MethodDeclKind::SEL:   return "CLASS_METHOD_DECL_HAS_ARGS";
            default:                    return "UNKNOWN_METHOD_DECL";
        }
    }
}

string MethodDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "identifier");
    appendDotEdge(result, methodSel, "method_sel");
    return result;
}

//--------------------------------------------------------------PropertyNode--------------------------------------------------------------

PropertyNode::PropertyNode() : AstNode() {
    attribute = Attribute::NONE;
    type = nullptr;
    name = nullptr;
}

PropertyNode* PropertyNode::createProperty(Attribute attr, TypeNode *type, ValueNode *name) {
    PropertyNode *node = new PropertyNode();
    node->attribute = attr;
    node->type = type;
    node->name = name;
    return node;
}

PropertyNode* PropertyNode::createProperty(TypeNode *type, ValueNode *name) {
    PropertyNode *node = new PropertyNode();
    node->type = type;
    node->name = name;
    return node;
}

Attribute PropertyNode::getAttribute() const {
    return attribute;
}

TypeNode* PropertyNode::getType() const {
    return type;
}

ValueNode* PropertyNode::getName() const {
    return name;
}

string PropertyNode::getDotLabel() const {
    switch (attribute) {
        case Attribute::READONLY:   return "READONLY_PROPERTY";
        case Attribute::READWRITE:  return "READWRITE_PROPERTY";
        default:                    return "NO_ATTR_PROPERTY";
    }
}

string PropertyNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, name, "name");
    return result;
}

//--------------------------------------------------------------InterfaceDeclListNode--------------------------------------------------------------

InterfaceDeclListNode::InterfaceDeclListNode() : AstNode() {
    properties = nullptr;
    classMethodDecls = nullptr;
    instanceMethodDecls = nullptr;
}

InterfaceDeclListNode* InterfaceDeclListNode::createInterfaceDeclList() {
    InterfaceDeclListNode *node = new InterfaceDeclListNode();
    node->properties = new list<PropertyNode*>();
    node->classMethodDecls = new list<MethodDeclNode*>();
    node->instanceMethodDecls = new list<MethodDeclNode*>();
    return node;
}

InterfaceDeclListNode* InterfaceDeclListNode::addProperty(InterfaceDeclListNode *interfaceDeclList, PropertyNode *property) {
    if (!interfaceDeclList->properties) {
        interfaceDeclList->properties = new std::list<PropertyNode*>();
    }
    interfaceDeclList->properties->push_back(property);
    return interfaceDeclList;
}

InterfaceDeclListNode* InterfaceDeclListNode::addClassMethodDecl(InterfaceDeclListNode *interfaceDeclList, MethodDeclNode *classMethodDecl) {
    if (!interfaceDeclList->classMethodDecls) {
        interfaceDeclList->classMethodDecls = new std::list<MethodDeclNode*>();
    }
    interfaceDeclList->classMethodDecls->push_back(classMethodDecl);
    return interfaceDeclList;
}

InterfaceDeclListNode* InterfaceDeclListNode::addInstanceMethodDecl(InterfaceDeclListNode *interfaceDeclList, MethodDeclNode *instanceMethodDecl) {
    if (!interfaceDeclList->instanceMethodDecls) {
        interfaceDeclList->instanceMethodDecls = new std::list<MethodDeclNode*>();
    }
    interfaceDeclList->instanceMethodDecls->push_back(instanceMethodDecl);
    return interfaceDeclList;
}

list<PropertyNode*>* InterfaceDeclListNode::getProperties() const {
    return properties;
}

list<MethodDeclNode*>* InterfaceDeclListNode::getClassMethodDecls() const {
    return classMethodDecls;
}

list<MethodDeclNode*>* InterfaceDeclListNode::getInstanceMethodDecls() const {
    return instanceMethodDecls;
}

string InterfaceDeclListNode::getDotLabel() const {
    return "INTERFACE_DECL_LIST";
}

string InterfaceDeclListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (properties) {
        int i = 0;
        for (PropertyNode *prop : *properties) {
            appendDotEdge(result, prop, "property_" + to_string(i++));
        }
    }

    if (classMethodDecls) {
        int i = 0;
        for (MethodDeclNode *method : *classMethodDecls) {
            appendDotEdge(result, method, "class_method_" + to_string(i++));
        }
    }

    if (instanceMethodDecls) {
        int i = 0;
        for (MethodDeclNode *method : *instanceMethodDecls) {
            appendDotEdge(result, method, "instance_method_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------InitializerListNode--------------------------------------------------------------

InitializerListNode::InitializerListNode() : AstNode() {
    initializers = nullptr;
}

InitializerListNode* InitializerListNode::createInitializerList() {
    InitializerListNode *node = new InitializerListNode();
    node->initializers = new list<InitializerNode*>();
    return node;
}

InitializerListNode* InitializerListNode::createInitializerList(InitializerNode *initializer) {
    InitializerListNode *node = new InitializerListNode();
    node->initializers = new list<InitializerNode*>{initializer};
    return node;
}

InitializerListNode* InitializerListNode::addInitializer(InitializerListNode *initList, InitializerNode *initializer) {
    if (!initList->initializers) {
        initList->initializers = new std::list<InitializerNode*>();
    }
    initList->initializers->push_back(initializer);
    return initList;
}

list<InitializerNode*>* InitializerListNode::getInitializerList() const {
    return initializers;
}

string InitializerListNode::getDotLabel() const {
    return "INITIALIZER_LIST";
}

string InitializerListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (initializers) {
        int i = 0;
        for (InitializerNode *init : *initializers) {
            appendDotEdge(result, init, "init_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------InitializerNode--------------------------------------------------------------

InitializerNode::InitializerNode() : AstNode() {
    kind = InitializerKind::NONE;
    expr = nullptr;
    initList = nullptr;
}

InitializerNode* InitializerNode::createExpr(ExprNode *expr) {
    InitializerNode *node = new InitializerNode();
    node->kind = InitializerKind::EXPR;
    node->expr = expr;
    return node;
}

InitializerNode* InitializerNode::createArrayInitializer(InitializerListNode *initList) {
    InitializerNode *node = new InitializerNode();
    node->kind = InitializerKind::ARRAY;
    node->initList = initList;
    return node;
}

InitializerKind InitializerNode::getKind() const {
    return kind;
}

ExprNode* InitializerNode::getExpr() const {
    return expr;
}

InitializerListNode* InitializerNode::getInitializerList() const {
    return initList;
}

string InitializerNode::getDotLabel() const {
    switch(kind) {
        case InitializerKind::EXPR:     return "EXPR_INITIALIZER";
        case InitializerKind::ARRAY:    return "ARRAY_INITIALIZER";
        default:                        return "UNKNOWN_INITIALIZER";
    }
}

string InitializerNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, expr, "expr");
    appendDotEdge(result, initList, "init_list");
    return result;
}

//--------------------------------------------------------------DeclaratorNode--------------------------------------------------------------

DeclaratorNode::DeclaratorNode() : AstNode() {
    identifier = nullptr;
    arraySizes = nullptr;
}

DeclaratorNode* DeclaratorNode::createDeclarator(ValueNode *identifier) {
    DeclaratorNode *node = new DeclaratorNode();
    node->identifier = identifier;
    return node;
}

DeclaratorNode* DeclaratorNode::addArrayAccess(DeclaratorNode *decl, ExprNode *size) {
    if (!decl->arraySizes) {
        decl->arraySizes = new std::list<ExprNode*>();
    }
    decl->arraySizes->push_back(size);
    return decl;
}

ValueNode* DeclaratorNode::getIdentifier() const {
    return identifier;
}

list<ExprNode*>* DeclaratorNode::getArraySizes() const {
    return arraySizes;
}

string DeclaratorNode::getDotLabel() const {
    return "DECLARATOR";
}

string DeclaratorNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, identifier, "id");
    
    if (arraySizes) {
        int i = 0;
        for (ExprNode *size : *arraySizes) {
            appendDotEdge(result, size, "size_" + to_string(i++));
        }
    }
    
    return result;
}

//--------------------------------------------------------------InitDeclNode--------------------------------------------------------------

InitDeclNode::InitDeclNode() : AstNode() {
    kind = InitDeclKind::NONE;
    declarator = nullptr;
    initializer = nullptr;
}

InitDeclNode* InitDeclNode::createDeclarator(DeclaratorNode *declarator) {
    InitDeclNode *node = new InitDeclNode();
    node->kind = InitDeclKind::DECLARATOR;
    node->declarator = declarator;
    return node;
}

InitDeclNode* InitDeclNode::createInitialized(DeclaratorNode *declarator, InitializerNode *initializer) {
    InitDeclNode *node = new InitDeclNode();
    node->kind = InitDeclKind::INITIALIZED;
    node->declarator = declarator;
    node->initializer = initializer;
    return node;
}

InitDeclNode* InitDeclNode::createArrayInitialized(DeclaratorNode *declarator, InitializerNode *initializer) {
    InitDeclNode *node = new InitDeclNode();
    node->kind = InitDeclKind::ARRAY_INITIALIZED;
    node->declarator = declarator;
    node->initializer = initializer;
    return node;
}

InitDeclKind InitDeclNode::getKind() const {
    return kind;
}

DeclaratorNode* InitDeclNode::getDeclarator() const {
    return declarator;
}

InitializerNode* InitDeclNode::getInitializer() const {
    return initializer;
}

string InitDeclNode::getDotLabel() const {
    switch (kind) {
        case InitDeclKind::DECLARATOR:          return "DECLARATOR_ONLY";
        case InitDeclKind::INITIALIZED:         return "INITIALIZED_DECL";
        case InitDeclKind::ARRAY_INITIALIZED:   return "ARRAY_INITIALIZED_DECL";
        default:                                return "UNKNOWN_INIT_DECL";
    }
}

string InitDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, declarator, "declarator");
    appendDotEdge(result, initializer, "initializer");
    return result;
}

//--------------------------------------------------------------AccessModifierNode--------------------------------------------------------------

AccessModifierNode::AccessModifierNode() : AstNode() {
    accessType = AccessModifier::NONE;
}

AccessModifierNode* AccessModifierNode::createPublic() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = AccessModifier::PUBLIC;
    return node;
}

AccessModifierNode* AccessModifierNode::createProtected() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = AccessModifier::PROTECTED;
    return node;
}

AccessModifierNode* AccessModifierNode::createPrivate() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = AccessModifier::PRIVATE;
    return node;
}

AccessModifier AccessModifierNode::getAccessType() const {
    return accessType;
}

string AccessModifierNode::getDotLabel() const {
    switch (accessType) {
        case AccessModifier::PUBLIC:    return "PUBLIC";
        case AccessModifier::PROTECTED: return "PROTECTED";
        case AccessModifier::PRIVATE:   return "PRIVATE";
        default:                        return "UNKNOWN_ACCESS_MODIFIER";
    }
}

string AccessModifierNode::toDot() const {
    string result;
    appendDotNode(result);
    return result;
}

//--------------------------------------------------------------InstanceVarDeclNode--------------------------------------------------------------

InstanceVarDeclNode::InstanceVarDeclNode() : AstNode() {
    accessModifier = nullptr;
    type = nullptr;
    initDecl = nullptr;
}

InstanceVarDeclNode* InstanceVarDeclNode::createInstanceVarDecl(AccessModifierNode *accessModifier, TypeNode *type, InitDeclNode *initDecl) {
    InstanceVarDeclNode *node = new InstanceVarDeclNode();
    node->accessModifier = accessModifier;
    node->type = type;
    node->initDecl = initDecl;
    return node;
}

AccessModifierNode* InstanceVarDeclNode::getAccessModifier() const {
    return accessModifier;
}

TypeNode* InstanceVarDeclNode::getType() const {
    return type;
}

InitDeclNode* InstanceVarDeclNode::getInitDecl() const {
    return initDecl;
}

string InstanceVarDeclNode::getDotLabel() const {
    return "INSTANCE_VAR_DECL";
}

string InstanceVarDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, accessModifier, "access_modifier");
    appendDotEdge(result, type, "type");
    appendDotEdge(result, initDecl, "init_decl");
    return result;
}

//--------------------------------------------------------------InstanceVarsDeclListNode--------------------------------------------------------------

InstanceVarsDeclListNode::InstanceVarsDeclListNode() : AstNode() {
    instanceVarDecls = nullptr;
}

InstanceVarsDeclListNode* InstanceVarsDeclListNode::createInstanceVarsDeclList(InstanceVarDeclNode *instanceVarDecl) {
    InstanceVarsDeclListNode *node = new InstanceVarsDeclListNode();
    node->instanceVarDecls = new list<InstanceVarDeclNode*>{instanceVarDecl};
    return node;
}

InstanceVarsDeclListNode* InstanceVarsDeclListNode::addInstanceVarDecl(InstanceVarDeclNode *instanceVarDecl, InstanceVarsDeclListNode *instanceVarsDeclList) {
    if (!instanceVarsDeclList->instanceVarDecls) {
        instanceVarsDeclList->instanceVarDecls = new std::list<InstanceVarDeclNode*>();
    }
    instanceVarsDeclList->instanceVarDecls->push_back(instanceVarDecl);
    return instanceVarsDeclList;
}

list<InstanceVarDeclNode*>* InstanceVarsDeclListNode::getInstanceVarsDeclList() const {
    return instanceVarDecls;
}

string InstanceVarsDeclListNode::getDotLabel() const {
    return "INSTANCE_VARS_DECL_LIST";
}

string InstanceVarsDeclListNode::toDot() const {
    string result;
    appendDotNode(result);
    
    if (instanceVarDecls) {
        int i = 0;
        for (InstanceVarDeclNode *instanceVarDecl : *instanceVarDecls) {
            appendDotEdge(result, instanceVarDecl, "inst_decl_" + to_string(i++));
        }
    }
    
    return result;
}

//--------------------------------------------------------------InstanceVarsNode--------------------------------------------------------------

InstanceVarsNode::InstanceVarsNode() : AstNode() {
    instanceVarsDeclList = nullptr;
}

InstanceVarsNode* InstanceVarsNode::createInstanceVars() {
    InstanceVarsNode *node = new InstanceVarsNode();
    return node;
}

InstanceVarsNode* InstanceVarsNode::createInstanceVars(InstanceVarsDeclListNode *instanceVarsDeclList) {
    InstanceVarsNode *node = new InstanceVarsNode();
    node->instanceVarsDeclList = instanceVarsDeclList;
    return node;
}

InstanceVarsDeclListNode* InstanceVarsNode::getInstanceVarsDeclList() const {
    return instanceVarsDeclList;
}

string InstanceVarsNode::getDotLabel() const {
    return "INSTANCE_VARS";
}

string InstanceVarsNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, instanceVarsDeclList, "instance_vars_decl_list");
    return result;
}

//--------------------------------------------------------------ImplementationNode--------------------------------------------------------------

ImplementationNode::ImplementationNode() : AstNode() {
    className = nullptr;
    superClassName = nullptr;
    instanceVars = nullptr;
    implDefList = nullptr;
}

ImplementationNode* ImplementationNode::createImplementation(ValueNode *className, InstanceVarsNode *instanceVars, ImplementationDefListNode *implDefList) {
    ImplementationNode *node = new ImplementationNode();
    node->className = className;
    node->instanceVars = instanceVars;
    node->implDefList = implDefList;
    return node;
}

ImplementationNode* ImplementationNode::createImplementation(ValueNode *className, ValueNode *superClassName, InstanceVarsNode *instanceVars, ImplementationDefListNode *implDefList) {
    ImplementationNode *node = new ImplementationNode();
    node->className = className;
    node->superClassName = superClassName;
    node->instanceVars = instanceVars;
    node->implDefList = implDefList;
    return node;
}

ValueNode* ImplementationNode::getClassName() const {
    return className;
}

ValueNode* ImplementationNode::getSuperClassName() const {
    return superClassName;
}

InstanceVarsNode* ImplementationNode::getInstanceVars() const {
    return instanceVars;
}

ImplementationDefListNode* ImplementationNode::getImplDefList() const {
    return implDefList;
}

void ImplementationNode::setClassName(string className) {
    if (!this->className) {
        this->className = ValueNode::createClassName(&className);
    }
    else {
        this->className->setClassName(className);
    }
}

void ImplementationNode::setSuperClassName(string superClassName) {
    if (!this->superClassName) {
        this->superClassName = ValueNode::createClassName(&superClassName);
    }
    else {
        this->superClassName->setClassName(superClassName);
    }
}

string ImplementationNode::getDotLabel() const {
    return "IMPLEMENTATION";
}

string ImplementationNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, className, "class_name");
    appendDotEdge(result, superClassName, "super_class_name");
    appendDotEdge(result, instanceVars, "instance_vars");
    appendDotEdge(result, implDefList, "impl_def_list");
    return result;
}

//--------------------------------------------------------------InterfaceNode--------------------------------------------------------------

InterfaceNode::InterfaceNode() : AstNode() {
    className = nullptr;
    superClassName = nullptr;
    instanceVars = nullptr;
    interfaceDeclList = nullptr;
}

InterfaceNode* InterfaceNode::createInterface(ValueNode *className, InstanceVarsNode *instanceVars, InterfaceDeclListNode *interfaceDeclList) {
    InterfaceNode *node = new InterfaceNode();
    node->className = className;
    node->instanceVars = instanceVars;
    node->interfaceDeclList = interfaceDeclList;
    return node;
}

InterfaceNode* InterfaceNode::createInterface(ValueNode *className, ValueNode *superClassName, InstanceVarsNode *instanceVars, InterfaceDeclListNode *interfaceDeclList) {
    InterfaceNode *node = new InterfaceNode();
    node->className = className;
    node->superClassName = superClassName;
    node->instanceVars = instanceVars;
    node->interfaceDeclList = interfaceDeclList;
    return node;
}

ValueNode* InterfaceNode::getClassName() const {
    return className;
}

ValueNode* InterfaceNode::getSuperClassName() const {
    return superClassName;
}

InstanceVarsNode* InterfaceNode::getInstanceVars() const {
    return instanceVars;
}

InterfaceDeclListNode* InterfaceNode::getInterfaceDeclList() const {
    return interfaceDeclList;
}

void InterfaceNode::setClassName(string className) {
    if (!this->className) {
        this->className = ValueNode::createClassName(&className);
    }
    else {
        this->className->setClassName(className);
    }
}

void InterfaceNode::setSuperClassName(string superClassName) {
    if (!this->superClassName) {
        this->superClassName = ValueNode::createClassName(&superClassName);
    }
    else {
        this->superClassName->setClassName(superClassName);
    }
}

string InterfaceNode::getDotLabel() const {
    return "INTERFACE";
}

string InterfaceNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, className, "class_name");
    appendDotEdge(result, superClassName, "super_class_name");
    appendDotEdge(result, instanceVars, "instance_vars");
    appendDotEdge(result, interfaceDeclList, "interface_decl_list");
    return result;
}

//--------------------------------------------------------------ClassNameListNode--------------------------------------------------------------

ClassNameListNode::ClassNameListNode() : AstNode() {
    classFwDeclList = nullptr;
}

ClassNameListNode* ClassNameListNode::createClassFwDeclList(ValueNode *className) {
    ClassNameListNode *node = new ClassNameListNode();
    node->classFwDeclList = new list<ValueNode*>{className};
    return node;
}

ClassNameListNode* ClassNameListNode::addClassFwDecl(ClassNameListNode *classFwDeclList, ValueNode *className) {
    if (!classFwDeclList->classFwDeclList) {
        classFwDeclList->classFwDeclList = new std::list<ValueNode*>();
    }
    classFwDeclList->classFwDeclList->push_back(className);
    return classFwDeclList;
}

list<ValueNode*>* ClassNameListNode::getClassFwDeclList() const {
    return classFwDeclList;
}

string ClassNameListNode::getDotLabel() const {
    return "CLASS_NAME_LIST";
}

string ClassNameListNode::toDot() const {
    string result;
    appendDotNode(result);
    
    if (classFwDeclList) {
        int i = 0;
        for (ValueNode *classFwDecl : *classFwDeclList) {
            appendDotEdge(result, classFwDecl, "class_name_" + to_string(i++));
        }
    }
    
    return result;
}

//--------------------------------------------------------------ExternalDeclNode--------------------------------------------------------------

ExternalDeclNode::ExternalDeclNode() : AstNode() {
    kind = ExternalDeclKind::NONE;
    interface = nullptr;
    implementation = nullptr;
    classNames = nullptr;
    funcDecl = nullptr;
    funcDef = nullptr;
}

ExternalDeclNode* ExternalDeclNode::createInterface(InterfaceNode *interface) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->kind = ExternalDeclKind::INTERFACE;
    node->interface = interface;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createImplementation(ImplementationNode *implementation) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->kind = ExternalDeclKind::IMPLEMENTATION;
    node->implementation = implementation;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFwClassDeclList(ClassNameListNode *classNames) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->kind = ExternalDeclKind::CLASS_FW_DECL_LIST;
    node->classNames = classNames;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFuncDecl(FuncDeclNode *funcDecl) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->kind = ExternalDeclKind::FUNC_DECL;
    node->funcDecl = funcDecl;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFuncDef(FuncDefNode *funcDef) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->kind = ExternalDeclKind::FUNC_DEF;
    node->funcDef = funcDef;
    return node;
}

ExternalDeclKind ExternalDeclNode::getKind() const {
    return kind;
}

InterfaceNode* ExternalDeclNode::getInterface() const {
    return interface;
}

ImplementationNode* ExternalDeclNode::getImplementation() const {
    return implementation;
}

ClassNameListNode* ExternalDeclNode::getClassNameList() const {
    return classNames;
}

FuncDeclNode* ExternalDeclNode::getFuncDecl() const {
    return funcDecl;
}

FuncDefNode* ExternalDeclNode::getFuncDef() const {
    return funcDef;
}

string ExternalDeclNode::getDotLabel() const {
    switch (kind) {
        case ExternalDeclKind::INTERFACE:               return "INTERFACE";
        case ExternalDeclKind::IMPLEMENTATION:          return "IMPLEMENTATION";
        case ExternalDeclKind::CLASS_FW_DECL_LIST:      return "CLASS_FW_DECL_LIST";
        case ExternalDeclKind::FUNC_DECL:               return "FUNC_DECL";
        case ExternalDeclKind::FUNC_DEF:                return "FUNC_DEF";
        default:                                        return "UNKNOWN_EXTERNAL_DECL";
    }
}

string ExternalDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, interface, "interface");
    appendDotEdge(result, implementation, "implementation");
    appendDotEdge(result, classNames, "class_fw_decl_list");
    appendDotEdge(result, funcDecl, "func_decl");
    appendDotEdge(result, funcDef, "func_def");
    return result;
}

//--------------------------------------------------------------ExternalDeclListNode--------------------------------------------------------------

ExternalDeclListNode::ExternalDeclListNode() : AstNode() {
    externalDeclList = nullptr;
}

ExternalDeclListNode* ExternalDeclListNode::createExternalDeclList() {
    ExternalDeclListNode *node = new ExternalDeclListNode();
    node->externalDeclList = new list<ExternalDeclNode*>();
    return node;
}

ExternalDeclListNode* ExternalDeclListNode::createExternalDeclList(ExternalDeclNode *externalDecl) {
    ExternalDeclListNode *node = new ExternalDeclListNode();
    node->externalDeclList = new list<ExternalDeclNode*>{externalDecl};
    return node;
}

ExternalDeclListNode* ExternalDeclListNode::addExternalDecl(ExternalDeclListNode *externalDeclList, ExternalDeclNode *externalDecl) {
    if (!externalDeclList->externalDeclList) {
        externalDeclList->externalDeclList = new std::list<ExternalDeclNode*>();
    }
    externalDeclList->externalDeclList->push_back(externalDecl);
    return externalDeclList;
}

list<ExternalDeclNode*>* ExternalDeclListNode::getExternalDeclList() const {
    return externalDeclList;
}

string ExternalDeclListNode::getDotLabel() const {
    return "EXTERNAL_DECL_LIST";
}

string ExternalDeclListNode::toDot() const {
    string result;
    appendDotNode(result);

    if (externalDeclList) {
        int i = 0;
        for (ExternalDeclNode *decl : *externalDeclList) {
            appendDotEdge(result, decl, "external_decl_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------ProgramNode--------------------------------------------------------------

ProgramNode::ProgramNode() : AstNode() {
    externalDeclList = nullptr;
}

ProgramNode* ProgramNode::createProgram(ExternalDeclListNode *externalDeclList) {
    ProgramNode *node = new ProgramNode();
    node->externalDeclList = externalDeclList;
    return node;
}

ExternalDeclListNode* ProgramNode::getExternalDeclList() const {
    return externalDeclList;
}

string ProgramNode::getDotLabel() const {
    return "PROGRAM";
}

string ProgramNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, externalDeclList, "external_decls");
    return result;
}
