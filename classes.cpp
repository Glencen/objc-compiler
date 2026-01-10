#include "classes.h"

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
    valueType = NONE;
    intValue = 0;
    floatValue = 0;
    boolValue = false;
    charValue = 0;
    stringValue = nullptr;
}

ValueNode* ValueNode::createInt(int value) {
    ValueNode *node = new ValueNode();
    node->valueType = INT_LIT;
    node->intValue = value;
    return node;
}

ValueNode* ValueNode::createFloat(float value) {
    ValueNode *node = new ValueNode();
    node->valueType = FLOAT_LIT;
    node->floatValue = value;
    return node;
}

ValueNode* ValueNode::createBool(bool value) {
    ValueNode *node = new ValueNode();
    node->valueType = BOOL_LIT;
    node->boolValue = value;
    return node;
}

ValueNode* ValueNode::createChar(char value) {
    ValueNode *node = new ValueNode();
    node->valueType = CHAR_LIT;
    node->charValue = value;
    return node;
}

ValueNode* ValueNode::createString(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = STRING_LIT;
    node->stringValue = value;
    return node;
}

ValueNode* ValueNode::createNil() {
    ValueNode *node = new ValueNode();
    node->valueType = NIL;
    return node;
}

ValueNode* ValueNode::createObjcInt(int value) {
    ValueNode *node = new ValueNode();
    node->valueType = OBJC_INT_LIT;
    node->intValue = value;
    return node;
}

ValueNode* ValueNode::createObjcFloat(float value) {
    ValueNode *node = new ValueNode();
    node->valueType = OBJC_FLOAT_LIT;
    node->floatValue = value;
    return node;
}

ValueNode* ValueNode::createObjcBool(bool value) {
    ValueNode *node = new ValueNode();
    node->valueType = OBJC_BOOL_LIT;
    node->boolValue = value;
    return node;
}

ValueNode* ValueNode::createObjcString(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = OBJC_STRING_LIT;
    node->stringValue = value;
    return node;
}

ValueNode* ValueNode::createIdentifier(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = IDENTIFIER;
    node->stringValue = value;
    return node;
}

ValueNode* ValueNode::createClassName(string *value) {
    ValueNode *node = new ValueNode();
    node->valueType = CLASS_NAME;
    node->stringValue = value;
    return node;
}

ValueNode::ValueType ValueNode::getValueType() const {
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

string* ValueNode::getString() const {
    return stringValue;
}

string* ValueNode::getObjcInt() const {
    return stringValue;
}

string* ValueNode::getObjcFloat() const {
    return stringValue;
}

string* ValueNode::getObjcBool() const {
    return stringValue;
}

string* ValueNode::getObjcString() const {
    return stringValue;
}

string* ValueNode::getIdentifier() const {
    return stringValue;
}

string* ValueNode::getClassName() const {
    return stringValue;
}

string ValueNode::getDotLabel() const {
    auto escapeString = [](const string &src) {
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
        case INT_LIT:           return "int: " + to_string(intValue);
        case FLOAT_LIT:         return "float: " + to_string(floatValue);
        case BOOL_LIT:          return string("bool: ") + (boolValue ? "true" : "false");
        case CHAR_LIT:          return "char: '" + string(1, charValue) + "'";
        case STRING_LIT:        return "string: " + escapeString(*stringValue);
        case NIL:               return "nil";
        case OBJC_INT_LIT:      return "OBJ-C int: " + to_string(intValue);
        case OBJC_FLOAT_LIT:    return "OBJ-C float: " + to_string(floatValue);
        case OBJC_BOOL_LIT:     return "OBJ-C bool: " + ((boolValue) ? string("true") : string("false"));
        case OBJC_STRING_LIT:   return "OBJ-C string: " + *stringValue;
        case IDENTIFIER:        return "Identifier: " + *stringValue;
        case CLASS_NAME:        return "Class name: " + *stringValue;
        default:                return "UNKNOWN_VALUE";
    }
}

string ValueNode::toDot() const {
    string result;
    appendDotNode(result);
    return result;
}

//--------------------------------------------------------------ReceiverNode--------------------------------------------------------------

ReceiverNode::ReceiverNode() : AstNode() {
    type = NONE;
    className = nullptr;
    expr = nullptr;
}

ReceiverNode* ReceiverNode::createExpr(ExprNode *expr) {
    ReceiverNode *node = new ReceiverNode();
    node->type = EXPR;
    node->expr = expr;
    return node;
}

ReceiverNode* ReceiverNode::createClassName(ValueNode *className) {
    ReceiverNode *node = new ReceiverNode();
    node->type = CLASS_NAME;
    node->className = className;
    return node;
}

ReceiverNode* ReceiverNode::createSuper() {
    ReceiverNode *node = new ReceiverNode();
    node->type = SUPER;
    return node;
}

ReceiverNode::ReceiverType ReceiverNode::getType() const {
    return type;
}

ExprNode* ReceiverNode::getExpr() const {
    return expr;
}

string ReceiverNode::getDotLabel() const {
    switch (type) {
        case EXPR:          return "EXPR_RECEIVER";
        case CLASS_NAME:    return "CLASS_RECEIVER";
        case SUPER:         return "SUPER_RECEIVER";
        default:            return "RECEIVER";
    }
}

string ReceiverNode::toDot() const {
    string result;
    appendDotNode(result);
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
    return result;
}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

MsgSelectorNode::MsgSelectorNode() : AstNode() {
    type = NONE;
    identifier = nullptr;
    argList = nullptr;
}

MsgSelectorNode* MsgSelectorNode::createSimpleSel(ValueNode *identifier) {
    MsgSelectorNode *node = new MsgSelectorNode();
    node->type = SIMPLE_SEL;
    node->identifier = identifier;
    return node;
}

MsgSelectorNode* MsgSelectorNode::createArgumentList(MsgArgListNode *list) {
    MsgSelectorNode *node = new MsgSelectorNode();
    node->type = ARGUMENT_LIST;
    node->argList = list;
    return node;
}

MsgSelectorNode::MsgSelectorType MsgSelectorNode::getType() const {
    return type;
}

ValueNode* MsgSelectorNode::getIdentifier() const {
    return identifier;
}

MsgArgListNode* MsgSelectorNode::getMsgArgList() const {
    return argList;
}

string MsgSelectorNode::getDotLabel() const {
    return "MESSAGE_SELECTOR";
}

string MsgSelectorNode::toDot() const {
    string result;
    appendDotNode(result);
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
    return result;
}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

ExprNode::ExprNode() : AstNode() {
    type = NONE;
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
    node->type = IDENTIFIER;
    node->identifier = value;
    return node;
}

ExprNode* ExprNode::createLiteral(ValueNode *value) {
    ExprNode *node = new ExprNode();
    node->type = LITERAL;
    node->literalValue = value;
    return node;
}

ExprNode* ExprNode::createObjcArrayLiteral(ExprListNode *exprList) {
    ExprNode *node = new ExprNode();
    node->type = OBJC_ARRAY_LITERAL;
    node->objcArrayExprList = exprList;
    return node;
}

ExprNode* ExprNode::createObjcBoxedExpr(ExprNode *expr) {
    ExprNode *node = new ExprNode();
    node->type = OBJC_BOXED_EXPR;
    node->boxedExpr = expr;
    return node;
}

ExprNode* ExprNode::createNil() {
    ExprNode *node = new ExprNode();
    node->type = NIL;
    return node;
}

ExprNode* ExprNode::createBoxedExpr(ExprNode *expr) {
    ExprNode *node = new ExprNode();
    node->type = BOXED_EXPR;
    node->boxedExpr = expr;
    return node;
}

ExprNode* ExprNode::createMessageSend(ReceiverNode *receiver, MsgSelectorNode *selector) {
    ExprNode *node = new ExprNode();
    node->type = MESSAGE;
    node->receiver = receiver;
    node->selector = selector;
    return node;
}

ExprNode* ExprNode::createSelf() {
    ExprNode *node = new ExprNode();
    node->type = SELF;
    return node;
}

ExprNode* ExprNode::createUnaryMinus(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->type = UNARY_MINUS;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createNot(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->type = NOT;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createPostInc(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->type = POST_INC;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createPostDec(ExprNode *operand) {
    ExprNode *node = new ExprNode();
    node->type = POST_DEC;
    node->operand = operand;
    return node;
}

ExprNode* ExprNode::createAddition(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = ADDITION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createSubtraction(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = SUBTRACTION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createMultiplication(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = MULTIPLICATION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createDivision(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = DIVISION;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createNotEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = NOT_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createGreater(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = GREATER;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createLess(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = LESS;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createLessOrEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = LESS_OR_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createGreaterOrEqual(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = GREATER_OR_EQUAL;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createAnd(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = AND;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createOr(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = OR;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createAssign(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = ASSIGN;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createArrayAccess(ExprNode *operand, ExprNode *index) {
    ExprNode *node = new ExprNode();
    node->type = ARRAY_ACCESS;
    node->operand = operand;
    node->index = index;
    return node;
}

ExprNode* ExprNode::createFunctionCall(ValueNode *funcId, ExprListNode *args) {
    ExprNode *node = new ExprNode();
    node->type = FUNCTION_CALL;
    node->funcId = funcId;
    node->args = args;
    return node;
}

ExprNode* ExprNode::createDot(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = DOT;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode* ExprNode::createArrow(ExprNode *left, ExprNode *right) {
    ExprNode *node = new ExprNode();
    node->type = ARROW;
    node->left = left;
    node->right = right;
    return node;
}

ExprNode::ExprType ExprNode::getType() const {
    return type;
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

string ExprNode::getDotLabel() const {
    switch (type) {
        case IDENTIFIER:                return "IDENTIFIER";
        case LITERAL:                   return "LITERAL";
        case OBJC_ARRAY_LITERAL:        return "@[]";
        case OBJC_BOXED_EXPR:           return "@()";
        case NIL:                       return "NIL";
        case BOXED_EXPR:                return "BOXED_EXPR";
        case MESSAGE:                   return "MESSAGE";
        case SELF:                      return "SELF";
        case UNARY_MINUS:               return "UNARY_MINUS";
        case NOT:                       return "!";
        case POST_INC:                  return "POST_INC";
        case POST_DEC:                  return "POST_DEC";
        case ADDITION:                  return "+";
        case SUBTRACTION:               return "-";
        case MULTIPLICATION:            return "*";
        case DIVISION:                  return "/";
        case EQUAL:                     return "==";
        case NOT_EQUAL:                 return "!=";
        case GREATER:                   return ">";
        case LESS:                      return "<";
        case LESS_OR_EQUAL:             return "<=";
        case GREATER_OR_EQUAL:          return ">=";
        case AND:                       return "&&";
        case OR:                        return "||";
        case ASSIGN:                    return "=";
        case ARRAY_ACCESS:              return "[]";
        case FUNCTION_CALL:             return "FUNC_CALL";
        case DOT:                       return ".";
        case ARROW:                     return "->";
        default:                        return "UNKNOWN_EXPR";
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
    kind = NONE;
    classNameValue = nullptr;
}

TypeNode* TypeNode::createIntType() {
    TypeNode *node = new TypeNode();
    node->kind = INT;
    return node;
}

TypeNode* TypeNode::createCharType() {
    TypeNode *node = new TypeNode();
    node->kind = CHAR;
    return node;
}

TypeNode* TypeNode::createFloatType() {
    TypeNode *node = new TypeNode();
    node->kind = FLOAT;
    return node;
}

TypeNode* TypeNode::createBoolType() {
    TypeNode *node = new TypeNode();
    node->kind = BOOL;
    return node;
}

TypeNode* TypeNode::createIdType() {
    TypeNode *node = new TypeNode();
    node->kind = TYPE_ID;
    return node;
}

TypeNode* TypeNode::createClassNameType(ValueNode *classNameValue) {
    TypeNode *node = new TypeNode();
    node->kind = CLASS_NAME;
    node->classNameValue = classNameValue;
    return node;
}

TypeNode* TypeNode::createVoid() {
    TypeNode *node = new TypeNode();
    node->kind = VOID;
    return node;
}

TypeNode::TypeKind TypeNode::getKind() const {
    return kind;
}

ValueNode* TypeNode::getClassName() const {
    return classNameValue;
}

string TypeNode::getDotLabel() const {
    switch (kind) {
        case INT: return "INT";
        case FLOAT: return "FLOAT";
        case BOOL: return "BOOL";
        case CHAR: return "CHAR";
        case TYPE_ID: return "TYPE_ID";
        case CLASS_NAME: return "CLASS_NAME";
        case VOID: return "VOID";
        default: return "UNKNOWN_TYPE";
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

DeclaratorListNode* DeclaratorListNode::createExternalDeclList(InitDeclNode *initDecl) {
    DeclaratorListNode *node = new DeclaratorListNode();
    node->initDeclList = new list<InitDeclNode*>{initDecl};
    return node;
}

DeclaratorListNode* DeclaratorListNode::addExternalDecl(DeclaratorListNode *declaratorList, InitDeclNode *initDecl) {
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
    type = NONE;
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
    node->type = EMPTY;
    return node;
}

StmtNode* StmtNode::createExpr(ExprNode *expr) {
    StmtNode *node = new StmtNode();
    node->type = EXPR;
    node->expr = expr;
    return node;
}

StmtNode* StmtNode::createReturn(ExprNode *expr) {
    StmtNode *node = new StmtNode();
    node->type = RETURN;
    node->expr = expr;
    return node;
}

StmtNode* StmtNode::createIf(ExprNode *condition, StmtNode *thenBranch) {
    StmtNode *node = new StmtNode();
    node->type = IF;
    node->condition = condition;
    node->thenBranch = thenBranch;
    return node;
}

StmtNode* StmtNode::createIfElse(ExprNode *condition, StmtNode *thenBranch, StmtNode *elseBranch) {
    StmtNode *node = new StmtNode();
    node->type = IF_ELSE;
    node->condition = condition;
    node->thenBranch = thenBranch;
    node->elseBranch = elseBranch;
    return node;
}

StmtNode* StmtNode::createFor(ExprNode *expr, ExprNode *condition, ExprNode *post, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->type = FOR_WITH_EXPR;
    node->expr = expr;
    node->condition = condition;
    node->post = post;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createFor(DeclNode *decl, ExprNode *condition, ExprNode *post, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->type = FOR_WITH_DECL;
    node->decl = decl;
    node->condition = condition;
    node->post = post;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createForIn(ValueNode *id, ExprNode *collection, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->type = FOR_IN;
    node->forInId = id;
    node->collection = collection;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createTypedForIn(TypeNode *type, ValueNode *id, ExprNode *collection, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->type = TYPED_FOR_IN;
    node->forInType = type;
    node->forInId = id;
    node->collection = collection;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createWhile(ExprNode *condition, StmtNode *body) {
    StmtNode *node = new StmtNode();
    node->type = WHILE;
    node->condition = condition;
    node->body = body;
    return node;
}

StmtNode* StmtNode::createDoWhile(StmtNode *body, ExprNode *condition) {
    StmtNode *node = new StmtNode();
    node->type = DO_WHILE;
    node->body = body;
    node->condition = condition;
    return node;
}

StmtNode* StmtNode::createCompound(StmtListNode *compound) {
    StmtNode *node = new StmtNode();
    node->type = COMPOUND;
    node->compound = compound;
    return node;
}

StmtNode* StmtNode::createDeclaration(DeclNode *decl) {
    StmtNode *node = new StmtNode();
    node->type = DECLARATION;
    node->decl = decl;
    return node;
}

string StmtNode::getDotLabel() const {
    switch (type) {
        case EMPTY:         return "EMPTY_STMT";
        case EXPR:          return "EXPR_STMT";
        case RETURN:        return "RETURN";
        case IF:            return "IF";
        case IF_ELSE:       return "IF_ELSE";
        case FOR_WITH_EXPR: return "FOR";
        case FOR_WITH_DECL: return "FOR";
        case FOR_IN:        return "FOR_IN";
        case TYPED_FOR_IN:  return "TYPED_FOR_IN";
        case WHILE:         return "WHILE";
        case DO_WHILE:      return "DO_WHILE";
        case COMPOUND:      return "COMPOUND";
        case DECLARATION:   return "DECLARATION";
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
    kind = NONE;
    type = nullptr;
    identifier = nullptr;
    arraySizeSpec = nullptr;
}

ParamDeclNode* ParamDeclNode::createParamDecl(TypeNode *type, ValueNode *identifier) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = IDENTIFIER;
    node->type = type;
    node->identifier = identifier;
    return node;
}

ParamDeclNode* ParamDeclNode::createArrayParamDecl(TypeNode *type, ValueNode *identifier) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ARRAY;
    node->type = type;
    node->identifier = identifier;
    return node;
}

ParamDeclNode* ParamDeclNode::createSizedArrayParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = SIZED_ARRAY;
    node->type = type;
    node->identifier = identifier;
    node->arraySizeSpec = arraySizeSpec;
    return node;
}

ParamDeclNode* ParamDeclNode::createSizedArrayOfArraysParamDecl(TypeNode *type, ValueNode *identifier, ArraySizeSpecNode *arraySizeSpec) {
    ParamDeclNode *node = new ParamDeclNode();
    node->kind = ARRAY_OF_ARRAYS;
    node->type = type;
    node->identifier = identifier;
    node->arraySizeSpec = arraySizeSpec;
    return node;
}

ParamDeclNode::ParamDeclKind ParamDeclNode::getKind() const {
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

string ParamDeclNode::getDotLabel() const {
    switch (kind) {
        case IDENTIFIER:        return "PARAM_DECL";
        case ARRAY:             return "ARRAY_PARAM_DECL";
        case SIZED_ARRAY:       return "SIZED_ARRAY_PARAM_DECL";
        case ARRAY_OF_ARRAYS:   return "ARRAY_OF_ARRAYS_PARAM_DECL";
        default:                return "UNKNOWN_PARAM_DECL";
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
    kind = NONE;
    selectorIdentifier = nullptr;
    type = nullptr;
    paramIdentifier = nullptr;
    arraySizeSpec = nullptr;
}

MethodParamNode* MethodParamNode::createMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = IDENTIFIER;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = ARRAY;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createSizedArrayMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = SIZED_ARRAY;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->arraySizeSpec = sizeSpec;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode* MethodParamNode::createSizedArrayOfArraysMethodParam(ValueNode *selectorIdentifier, TypeNode *type, ArraySizeSpecNode *sizeSpec, ValueNode *paramIdentifier) {
    MethodParamNode *node = new MethodParamNode();
    node->kind = ARRAY_OF_ARRAYS;
    node->selectorIdentifier = selectorIdentifier;
    node->type = type;
    node->arraySizeSpec = sizeSpec;
    node->paramIdentifier = paramIdentifier;
    return node;
}

MethodParamNode::MethodParamKind MethodParamNode::getKind() const {
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

string MethodParamNode::getDotLabel() const {
    switch (kind) {
        case IDENTIFIER:        return "METHOD_PARAM";
        case ARRAY:             return "ARRAY_METHOD_PARAM";
        case SIZED_ARRAY:       return "SIZED_ARRAY_METHOD_PARAM";
        case ARRAY_OF_ARRAYS:   return "ARRAY_OF_ARRAYS_METHOD_PARAM";
        default:                return "UNKNOWN_METHOD_PARAM";
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

//--------------------------------------------------------------InstanceMethodDefNode--------------------------------------------------------------

InstanceMethodDefNode::InstanceMethodDefNode() : AstNode() {
    kind = NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
    compoundStmt = nullptr;
}

InstanceMethodDefNode* InstanceMethodDefNode::createInstanceMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt) {
    InstanceMethodDefNode *node = new InstanceMethodDefNode();
    node->kind = TYPE_ID;
    node->type = type;
    node->identifier = identifier;
    node->compoundStmt = compoundStmt;
    return node;
}

InstanceMethodDefNode* InstanceMethodDefNode::createInstanceMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt) {
    InstanceMethodDefNode *node = new InstanceMethodDefNode();
    node->kind = TYPE_SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->compoundStmt = compoundStmt;
    return node;
}

InstanceMethodDefNode::InstanceMethodDefKind InstanceMethodDefNode::getKind() const {
    return kind;
}

TypeNode* InstanceMethodDefNode::getType() const {
    return type;
}

ValueNode* InstanceMethodDefNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* InstanceMethodDefNode::getMethodSel() const {
    return methodSel;
}

StmtNode* InstanceMethodDefNode::getCompoundStmt() const {
    return compoundStmt;
}

string InstanceMethodDefNode::getDotLabel() const {
    switch (kind) {
        case TYPE_ID:   return "INST_METHOD_DEF_TYPE_NO_ARGS";
        case TYPE_SEL:   return "INST_METHOD_DEF_TYPE_HAS_ARGS";
        case VOID_ID:   return "INST_METHOD_DEF_NO_TYPE_NO_ARGS";
        case VOID_SEL:   return "INST_METHOD_DEF_NO_TYPE_HAS_ARGS";
        default:        return "UNKNOWN_METHOD_DEF";
    }
}

string InstanceMethodDefNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "identifier");
    appendDotEdge(result, methodSel, "method_sel");
    appendDotEdge(result, compoundStmt, "compound_stmt");
    return result;
}

//--------------------------------------------------------------ClassMethodDefNode--------------------------------------------------------------

ClassMethodDefNode::ClassMethodDefNode() : AstNode() {
    kind = NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
    compoundStmt = nullptr;
}

ClassMethodDefNode* ClassMethodDefNode::createClassMethodDef(TypeNode *type, ValueNode *identifier, StmtNode *compoundStmt) {
    ClassMethodDefNode *node = new ClassMethodDefNode();
    node->kind = TYPE_ID;
    node->type = type;
    node->identifier = identifier;
    node->compoundStmt = compoundStmt;
    return node;
}

ClassMethodDefNode* ClassMethodDefNode::createClassMethodDef(TypeNode *type, MethodSelNode *methodSel, StmtNode *compoundStmt) {
    ClassMethodDefNode *node = new ClassMethodDefNode();
    node->kind = TYPE_SEL;
    node->type = type;
    node->methodSel = methodSel;
    node->compoundStmt = compoundStmt;
    return node;
}

ClassMethodDefNode::ClassMethodDefKind ClassMethodDefNode::getKind() const {
    return kind;
}

TypeNode* ClassMethodDefNode::getType() const {
    return type;
}

ValueNode* ClassMethodDefNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* ClassMethodDefNode::getMethodSel() const {
    return methodSel;
}

StmtNode* ClassMethodDefNode::getCompoundStmt() const {
    return compoundStmt;
}

string ClassMethodDefNode::getDotLabel() const {
    switch (kind) {
        case TYPE_ID:   return "CLASS_METHOD_DEF_TYPE_NO_ARGS";
        case TYPE_SEL:   return "CLASS_METHOD_DEF_TYPE_HAS_ARGS";
        case VOID_ID:   return "CLASS_METHOD_DEF_NO_TYPE_NO_ARGS";
        case VOID_SEL:   return "CLASS_METHOD_DEF_NO_TYPE_HAS_ARGS";
        default:        return "UNKNOWN_METHOD_DEF";
    }
}

string ClassMethodDefNode::toDot() const {
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

ImplementationDefListNode* ImplementationDefListNode::createImplementationDefList(ClassMethodDefNode *classMethodDef) {
    ImplementationDefListNode *node = new ImplementationDefListNode();
    node->classMethodDefs = new list<ClassMethodDefNode*>{classMethodDef};
    node->instanceMethodDefs = new list<InstanceMethodDefNode*>();
    return node;
}

ImplementationDefListNode* ImplementationDefListNode::createImplementationDefList(InstanceMethodDefNode *instanceMethodDef) {
    ImplementationDefListNode *node = new ImplementationDefListNode();
    node->classMethodDefs = new list<ClassMethodDefNode*>();
    node->instanceMethodDefs = new list<InstanceMethodDefNode*>{instanceMethodDef};
    return node;
}

ImplementationDefListNode* ImplementationDefListNode::addClassMethodDef(ImplementationDefListNode *implementationDefList, ClassMethodDefNode *classMethodDef) {
    if (!implementationDefList->classMethodDefs) {
        implementationDefList->classMethodDefs = new std::list<ClassMethodDefNode*>();
    }
    implementationDefList->classMethodDefs->push_back(classMethodDef);
    return implementationDefList;
}

ImplementationDefListNode* ImplementationDefListNode::addInstanceMethodDef(ImplementationDefListNode *implementationDefList, InstanceMethodDefNode *instanceMethodDef) {
    if (!implementationDefList->instanceMethodDefs) {
        implementationDefList->instanceMethodDefs = new std::list<InstanceMethodDefNode*>();
    }
    implementationDefList->instanceMethodDefs->push_back(instanceMethodDef);
    return implementationDefList;
}

list<ClassMethodDefNode*>* ImplementationDefListNode::getClassMethodDefs() const {
    return classMethodDefs;
}

list<InstanceMethodDefNode*>* ImplementationDefListNode::getInstanceMethodDefs() const {
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
        for (ClassMethodDefNode *method : *classMethodDefs) {
            appendDotEdge(result, method, "class_method_" + to_string(i++));
        }
    }

    if (instanceMethodDefs) {
        int i = 0;
        for (InstanceMethodDefNode *method : *instanceMethodDefs) {
            appendDotEdge(result, method, "instance_method_" + to_string(i++));
        }
    }

    return result;
}

//--------------------------------------------------------------InstanceMethodDeclNode--------------------------------------------------------------

InstanceMethodDeclNode::InstanceMethodDeclNode() : AstNode() {
    kind = NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
}

InstanceMethodDeclNode* InstanceMethodDeclNode::createInstanceMethodDecl(TypeNode *type, ValueNode *identifier) {
    InstanceMethodDeclNode *node = new InstanceMethodDeclNode();
    node->kind = TYPE_ID;
    node->type = type;
    node->identifier = identifier;
    return node;
}

InstanceMethodDeclNode* InstanceMethodDeclNode::createInstanceMethodDecl(TypeNode *type, MethodSelNode *methodSel) {
    InstanceMethodDeclNode *node = new InstanceMethodDeclNode();
    node->kind = TYPE_SEL;
    node->type = type;
    node->methodSel = methodSel;
    return node;
}

InstanceMethodDeclNode::InstanceMethodDeclKind InstanceMethodDeclNode::getKind() const {
    return kind;
}

TypeNode* InstanceMethodDeclNode::getType() const {
    return type;
}

ValueNode* InstanceMethodDeclNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* InstanceMethodDeclNode::getMethodSel() const {
    return methodSel;
}

string InstanceMethodDeclNode::getDotLabel() const {
    switch (kind) {
        case TYPE_ID:   return "INST_METHOD_DECL_TYPE_NO_ARGS";
        case TYPE_SEL:   return "INST_METHOD_DECL_TYPE_HAS_ARGS";
        case VOID_ID:   return "INST_METHOD_DECL_NO_TYPE_NO_ARGS";
        case VOID_SEL:   return "INST_METHOD_DECL_NO_TYPE_HAS_ARGS";
        default:        return "UNKNOWN_METHOD_DECL";
    }
}

string InstanceMethodDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "identifier");
    appendDotEdge(result, methodSel, "method_sel");
    return result;
}

//--------------------------------------------------------------ClassMethodDeclNode--------------------------------------------------------------

ClassMethodDeclNode::ClassMethodDeclNode() : AstNode() {
    kind = NONE;
    type = nullptr;
    identifier = nullptr;
    methodSel = nullptr;
}

ClassMethodDeclNode* ClassMethodDeclNode::createClassMethodDecl(TypeNode *type, ValueNode *identifier) {
    ClassMethodDeclNode *node = new ClassMethodDeclNode();
    node->kind = TYPE_ID;
    node->type = type;
    node->identifier = identifier;
    return node;
}

ClassMethodDeclNode* ClassMethodDeclNode::createClassMethodDecl(TypeNode *type, MethodSelNode *methodSel) {
    ClassMethodDeclNode *node = new ClassMethodDeclNode();
    node->kind = TYPE_SEL;
    node->type = type;
    node->methodSel = methodSel;
    return node;
}

ClassMethodDeclNode::ClassMethodDeclKind ClassMethodDeclNode::getKind() const {
    return kind;
}

TypeNode* ClassMethodDeclNode::getType() const {
    return type;
}

ValueNode* ClassMethodDeclNode::getIdentifier() const {
    return identifier;
}

MethodSelNode* ClassMethodDeclNode::getMethodSel() const {
    return methodSel;
}

string ClassMethodDeclNode::getDotLabel() const {
    switch (kind) {
        case TYPE_ID:   return "CLASS_METHOD_DECL_TYPE_NO_ARGS";
        case TYPE_SEL:   return "CLASS_METHOD_DECL_TYPE_HAS_ARGS";
        case VOID_ID:   return "CLASS_METHOD_DECL_NO_TYPE_NO_ARGS";
        case VOID_SEL:   return "CLASS_METHOD_DECL_NO_TYPE_HAS_ARGS";
        default:        return "UNKNOWN_METHOD_DECL";
    }
}

string ClassMethodDeclNode::toDot() const {
    string result;
    appendDotNode(result);
    appendDotEdge(result, type, "type");
    appendDotEdge(result, identifier, "identifier");
    appendDotEdge(result, methodSel, "method_sel");
    return result;
}

//--------------------------------------------------------------PropertyNode--------------------------------------------------------------

PropertyNode::PropertyNode() : AstNode() {
    attribute = NONE;
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

PropertyNode::Attribute PropertyNode::getAttribute() const {
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
        case READONLY: return "READONLY_PROPERTY";
        case READWRITE: return "READWRITE_PROPERTY";
        default: return "UNKNOWN_PROPERTY";
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
    node->classMethodDecls = new list<ClassMethodDeclNode*>();
    node->instanceMethodDecls = new list<InstanceMethodDeclNode*>();
    return node;
}

InterfaceDeclListNode* InterfaceDeclListNode::addProperty(InterfaceDeclListNode *interfaceDeclList, PropertyNode *property) {
    if (!interfaceDeclList->properties) {
        interfaceDeclList->properties = new std::list<PropertyNode*>();
    }
    interfaceDeclList->properties->push_back(property);
    return interfaceDeclList;
}

InterfaceDeclListNode* InterfaceDeclListNode::addClassMethodDecl(InterfaceDeclListNode *interfaceDeclList, ClassMethodDeclNode *classMethodDecl) {
    if (!interfaceDeclList->classMethodDecls) {
        interfaceDeclList->classMethodDecls = new std::list<ClassMethodDeclNode*>();
    }
    interfaceDeclList->classMethodDecls->push_back(classMethodDecl);
    return interfaceDeclList;
}

InterfaceDeclListNode* InterfaceDeclListNode::addInstanceMethodDecl(InterfaceDeclListNode *interfaceDeclList, InstanceMethodDeclNode *instanceMethodDecl) {
    if (!interfaceDeclList->instanceMethodDecls) {
        interfaceDeclList->instanceMethodDecls = new std::list<InstanceMethodDeclNode*>();
    }
    interfaceDeclList->instanceMethodDecls->push_back(instanceMethodDecl);
    return interfaceDeclList;
}

list<PropertyNode*>* InterfaceDeclListNode::getProperties() const {
    return properties;
}

list<ClassMethodDeclNode*>* InterfaceDeclListNode::getClassMethodDecls() const {
    return classMethodDecls;
}

list<InstanceMethodDeclNode*>* InterfaceDeclListNode::getInstanceMethodDecls() const {
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
        for (ClassMethodDeclNode *method : *classMethodDecls) {
            appendDotEdge(result, method, "class_method_" + to_string(i++));
        }
    }

    if (instanceMethodDecls) {
        int i = 0;
        for (InstanceMethodDeclNode *method : *instanceMethodDecls) {
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
    type = NONE;
    expr = nullptr;
    initList = nullptr;
}

InitializerNode* InitializerNode::createExpr(ExprNode *expr) {
    InitializerNode *node = new InitializerNode();
    node->type = EXPR;
    node->expr = expr;
    return node;
}

InitializerNode* InitializerNode::createArrayInitializer(InitializerListNode *initList) {
    InitializerNode *node = new InitializerNode();
    node->type = ARRAY;
    node->initList = initList;
    return node;
}

InitializerNode::InitializerType InitializerNode::getType() const {
    return type;
}

ExprNode* InitializerNode::getExpr() const {
    return expr;
}

InitializerListNode* InitializerNode::getInitializerList() const {
    return initList;
}

string InitializerNode::getDotLabel() const {
    switch(type) {
        case EXPR:  return "EXPR_INITIALIZER";
        case ARRAY: return "ARRAY_INITIALIZER";
        default:    return "UNKNOWN_INITIALIZER";
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
    type = NONE;
    declarator = nullptr;
    initializer = nullptr;
}

InitDeclNode* InitDeclNode::createDeclarator(DeclaratorNode *declarator) {
    InitDeclNode *node = new InitDeclNode();
    node->type = DECLARATOR;
    node->declarator = declarator;
    return node;
}

InitDeclNode* InitDeclNode::createInitialized(DeclaratorNode *declarator, InitializerNode *initializer) {
    InitDeclNode *node = new InitDeclNode();
    node->type = INITIALIZED;
    node->declarator = declarator;
    node->initializer = initializer;
    return node;
}

InitDeclNode* InitDeclNode::createArrayInitialized(DeclaratorNode *declarator, InitializerNode *initializer) {
    InitDeclNode *node = new InitDeclNode();
    node->type = ARRAY_INITIALIZED;
    node->declarator = declarator;
    node->initializer = initializer;
    return node;
}

InitDeclNode::InitDeclType InitDeclNode::getType() const {
    return type;
}

DeclaratorNode* InitDeclNode::getDeclarator() const {
    return declarator;
}

InitializerNode* InitDeclNode::getInitializer() const {
    return initializer;
}

string InitDeclNode::getDotLabel() const {
    switch (type) {
        case DECLARATOR: return "DECLARATOR_ONLY";
        case INITIALIZED: return "INITIALIZED_DECL";
        case ARRAY_INITIALIZED: return "ARRAY_INITIALIZED_DECL";
        default: return "UNKNOWN_INIT_DECL";
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
    accessType = NONE;
}

AccessModifierNode* AccessModifierNode::createPublic() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = PUBLIC;
    return node;
}

AccessModifierNode* AccessModifierNode::createProtected() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = PROTECTED;
    return node;
}

AccessModifierNode* AccessModifierNode::createPrivate() {
    AccessModifierNode *node = new AccessModifierNode();
    node->accessType = PRIVATE;
    return node;
}

AccessModifierNode::AccessModifier AccessModifierNode::getAccessType() const {
    return accessType;
}

string AccessModifierNode::getDotLabel() const {
    switch (accessType) {
        case PUBLIC: return "PUBLIC";
        case PROTECTED: return "PROTECTED";
        case PRIVATE: return "PRIVATE";
        default: return "UNKNOWN_ACCESS_MODIFIER";
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
    type = NONE;
    interface = nullptr;
    implementation = nullptr;
    classNames = nullptr;
    funcDecl = nullptr;
    funcDef = nullptr;
}

ExternalDeclNode* ExternalDeclNode::createInterface(InterfaceNode *interface) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->type = INTERFACE;
    node->interface = interface;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createImplementation(ImplementationNode *implementation) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->type = IMPLEMENTATION;
    node->implementation = implementation;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFwClassDeclList(ClassNameListNode *classNames) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->type = CLASS_FW_DECL_LIST;
    node->classNames = classNames;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFuncDecl(FuncDeclNode *funcDecl) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->type = FUNC_DECL;
    node->funcDecl = funcDecl;
    return node;
}

ExternalDeclNode* ExternalDeclNode::createFuncDef(FuncDefNode *funcDef) {
    ExternalDeclNode *node = new ExternalDeclNode();
    node->type = FUNC_DEF;
    node->funcDef = funcDef;
    return node;
}

ExternalDeclNode::ExternalDeclType ExternalDeclNode::getType() const {
    return type;
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
    switch (type) {
        case INTERFACE:             return "INTERFACE";
        case IMPLEMENTATION:        return "IMPLEMENTATION";
        case CLASS_FW_DECL_LIST:    return "CLASS_FW_DECL_LIST";
        case FUNC_DECL:             return "FUNC_DECL";
        case FUNC_DEF:              return "FUNC_DEF";
        default:                    return "UNKNOWN_EXTERNAL_DECL";
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
