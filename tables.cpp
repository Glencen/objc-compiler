#include <fstream>
#include <algorithm>
#include "tables.h"

//--------------------------------------------------------------Type--------------------------------------------------------------

Type::Type(TypeKind dataType, string className, ExprNode* arrSize) {
    if (arrSize != NULL) {
        if (arrSize->getType() == ExprNode::UNARY_MINUS) {
            ExprNode* operand = arrSize->getOperand();
            if (operand->getType() == ExprNode::LITERAL) {
                ValueNode* value = operand->getLiteral();
                if (value->getValueType() == ValueNode::INT_LIT) {
                    int intValue = value->getInt();
                    if (intValue > 0) {
                        string msg = "Negative array size '-" + to_string(intValue) + "'";
                        throw std::invalid_argument(msg);
                    }
                }
            }
        }
    }
    this->dataType = dataType;
    this->className = className;
    this->arrSize = arrSize;
}

Type::Type(TypeKind dataType, string className) {
    this->dataType = dataType;
	this->className = className;
}

Type::Type(TypeKind dataType) {
    this->dataType = dataType;
}

Type::Type(TypeKind dataType, ExprNode* arrSize) {
    if (arrSize != NULL) {
        if (arrSize->getType() == ExprNode::UNARY_MINUS) {
            ExprNode* operand = arrSize->getOperand();
            if (operand->getType() == ExprNode::LITERAL) {
                ValueNode* value = operand->getLiteral();
                if (value->getValueType() == ValueNode::INT_LIT) {
                    int intValue = value->getInt();
                    if (intValue > 0) {
                        string msg = "Negative array size '-" + to_string(intValue) + "'";
                        throw std::invalid_argument(msg);
                    }
                }
            }
        }
    }
    this->dataType = dataType;
    this->arrSize = arrSize;
}

Type::Type(TypeKind dataType, string className, int arrSize) {
    this->dataType = dataType;
    this->className = className;
    this->arrSize = ExprNode::createLiteral(ValueNode::createInt(arrSize));
}

Type::Type(TypeKind dataType, int arrSize) {
    this->dataType = dataType;
    this->arrSize = ExprNode::createLiteral(ValueNode::createInt(arrSize));
}

string Type::toString() {
    string res = "";
    switch (dataType) {
    case TypeKind::INT:
        res += string("int");
        break;
    case TypeKind::CHAR:
        res += string("char");
        break;
    case TypeKind::FLOAT:
        res += string("float");
        break;
    case TypeKind::TYPE_ID:
        res += string("id");
        break;
    case TypeKind::CLASS_NAME:
        res += className;
        break;
    case TypeKind::VOID:
        res += string("void");
        break;
    default:
        break;
    }

    if (arrSize != NULL) {
        if (arrSize->getType() == ExprNode::LITERAL) {
            ValueNode* value = arrSize->getLiteral();
            if (value->getValueType() == ValueNode::INT_LIT) {
                res += '[' + to_string(value->getInt()) + ']';
            }
            else {
                res += "[ literal ]";
            }
        }
        else {
            res += string("[ expr(") + to_string(arrSize->getId()) + ") ]";
        }
    }
    return res;
}

string Type::getDescriptor() {
    string res = "";
    if (arrSize != NULL) {
        res += '[';
    }
    switch (dataType) {
    case TypeKind::INT:
        res += string("I");
        break;
    case TypeKind::CHAR:
        res += string("C");
        break;
    case TypeKind::FLOAT:
        res += string("F");
        break;
    case TypeKind::TYPE_ID:
        res += string("Lrtl/NSObject;");
        break;
    case TypeKind::CLASS_NAME:
        res += 'L' + className + ';';
        break;
    case TypeKind::VOID:
        res += string("V");
        break;
    default:
        break;
    }
    return res;
}

bool Type::equal(Type* other) {
    bool isBouthArray = (arrSize != NULL && other->arrSize != NULL) || (arrSize == NULL && other->arrSize == NULL);
	return dataType == other->dataType && className == other->className && isBouthArray;
}

int Type::getDefaultValue() {
    if (dataType == TypeKind::CLASS_NAME) {
        return NULL;
    }
    return 0;
}

bool Type::isCastableTo(Type* other) {
    if (arrSize != NULL || other->arrSize != NULL) {
        return false;
    }
    if (this->dataType == other->dataType) {
        if (this->dataType == TypeKind::CLASS_NAME && this->className == other->className) {
            return true;
        }
        else {
            return true;
        }
    }
    if (this->dataType == TypeKind::INT && other->dataType == TypeKind::CHAR) {
        return true;
    }
    if (this->dataType == TypeKind::CHAR && other->dataType == TypeKind::INT) {
        return true;
    }
    if (this->dataType == TypeKind::CLASS_NAME && other->dataType == TypeKind::TYPE_ID) {
        return true;
    }
    if (this->dataType == TypeKind::TYPE_ID && other->dataType == TypeKind::CLASS_NAME) {
        return true;
    }
    if (this->dataType == TypeKind::CLASS_NAME && other->dataType == TypeKind::CLASS_NAME) {
        ClassesTableElement* thisClass = ClassesTable::items[this->className];
        ClassesTableElement* otherClass = ClassesTable::items[other->className];
        if (thisClass->isHaveOneOfSuperclass(other->className)) {
            return true;
        }
        if (otherClass->isHaveOneOfSuperclass(this->className)) {
            return true;
        }
    }
    return false;
}

bool Type::isPrimitive() {
    return dataType == TypeKind::INT || dataType == TypeKind::CHAR || dataType == TypeKind::VOID;
}

Type* Type::getSuperType() {
    if (dataType != TypeKind::CLASS_NAME) {
        throw new std::exception("Type is not a class");
    }
    ClassesTableElement* thisClass = ClassesTable::items[className];
    string superClassName = thisClass->getSuperClassName();
    if (superClassName == "") {
        return NULL;
    }
    return new Type(TypeKind::CLASS_NAME, superClassName);
}


//--------------------------------------------------------------ConstantsTableElement--------------------------------------------------------------

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, string utf8String) {
    this->id = id;
    this->type = type;
    this->utf8String = &utf8String;
}

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, float floatNumber) {
    this->id = id;
    this->type = type;
    this->floatNumber = floatNumber;
}

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, int number, int firstRef, int secondRef) {
    this->id = id;
    this->type = type;
    this->number = number;
    this->firstRef = firstRef;
    this->secondRef = secondRef;
}

string ConstantsTableElement::toCSVString(char separator) {
	string res = "";
	res += to_string(id) + separator;

	switch (type) {
        case ConstantType::UTF8:
            res += string("UTF8") + separator;
            res += *utf8String;
            break;
        case ConstantType::INTEGER:
            res += string("Integer") + separator;
            res += to_string(number);
            break;
        case ConstantType::FLOAT:
            res += string("Float") + separator;
            res += to_string(floatNumber);
            break;
        case ConstantType::STRING:
            res += string("String") + separator;
            res += to_string(firstRef);
            break;
        case ConstantType::CLASS:
            res += string("Class") + separator;
            res += to_string(firstRef);
            break;
        case ConstantType::NAME_AND_TYPE:
            res += string("NameAndType") + separator;
            res += to_string(firstRef) + ", ";
            res += to_string(secondRef);
            break;
        case ConstantType::FIELD_REF:
            res += string("FieldRef") + separator;
            res += to_string(firstRef) + ", ";
            res += to_string(secondRef);
            break;
        case ConstantType::METHOD_REF:
            res += string("MethodRef") + separator;
            res += to_string(firstRef) + ", ";
            res += to_string(secondRef);
            break;
        default:
            break;
	}

	return res;
}

//--------------------------------------------------------------ConstantsTable--------------------------------------------------------------

ConstantsTable::ConstantsTable() {
    items[maxId] = new ConstantsTableElement(maxId, UTF8, "Code");
    maxId++;
}

int ConstantsTable::findOrAddConstant(ConstantType type, string utf8String) {
    int res = findConstant(type, &utf8String, NULL);
    if (res == -1) {
        res == maxId++;
        items[res] = new ConstantsTableElement(res, type, utf8String);
    }
    return res;
}

int ConstantsTable::findOrAddConstant(ConstantType type, float floatNumber) {
    int res = findConstant(type, NULL, floatNumber);
    if (res == -1) {
        res == maxId++;
        items[res] = new ConstantsTableElement(res, type, floatNumber);
    }
    return res;
}

int ConstantsTable::findOrAddConstant(ConstantType type, int number, int firstRef, int secondRef) {
    int res = findConstant(type, NULL, NULL, number, firstRef, secondRef);
    if (res == -1) {
        res = maxId++;
        items[res] = new ConstantsTableElement(res, type, number, firstRef, secondRef);
    }
    return res;
}

int ConstantsTable::findConstant(ConstantType type, string *utf8string, float floatNumber, int number, int firstRef, int secondRef) {
    string compared = utf8string == NULL ? "" : *utf8string;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string curStr = iter->second->utf8String == NULL ? "" : *iter->second->utf8String;
        if (iter->second->type == type && curStr == compared && iter->second->number == number && iter->second->firstRef == firstRef && iter->second->secondRef == secondRef) {
            return iter->first;
        }
        ++iter;
    }
    return -1;
}

ConstantsTableElement* ConstantsTable::getConstant(int id) {
    return items[id];
}

string ConstantsTable::getConstantString(int id) {
    if (items[id]->type != ConstantType::UTF8) {
        return "";
    }
    return *items[id]->utf8String;
}

void ConstantsTable::toCSVFile(string filename, string filepath, char separator) {
    ofstream out(filepath + filename);
    out << "ID" << separator << "Type" << separator << "Value" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string str = iter->second->toCSVString(separator);
        out << str << endl;
        ++iter;
    }
    out.close();
}

int ConstantsTable::findOrAddFieldRefConstant(string className, string fieldName, string descriptor) {
    int classNameConst = this->findOrAddConstant(UTF8, className);
    int classConst = this->findOrAddConstant(CLASS, NULL, classNameConst);
    int nameConst = this->findOrAddConstant(UTF8, fieldName);
    int descriptorConst = this->findOrAddConstant(UTF8, descriptor);
    int nameAndTypeConst = this->findOrAddConstant(NAME_AND_TYPE, NULL, nameConst, descriptorConst);
    int fieldRefConst = this->findOrAddConstant(FIELD_REF, NULL, nameAndTypeConst, classConst);
    return fieldRefConst;
}

int ConstantsTable::findOrAddMethodRefConstant(string className, string methodName, string descriptor) {
    int classNameConst = this->findOrAddConstant(UTF8, className);
    int classConst = this->findOrAddConstant(CLASS, NULL, classNameConst);
    int nameConst = this->findOrAddConstant(UTF8, methodName);
    int descriptorConst = this->findOrAddConstant(UTF8, descriptor);
    int nameAndTypeConst = this->findOrAddConstant(NAME_AND_TYPE, NULL, nameConst, descriptorConst);
    int methodRefConst = this->findOrAddConstant(METHOD_REF, NULL, nameAndTypeConst, classConst);
    return methodRefConst;
}

//--------------------------------------------------------------FunctionsTableElement--------------------------------------------------------------

FunctionsTableElement::FunctionsTableElement(StmtNode *bodyStart, string nameStr, string descriptorStr, vector<Type*> *params, Type *returnType) {
    this->bodyStart = bodyStart;
    localVariables = new LocalVariablesTable();
    this->nameStr = nameStr;
    this->descriptorStr = descriptorStr;
    parametersTypes = params;
    this->returnType = returnType;
}

string FunctionsTableElement::toCSVString(string funcName, char separator) {
    string res = "";
    res +=  nameStr + separator;
    res += descriptorStr + separator;
    res += to_string(bodyStart->getId()) + separator;
    if (localVariables->items.size() > 0)
        res += funcName + "_LocalVariablesTable.csv";
    else
        res += string("empty");
    return res;
}

void FunctionsTableElement::refTablesToCSVFile(string filename, string filepath, char separator) {
    if (localVariables->items.size() > 0) {
        localVariables->toCSVFile(filename, filepath, separator);
    }
}

void FunctionsTableElement::fillFieldRefs(ConstantsTable *constantTable, ClassesTableElement *classTableElement) {
    if (bodyStart) {
        bodyStart->fillFieldRefs(constantTable, localVariables, classTableElement);
    }
}

void FunctionsTableElement::fillMethodRefs(ConstantsTable *constantTable, ClassesTableElement *classTableElement) {
    if (bodyStart) {
        bodyStart->fillMethodRefs(constantTable, localVariables, classTableElement, false);
    }
}

void FunctionsTableElement::fillLiterals(ConstantsTable *constantTable) {
    if (bodyStart) {
        bodyStart->fillLiterals(constantTable);
    }
}

void FunctionsTableElement::convertToClassProgramMethods(ClassesTableElement *classTableElement) {
    MethodsTableElement* method = classTableElement->methods->addMethod(
        classTableElement->constantTable, nameStr, descriptorStr, true, bodyStart, returnType, new vector<Type*>, parametersTypes);
    method->localVariables = localVariables;
}

void FunctionsTableElement::semanticTransform() {
    if (bodyStart != nullptr) {
        bodyStart->semanticTransform(localVariables);
        if (returnType->dataType != TypeKind::VOID) {
            addDefaultReturn(bodyStart);
        }
    }
}

void FunctionsTableElement::addDefaultReturn(StmtNode *lastStatement) {
    if (lastStatement == nullptr) return;

    StmtListNode* stmtList = nullptr;
    
    if (bodyStart->getType() == StmtNode::COMPOUND) {
        stmtList = bodyStart->getCompound();
    } else {
        stmtList = StmtListNode::createStmtList(bodyStart);
    }

    StmtNode* defaultReturn = nullptr;
    
    if (returnType->dataType == TypeKind::VOID) {
        defaultReturn = StmtNode::createReturn(nullptr);
    } else {
        ExprNode* defaultValue = nullptr;
        
        switch (returnType->dataType) {
            case TypeKind::INT:
                defaultValue = ExprNode::createLiteral(ValueNode::createInt(0));
                break;
            case TypeKind::FLOAT:
                defaultValue = ExprNode::createLiteral(ValueNode::createFloat(0.0f));
                break;
            case TypeKind::CHAR:
                defaultValue = ExprNode::createLiteral(ValueNode::createChar('\0'));
                break;
            case TypeKind::BOOL:
                defaultValue = ExprNode::createLiteral(ValueNode::createBool(false));
                break;
            case TypeKind::CLASS_NAME:
            case TypeKind::TYPE_ID:
                defaultValue = ExprNode::createNil();
                break;
            default:
                defaultValue = ExprNode::createNil();
                break;
        }
        
        defaultReturn = StmtNode::createReturn(defaultValue);
    }
    
    if (stmtList != nullptr) {
        stmtList = StmtListNode::addStmtToList(stmtList, defaultReturn);
        StmtNode* newBody = StmtNode::createCompound(stmtList);
        bodyStart = newBody;
    }
}

//--------------------------------------------------------------FunctionsTable--------------------------------------------------------------

FunctionsTableElement* FunctionsTable::addFunction(string name, string descriptor, StmtNode *bodyStart, vector<Type*> *params, Type *returnType) {
    if (items.count(name) != 0) {
        string msg = "Function '" + name + "' already exists";
        throw new exception(msg.c_str());
    }
    FunctionsTableElement *function = new FunctionsTableElement(bodyStart, name, descriptor, params, returnType);
    items[name] = function;
    return function;
}

void FunctionsTable::toCSVFile(string filename, string filepath, char separator) {
    ofstream out(filepath + filename);
    out << "Name" << separator << "Descriptor" << separator << "BodyStartStatementId" << separator << "LocalVariablesTableName" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string funcName = filename.substr(0, filename.find(".")) + string("_") + iter->first + "_LocalVariables.csv";
        string str = iter->second->toCSVString(funcName, separator);
        out << str << endl;
        iter->second->refTablesToCSVFile(funcName, filepath, separator);
        ++iter;
    }
    out.close();
}

void FunctionsTable::fillFieldRefs() {
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    bool isDontContainsMain = true;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        isDontContainsMain = isDontContainsMain && iter->first == "main";
        iter->second->fillFieldRefs(classTableElement->constantTable, classTableElement);
        ++iter;
    }

    if (!isDontContainsMain) {
        string msg = "Function 'main' not found";
        throw new exception(msg.c_str());
    }
}

void FunctionsTable::fillMethodRefs() {
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillMethodRefs(classTableElement->constantTable, classTableElement);
        ++iter;
    }
}

void FunctionsTable::fillLiterals() {
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillLiterals(classTableElement->constantTable);
        ++iter;
    }
}

void FunctionsTable::convertToClassProgramMethods() {
    if (items.count("main") == 0) {
        string msg = "Function 'main' not found";
        throw new exception(msg.c_str());
    }

    ClassesTableElement* classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->convertToClassProgramMethods(classTableElement);
        ++iter;
    }
}

void FunctionsTable::semanticTransform() {
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->semanticTransform();
        ++iter;
    }
}

//--------------------------------------------------------------ClassesTableElement--------------------------------------------------------------

ClassesTableElement::ClassesTableElement(string name, string *superclassName, bool isImplementation) {
    constantTable = new ConstantsTable();
    fields = new FieldsTable();
    methods = new MethodsTable();
    properties = new PropertiesTable();
    this->name = constantTable->findOrAddConstant(UTF8, name);
    if (superclassName != NULL) {
        this->superclassName = constantTable->findOrAddConstant(UTF8, *superclassName);
    }
    thisClass = constantTable->findOrAddConstant(CLASS, NULL, this->name);
    if (superclassName != NULL) {
        this->superclass = constantTable->findOrAddConstant(CLASS, NULL, this->superclassName);
    }
    isImplementation = isImplementation;
}

string ClassesTableElement::toCSVString(char separator) {
    string res = "";
    res += to_string(name) + '(' + *constantTable->getConstant(name)->utf8String + ')' + separator;
    if (superclassName != NULL) {
        res += to_string(superclassName) + '(' + *constantTable->getConstant(superclassName)->utf8String + ')' + separator;
    }
    else {
        res += string("empty") + separator;
    }
    res += string((isImplementation ? "true" : "false")) + separator;
    res += to_string(thisClass) + separator;
    res += to_string(superclass) + separator;

    if (fields->items.size() > 0) {
        res += *constantTable->getConstant(name)->utf8String + "_FieldsTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }

    if (methods->items.size() >0 ) {
        res += *constantTable->getConstant(name)->utf8String + "_MethodsTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }

    if (properties->items.size() > 0) {
        res += *constantTable->getConstant(name)->utf8String + "_PropertiesTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }	
    res += *constantTable->getConstant(name)->utf8String + "_ConstantsTable.csv";
    return res;
}

void ClassesTableElement::refTablesToCSVFile(string filepath, char separator) {
    string className = *constantTable->getConstant(name)->utf8String;
    replace(className.begin(), className.end(), '/', '_');
    if (fields->items.size() > 0) {
        fields->toCSVFile(className + "_FieldsTable.csv", filepath, separator);
    }
    if (methods->items.size() > 0) {
        methods->toCSVFile(className + "_MethodsTable.csv", filepath, separator);
    }
    if (properties->items.size() > 0) {
        properties->toCSVFile(className + "_PropertiesTable.csv", filepath, separator);
    }
    constantTable->toCSVFile(className + "_ConstantsTable.csv", filepath, separator);
}

string ClassesTableElement::getClassName() {
    return constantTable->getConstantString(name);
}

string ClassesTableElement::getSuperClassName() {
    return constantTable->getConstantString(superclassName);
}

void ClassesTableElement::fillFieldRefs() {
    for (auto iter = methods->items.cbegin(); iter != methods->items.cend(); ++iter) {
        iter->second->fillFieldRefs(constantTable, this);
    }
}

void ClassesTableElement::fillMethodRefs() {
    for (auto iter = methods->items.cbegin(); iter != methods->items.cend(); ++iter) {
        iter->second->fillMethodRefs(constantTable, this);
    }
}

void ClassesTableElement::fillLiterals() {
    for (auto iter = methods->items.cbegin(); iter != methods->items.cend(); ++iter) {
        iter->second->fillLiterals(constantTable);
    }
    for (auto iter = fields->items.cbegin(); iter != fields->items.cend(); ++iter) {
        iter->second->fillLiterals(constantTable);
    }
}

bool ClassesTableElement::isContainsField(string fieldName) {
    if (fields->items.count(fieldName) != 0) {
        return true;
    }
    else {
        if (superclassName != NULL) {
            return ClassesTable::items[getSuperClassName()]->isContainsField(fieldName);
        }
    }
    return false;
}

FieldsTableElement* ClassesTableElement::getFieldForRef(string name, string *descriptor, string *className) {
    if (isContainsField(name)){
        if (fields->items.count(name) != 0) {
            *descriptor = fields->items[name]->descriptorStr;
            *className = getClassName();
            return fields->items[name];
        }
        else {
            if (superclassName != NULL) {
                return ClassesTable::items[getSuperClassName()]->getFieldForRef(name, descriptor, className);
            }
        }
    }
    return NULL;
}

bool ClassesTableElement::isHaveOneOfSuperclass(string name) {
    if (superclassName == NULL) {
        return false;
    }
    else {
        if (getSuperClassName() == name) {
            return true;
        }
        else {
            return ClassesTable::items[getSuperClassName()]->isHaveOneOfSuperclass(name);
        }
    }
}

bool ClassesTableElement::isContainsMethod(string methodName) {
    if (methods->items.count(methodName) != 0) {
        return true;
    }
    else {
        if (superclassName != NULL) {
            return ClassesTable::items[getSuperClassName()]->isContainsMethod(methodName);
        }
    }
    return false;
}

MethodsTableElement* ClassesTableElement::getMethodForRef(string name, string *descriptor, string *className) {
    if (isContainsMethod(name)) {
        if (methods->items.count(name) != 0) {
            *descriptor = methods->items[name]->descriptorStr;
            *className = getClassName();
            return methods->items[name];
        }
        else {
            if (superclassName != NULL) {
                return ClassesTable::items[getSuperClassName()]->getMethodForRef(name, descriptor, className);
            }
        }
    }
    return NULL;
}

void ClassesTableElement::semanticTransform() {
    for (auto iter = methods->items.cbegin(); iter != methods->items.cend(); ++iter) {
        iter->second->semanticTransform();
    }
}

//--------------------------------------------------------------ClassesTable--------------------------------------------------------------

ClassesTableElement* ClassesTable::addClass(string name, string *superclassName, bool isImplementation, AstNode *classBlock) {
    string fullName = "global/" + name;
    string* fullSuperclassName = NULL;
    if (superclassName != NULL) {
        if (*superclassName == "NSObject" || *superclassName == "NSString" || *superclassName == "NSArray") {
            fullSuperclassName = new string("rtl/" + *superclassName);
        }
        else {
            fullSuperclassName = new string("global/" + *superclassName);
        }
    }

    ClassesTableElement *element = new ClassesTableElement("global/" + name, fullSuperclassName, isImplementation);

    if (!isImplementation && items.count(fullName) && items[fullName]->isImplementation) {
        string msg = "Class interface'" + name + "' after implementation";
        throw new std::exception(msg.c_str());
    }
    else if (items.count(fullName) && items[fullName]->isImplementation == isImplementation) {
        string msg = "Rediifnition of class '" + name + "'";
        throw new std::exception(msg.c_str());
    }
    else if (superclassName != NULL && items.count(fullName) && items[fullName]->constantTable->getConstantString(items[fullName]->superclassName) != *fullSuperclassName) {
        string msg = "Class '" + name + "' with different superclass";
        throw new std::exception(msg.c_str());
    }
    else if (items.count(fullName) && !items[fullName]->isImplementation && isImplementation) {
        items[fullName]->isImplementation = true;
        items[fullName]->isHaveInterface = true;
        delete element;
    }
    else {
        items[fullName] = element;
    }

    if (isImplementation) {
        ImplementationNode* implementation = (ImplementationNode*)classBlock;
        implementation->setClassName(fullName);
        if (fullSuperclassName != NULL) {
            implementation->setSuperClassName(*fullSuperclassName);
        }
        else {
            implementation->setSuperClassName(NULL);
        }
    }
    else {
        InterfaceNode* interface = (InterfaceNode*)classBlock;
        interface->setClassName(fullName);
        if (fullSuperclassName != NULL) {
            interface->setSuperClassName(*fullSuperclassName);
        }
        else {
            interface->setSuperClassName(NULL);
        }
    }
    return items[fullName];
}

void ClassesTable::initRTL() {
    initClassProgram();
    initClassInOutFuncs();
    initClassNSObject();
    initClassNSString();
    initClassNSArray();
}

void ClassesTable::toCSVFile(string filepath, char separator) {
    ofstream out(filepath + "ClassesTable.csv");
    out << "Name" << separator << "SuperclassName" << separator << "IsImplementation" << separator << "ThisClass" << separator << "Superclass" << separator << "FieldsTableName" << separator << "MethodsTableName" << separator << "PropertiesTableName" << separator << "ConstantsTableName" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string str = iter->second->toCSVString(separator);
        out << str << endl;
        iter->second->refTablesToCSVFile(filepath, separator);
        ++iter;
    }
    out.close();
}

void ClassesTable::fillFieldRefs() {
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillFieldRefs();
        ++iter;
    }
}

void ClassesTable::fillMethodRefs() {
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillMethodRefs();
        ++iter;
    }
}

void ClassesTable::fillLiterals() {
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillLiterals();
        ++iter;
    }
}

string ClassesTable::getFullClassName(string name) {
    if (name.find("global/") != string::npos) {
        return name;
    }
    if (name.find("rtl/") != string::npos) {
        return name;
    }
    string fullName;
    if (name == "NSString" || name == "NSArray" || name == "NSObject" || name == "InOutFuncs") {
        fullName = "rtl/" + name;
    }
    else {
        fullName = "global/" + name;
    }
    if (items.count(fullName) == 0) {
        string msg = "Class '" + name + "' not found";
        throw new std::exception(msg.c_str());
    }
    return fullName;
}

void ClassesTable::semanticTransform() {
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->semanticTransform();
        ++iter;
    }
}

void ClassesTable::initClassProgram() {
    ClassesTableElement* Program = new ClassesTableElement("rtl/Program", NULL, true);
    items["rtl/Program"] = Program;
}

void ClassesTable::initClassInOutFuncs() {
    ClassesTableElement* inOutFuncs = new ClassesTableElement("rtl/InOutFuncs", NULL, true);

    ConstantsTable* сonstantTable = inOutFuncs->constantTable;
    Type *printIntReturnType = new Type(TypeKind::VOID);
    vector<Type*> *printIntKeywordsType = new vector<Type*>{ new Type(TypeKind::INT) };
    vector<Type*> *printIntParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "printInt", "(I)V", true, NULL, printIntReturnType, printIntParamsType, printIntKeywordsType);

    Type* printCharReturnType = new Type(TypeKind::VOID);
    vector<Type*> *printCharKeywordsType = new vector<Type*>{ new Type(TypeKind::CHAR) };
    vector<Type*> *printCharParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "printChar", "(C)V", true, NULL, printCharReturnType, printCharParamsType, printCharKeywordsType);

    Type* printStringReturnType = new Type(TypeKind::VOID);
    vector<Type*> *printStringKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "java/lang/String")};
    vector<Type*> *printStringParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "printString", "(Ljava/lang/String;)V", true, NULL, printStringReturnType, printStringParamsType, printStringKeywordsType);

    Type* printCharArrayReturnType = new Type(TypeKind::VOID);
    vector<Type*> *printCharArrayKeywordsType = new vector<Type*>{ new Type(TypeKind::CHAR, 1024) };
    vector<Type*> *printCharArrayParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "printCharArray", "([C)V", true, NULL, printCharArrayReturnType, printCharArrayParamsType, printCharArrayKeywordsType);

    Type* printObjectReturnType = new Type(TypeKind::VOID);
    vector<Type*> *printObjectKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "java/lang/Object") };
    vector<Type*> *printObjectParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "printObject", "(Ljava/lang/Object;)V", true, NULL, printObjectReturnType, printObjectParamsType, printObjectKeywordsType);

    Type* readReturnType = new Type(TypeKind::CLASS_NAME, "java/lang/String");
    vector<Type*> *readKeywordsType = new vector<Type*>;
    vector<Type*> *readParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "read", "()Ljava/lang/String;", true, NULL, readReturnType, readParamsType, readKeywordsType);

    Type* readIntReturnType = new Type(TypeKind::INT);
    vector<Type*> *readIntKeywordsType = new vector<Type*>;
    vector<Type*> *readIntParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "readInt", "()I", true, NULL, readIntReturnType, readIntParamsType, readIntKeywordsType);

    Type* readCharReturnType = new Type(TypeKind::CHAR);
    vector<Type*> *readCharKeywordsType = new vector<Type*>;
    vector<Type*> *readCharParamsType = new vector<Type*>;
    inOutFuncs->methods->addMethod(сonstantTable, "readChar", "()C", true, NULL, readCharReturnType, readCharParamsType, readCharKeywordsType);

    сonstantTable->findOrAddFieldRefConstant("java/lang/System", "out", "Ljava/io/PrintStream;");

    сonstantTable->findOrAddMethodRefConstant("java/lang/Object", "<init>", "()V");
    сonstantTable->findOrAddMethodRefConstant("java/io/PrintStream", "print", "(I)V");
    сonstantTable->findOrAddMethodRefConstant("java/io/PrintStream", "print", "(C)V");
    сonstantTable->findOrAddMethodRefConstant("java/io/PrintStream", "print", "(Ljava/lang/String;)V");
    сonstantTable->findOrAddMethodRefConstant("java/io/PrintStream", "print", "([C)V");
    сonstantTable->findOrAddMethodRefConstant("java/io/PrintStream", "print", "Ljava/lang/Object;");
    сonstantTable->findOrAddMethodRefConstant("java/lang/System", "console", "Ljava/io/Console;");
    сonstantTable->findOrAddMethodRefConstant("java/io/Console", "readLine", "()Ljava/lang/String;");
    сonstantTable->findOrAddMethodRefConstant("rtl/InOutFuncs", "read", "()Ljava/lang/String;");
    сonstantTable->findOrAddMethodRefConstant("java/lang/Integer", "parseInt", "(Ljava/lang/String;)I");
    сonstantTable->findOrAddMethodRefConstant("java/lang/String", "charAt", "(I)C");

    items["rtl/InOutFuncs"] = inOutFuncs;
}

void ClassesTable::initClassNSObject() {
    ClassesTableElement* nsobject = new ClassesTableElement("rtl/NSObject", NULL, true);
    ConstantsTable* constantTable = nsobject->constantTable;

    Type* constructorReturnType = new Type(TypeKind::VOID);
    vector<Type*>* constructorKeywordsType = new vector<Type*>;
    vector<Type*>* constructorParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "<init>", "()V", false, NULL, constructorReturnType, constructorParamsType, constructorKeywordsType);

    Type* allocReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
    vector<Type*>* allocKeywordsType = new vector<Type*>;
    vector<Type*>* allocParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "allocStatic", "()Lrtl/NSObject;", true, NULL, allocReturnType, allocParamsType, allocKeywordsType);

    Type* initReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
    vector<Type*>* initKeywordsType = new vector<Type*>;
    vector<Type*>* initParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "initDynamic", "()Lrtl/NSObject;", false, NULL, initReturnType, initParamsType, initKeywordsType);

    Type* newReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
    vector<Type*>* newKeywordsType = new vector<Type*>;
    vector<Type*>* newParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "newStatic", "()Lrtl/NSObject;", true, NULL, newReturnType, newParamsType, newKeywordsType);

    Type* getClassDynamicReturnType = new Type(TypeKind::CLASS_NAME, "java/lang/Class");
    vector<Type*>* getClassDynamicKeywordsType = new vector<Type*>;
    vector<Type*>* getClassDynamicParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "getClassDynamic", "()Ljava/lang/Class;", false, NULL, getClassDynamicReturnType, getClassDynamicParamsType, getClassDynamicKeywordsType);

    Type* getClassStaticReturnType = new Type(TypeKind::CLASS_NAME, "java/lang/Class");
    vector<Type*>* getClassStaticKeywordsType = new vector<Type*>;
    vector<Type*>* getClassStaticParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "getClassStatic", "()Ljava/lang/Class;", true, NULL, getClassStaticReturnType, getClassStaticParamsType, getClassStaticKeywordsType);

    Type* isSubclassOfClassReturnType = new Type(TypeKind::INT);
    vector<Type*>* isSubclassOfClassKeywordsType = new vector<Type*>{new Type(TypeKind::CLASS_NAME, "java/lang/Class")};
    vector<Type*>* isSubclassOfClassParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "isSubclassOfClassStatic", "(Ljava/lang/Class;)I", true, NULL, isSubclassOfClassReturnType, isSubclassOfClassParamsType, isSubclassOfClassKeywordsType);

    Type* classNameReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
    vector<Type*>* classNameKeywordsType = new vector<Type*>;
    vector<Type*>* classNameParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "classNameDynamic", "()Lrtl/NSString;", false, NULL, classNameReturnType, classNameParamsType, classNameKeywordsType);

    Type* superclassReturnType = new Type(TypeKind::CLASS_NAME, "java/lang/Class");
    vector<Type*>* superclassKeywordsType = new vector<Type*>;
    vector<Type*>* superclassParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "superclassDynamic", "()Ljava/lang/Class;", false, NULL, superclassReturnType, superclassParamsType, superclassKeywordsType);

    Type* descriptionReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
    vector<Type*>* descriptionKeywordsType = new vector<Type*>;
    vector<Type*>* descriptionParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "descriptionDynamic", "()Lrtl/NSString;", false, NULL, descriptionReturnType, descriptionParamsType, descriptionKeywordsType);

    Type* isEqualReturnType = new Type(TypeKind::INT);
    vector<Type*>* isEqualKeywordsType = new vector<Type*>{new Type(TypeKind::CLASS_NAME, "rtl/NSObject")};
    vector<Type*>* isEqualParamsType = new vector<Type*>;
    nsobject->methods->addMethod(constantTable, "isEqualDynamic", "(Lrtl/NSObject;)I", false, NULL, isEqualReturnType, isEqualParamsType, isEqualKeywordsType);

    constantTable->findOrAddMethodRefConstant("java/lang/Object", "<init>", "()V");
    constantTable->findOrAddMethodRefConstant("rtl/NSObject", "<init>", "()V");
    constantTable->findOrAddMethodRefConstant("rtl/NSObject", "allocStatic", "()Lrtl/NSObject;");
    constantTable->findOrAddMethodRefConstant("rtl/NSObject", "initDynamic", "()Lrtl/NSObject;");
    constantTable->findOrAddMethodRefConstant("java/lang/Object", "getClass", "()Ljava/lang/Class;");
    constantTable->findOrAddMethodRefConstant("rtl/NSObject", "getClassStatic", "()Ljava/lang/Class;");
    constantTable->findOrAddMethodRefConstant("java/lang/Class", "isAssignableFrom", "(Ljava/lang/Class;)Z");
    constantTable->findOrAddMethodRefConstant("java/lang/Class", "getName", "()Ljava/lang/String;");
    constantTable->findOrAddMethodRefConstant("java/lang/String", "toCharArray", "()[C");
    constantTable->findOrAddMethodRefConstant("rtl/NSString", "stringWithCStringStatic", "([C)Lrtl/NSString;");
    constantTable->findOrAddMethodRefConstant("java/lang/Class", "getSuperclass", "()Ljava/lang/Class;");

    int strNum = constantTable->findOrAddConstant(UTF8, "nsobject implementation");
    constantTable->findOrAddConstant(STRING, NULL, strNum);

    items["rtl/NSObject"] = nsobject;
}

void ClassesTable::initClassNSString() {
    string superclassName = "rtl/NSObject";
	ClassesTableElement* nsstring = new ClassesTableElement("rtl/NSString", &superclassName, true);
	ConstantsTable* constantTable = nsstring->constantTable;

	Type* stringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* stringKeywordsType = new vector<Type*>;
	vector<Type*>* stringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "stringStatic", "()Lrtl/NSString;", true, NULL, stringReturnType, stringParamsType, stringKeywordsType);

	Type* stringWithCStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* stringWithCStringKeywordsType = new vector<Type*>{ new Type(TypeKind::CHAR, 1024) };
	vector<Type*>* stringWithCStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "stringWithCStringStatic", "([C)Lrtl/NSString;", true, NULL, stringWithCStringReturnType, stringWithCStringParamsType, stringWithCStringKeywordsType);

	Type* stringWithStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* stringWithStringKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSString")};
	vector<Type*>* stringWithStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "stringWithStringStatic", "(Lrtl/NSString;)Lrtl/NSString;", true, NULL, stringWithStringReturnType, stringWithStringParamsType, stringWithStringKeywordsType);

	Type* cStringReturnType = new Type(TypeKind::CHAR, 1024);
	vector<Type*>* cStringKeywordsType = new vector<Type*>;
	vector<Type*>* cStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "cStringDynamic", "()[C;", false, NULL, cStringReturnType, cStringParamsType, cStringKeywordsType);

	Type* capitalizeStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* capitalizeStringKewordsType = new vector<Type*>;
	vector<Type*>* capitalizeStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "capitalizeStringDynamic", "()Lrtl/NSString;", false, NULL, capitalizeStringReturnType, capitalizeStringParamsType, capitalizeStringKewordsType);

	Type* characterAtIndexReturnType = new Type(TypeKind::CHAR);
	vector<Type*>* characterAtIndexKeywordsType = new vector<Type*>{ new Type(TypeKind::INT) };
	vector<Type*>* characterAtIndexParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "characterAtIndexDynamic", "(I)C;", false, NULL, characterAtIndexReturnType, characterAtIndexParamsType, characterAtIndexKeywordsType);

	Type* hasPrefixReturnType = new Type(TypeKind::INT);
	vector<Type*>* hasPrefixKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
	vector<Type*>* hasPrefixParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "hasPrefixDynamic", "(Lrtl/NSString;)I", false, NULL, hasPrefixReturnType, hasPrefixParamsType, hasPrefixKeywordsType);

	Type* hasSuffixReturnType = new Type(TypeKind::INT);
	vector<Type*>* hasSuffixKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
	vector<Type*>* hasSuffixParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "hasSuffixDynamic", "(Lrtl/NSString;)I", false, NULL, hasSuffixReturnType, hasSuffixParamsType, hasSuffixKeywordsType);

	Type* intValueReturnType = new Type(TypeKind::INT);
	vector<Type*>* intValueKeywordsType = new vector<Type*>;
	vector<Type*>* intValueParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "intValueDynamic", "()I", false, NULL, intValueReturnType, intValueParamsType, intValueKeywordsType);

	Type* isEqualReturnType = new Type(TypeKind::INT);
	vector<Type*>* isEqualKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
	vector<Type*>* isEqualParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "isEqualDynamic", "(Lrtl/NSObject;)I", false, NULL, isEqualReturnType, isEqualParamsType, isEqualKeywordsType);

	Type* isEqualToStringReturnType = new Type(TypeKind::INT);
	vector<Type*>* isEqualToStringKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
	vector<Type*>* isEqualToStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "isEqualToStringDynamic", "(Lrtl/NSString;)I", false, NULL, isEqualToStringReturnType, isEqualToStringParamsType, isEqualToStringKeywordsType);

	Type* lengthReturnType = new Type(TypeKind::INT);
	vector<Type*>* lengthKeywordsType = new vector<Type*>;
	vector<Type*>* lengthParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "lengthDynamic", "()I", false, NULL, lengthReturnType, lengthParamsType, lengthKeywordsType);

	Type* lowercaseStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* lowercaseStringKeywordsType = new vector<Type*>;
	vector<Type*>* lowercaseStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "lowercaseStringDynamic", "()Lrtl/NSString;", false, NULL, lowercaseStringReturnType, lowercaseStringParamsType, lowercaseStringKeywordsType);

	Type* uppercaseStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* uppercaseStringKeywordsType = new vector<Type*>;
	vector<Type*>* uppercaseStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "uppercaseStringDynamic", "()Lrtl/NSString;", false, NULL, uppercaseStringReturnType, uppercaseStringParamsType, uppercaseStringKeywordsType);

	Type* stringByAppendingStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* stringByAppendingStringKeywordsType = new vector<Type*>{new Type(TypeKind::CLASS_NAME, "rtl/NSString")};
	vector<Type*>* stringByAppendingStringParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "stringByAppendingStringDynamic", "(Lrtl/NSString;)Lrtl/NSString;", false, NULL, stringByAppendingStringReturnType, stringByAppendingStringParamsType, stringByAppendingStringKeywordsType);

	Type* descriptionReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* descriptionKeywordsType = new vector<Type*>;
	vector<Type*>* descriptionParamsType = new vector<Type*>;
	nsstring->methods->addMethod(constantTable, "descriptionDynamic", "()Lrtl/NSString;", false, NULL, descriptionReturnType, descriptionParamsType, descriptionKeywordsType);

	constantTable->findOrAddFieldRefConstant("rtl/NSString", "string", "java/lang/String");

	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "<init>", "()V");
	constantTable->findOrAddMethodRefConstant("rtl/NSString", "<init>", "(Ljava/lang/String;)V");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "<init>", "([C)V");
	constantTable->findOrAddMethodRefConstant("rtl/NSString", "<init>", "(Lrtl/NSString;)V");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "length", "()I");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "charAt", "(I)C");
	constantTable->findOrAddMethodRefConstant("java/lang/Character", "isWhitespace", "(C)Z");
	constantTable->findOrAddMethodRefConstant("java/lang/Character", "toUpperCase", "(C)C");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "startWith", "(Ljava/lang/String;)Z");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "endsWith", "(Ljava/lang/String;)Z");
	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "initDynamic", "()Lrtl/NSObject");
	constantTable->findOrAddMethodRefConstant("java/lang/Integer", "parseInt", "(Ljava/lang/String;)I");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "equals", "(Ljava/lang/Object;)Z");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "toLowerCase", "()Ljava/lang/String;");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "toUpperCase", "()Ljava/lang/String;");
	
	items["rtl/NSString"] = nsstring;
}

void ClassesTable::initClassNSArray() {
    string superclassName = "rtl/NSObject";
	ClassesTableElement* nsarray = new ClassesTableElement("rtl/NSArray", &superclassName, true);
	ConstantsTable* constantTable = nsarray->constantTable;

	Type* arrayReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayKeywordsType = new vector<Type*>;
	vector<Type*>* arrayParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "arrayStatic", "()Lrtl/NSArray;", true, NULL, arrayReturnType, arrayParamsType, arrayKeywordsType);

	Type* arrayWithArrayReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayWithArrayKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSArray") };
	vector<Type*>* arrayWithArrayParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "arrayWithArrayStatic", "(Lrtl/NSArray;)Lrtl/NSArray;", true, NULL, arrayWithArrayReturnType, arrayWithArrayParamsType, arrayWithArrayKeywordsType);

	Type* arrayWithObjectsReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayWithObjectsKeywordsType = new vector<Type*>;
	vector<Type*>* arrayWithObjectsParamsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject", 1024) };
	nsarray->methods->addMethod(constantTable, "arrayWithObjectsStatic", "([Lrtl/NSObject;)Lrtl/NSArray;", true, NULL, arrayWithObjectsReturnType, arrayWithObjectsParamsType, arrayWithObjectsKeywordsType);

	Type* arrayWithObjectReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayWithObjectKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
	vector<Type*>* arrayWithObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "arrayWithObjectStatic", "(Lrtl/NSObject;)Lrtl/NSArray;", true, NULL, arrayWithObjectReturnType, arrayWithObjectParamsType, arrayWithObjectKeywordsType);

	Type* arrayByAddingObjectReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayByAddingObjectKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
	vector<Type*>* arrayByAddingObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "arrayByAddingObjectDynamic", "(Lrtl/NSObject;)Lrtl/NSArray;", false, NULL, arrayByAddingObjectReturnType, arrayByAddingObjectParamsType, arrayByAddingObjectKeywordsType);

	Type* arrayByAddingObjectsFromArrayReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* arrayByAddingObjectsFromArrayKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME,"rtl/NSArray") };
	vector<Type*>* arrayByAddingObjectsFromArrayParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "arrayByAddingObjectsFromArrayDynamic", "(Lrtl/NSArray;)Lrtl/NSArray;", false, NULL, arrayByAddingObjectsFromArrayReturnType, arrayByAddingObjectsFromArrayParamsType, arrayByAddingObjectsFromArrayParamsType);

	Type* componentsJoinedByStringReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* componentsJoinedByStringKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSString") };
	vector<Type*>* componentsJoinedByStringParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "componentsJoinedByStringDynamic", "(Lrtl/NSString;)Lrtl/NSString;", false, NULL, componentsJoinedByStringReturnType, componentsJoinedByStringParamsType, componentsJoinedByStringKeywordsType);

	Type* containsObjectReturnType = new Type(TypeKind::INT);
	vector<Type*>* containsObjectKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
	vector<Type*>* containsObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "containsObjectDynamic", "(Lrtl/NSObject;)I", false, NULL, containsObjectReturnType, containsObjectParamsType, containsObjectKeywordsType);

	Type* countReturnType = new Type(TypeKind::INT);
	vector<Type*>* countKeywordsType = new vector<Type*>;
	vector<Type*>* countParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "countDynamic", "()I", false, NULL, countReturnType, countParamsType, countKeywordsType);

	Type* descriptionReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSString");
	vector<Type*>* descriptionKeywordsType = new vector<Type*>;
	vector<Type*>* descriptionParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "descriptionDynamic", "()Lrtl/NSString;", false, NULL, descriptionReturnType, descriptionParamsType, descriptionKeywordsType);

	Type* firstObjectReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
	vector<Type*>* firstObjectKeywordsType = new vector<Type*>;
	vector<Type*>* firstObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "firstObjectDynamic", "()Lrtl/NSObject;", false, NULL, firstObjectReturnType, firstObjectParamsType, firstObjectKeywordsType);

	Type* firstObjectCommonWithArrayReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
	vector<Type*>* firstObjectCommonWithArrayKeywordsType = new vector<Type*>{new Type(TypeKind::CLASS_NAME, "rtl/NSArray")};
	vector<Type*>* firstObjectCommonWithArrayParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "firstObjectCommonWithArrayDynamic", "(Lrtl/NSArray;)Lrtl/NSObject;", false, NULL, firstObjectCommonWithArrayReturnType, firstObjectCommonWithArrayParamsType, firstObjectCommonWithArrayKeywordsType);

	Type* getObjectsReturnType = new Type(TypeKind::VOID);
	vector<Type*>* getObjectsKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject", 1024) };
	vector<Type*>* getObjectsParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "getObjectsDynamic", "([Lrtl/NSObject;)V", false, NULL, getObjectsReturnType, getObjectsParamsType, getObjectsKeywordsType);

	Type* indexOfObjectReturnType = new Type(TypeKind::INT);
	vector<Type*>* indexOfObjectKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSObject") };
	vector<Type*>* indexOfObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "indexOfObjectDynamic", "(Lrtl/NSObject;)I", false, NULL, indexOfObjectReturnType, indexOfObjectParamsType, indexOfObjectKeywordsType);

	Type* initReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSArray");
	vector<Type*>* initKeywordsType = new vector<Type*>;
	vector<Type*>* initParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "initDynamic", "()Lrtl/NSArray;", false, NULL, initReturnType, initParamsType, initKeywordsType);

	Type* isEqualToArrayReturnType = new Type(TypeKind::INT);
	vector<Type*>* isEqualToArrayKeywordsType = new vector<Type*>{ new Type(TypeKind::CLASS_NAME, "rtl/NSArray") };
	vector<Type*>* isEqualToArrayParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "isEqualToArrayDynamic", "(Lrtl/NSArray;)I", false, NULL, isEqualToArrayReturnType, isEqualToArrayParamsType, isEqualToArrayKeywordsType);

	Type* lastObjectReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
	vector<Type*>* lastObjectKeywordsType = new vector<Type*>;
	vector<Type*>* lastObjectParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "lastObjectDynamic", "()Lrtl/NSObject;", false, NULL, lastObjectReturnType, lastObjectParamsType, lastObjectKeywordsType);

	Type* objectAtIndexReturnType = new Type(TypeKind::CLASS_NAME, "rtl/NSObject");
	vector<Type*>* objectAtIndexKeywordsType = new vector<Type*>{ new Type(TypeKind::INT) };
	vector<Type*>* objectAtIndexParamsType = new vector<Type*>;
	nsarray->methods->addMethod(constantTable, "objectAtIndexDynamic", "(I)Lrtl/NSObject;", false, NULL, objectAtIndexReturnType, objectAtIndexParamsType, objectAtIndexKeywordsType);

	constantTable->findOrAddFieldRefConstant("rtl/NSArray", "array", "[Lrtl/NSObject");

	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "<init>", "()V");
	constantTable->findOrAddMethodRefConstant("rtl/NSArray", "<init>", "()V");
	constantTable->findOrAddMethodRefConstant("rtl/NSArray", "<init>", "(Lrtl/NSArray;)V");
	constantTable->findOrAddMethodRefConstant("rtl/NSArray", "<init>", "([Lrtl/NSObject;)V");
	constantTable->findOrAddMethodRefConstant("rtl/NSString", "cStringDynamic", "()[C");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "<init>", "([C)V");
	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "descriptionDynamic", "()Lrtl/NSString;");
	constantTable->findOrAddMethodRefConstant("java/lang/String", "toCharArray", "()[C");
	constantTable->findOrAddMethodRefConstant("rtl/NSString", "stringWithCStringStatic", "([C)Lrtl/NSString;");
	constantTable->findOrAddMethodRefConstant("rtl/NSArray", "containsObjectDynamic", "(Lrtl/NSObject;)I");
	constantTable->findOrAddMethodRefConstant("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V");
	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "initDynamic", "()Lrtl/NSObject;");
	constantTable->findOrAddMethodRefConstant("rtl/NSObject", "isEqualDynamic", "(Lrtl/NSObject;)I");
	constantTable->findOrAddMethodRefConstant("rtl/NSArray", "initDynamic", "()Lrtl/NSArray;");

	items["rtl/NSArray"] = nsarray;
}

//--------------------------------------------------------------FieldsTableElement--------------------------------------------------------------

FieldsTableElement::FieldsTableElement(int name, int descriptor, bool isInstance, int instanceIndex, Type* type, string nameStr, string descriptorStr, ExprNode* initialValue) {
    this->name = name;
    this->descriptor = descriptor;
    this->isInstance = isInstance;
    this->instanceIndex = instanceIndex;
    this->type = type;
    this->nameStr = nameStr;
    this->descriptorStr = descriptorStr;
    this->initialValue = initialValue;
}

string FieldsTableElement::toCSVString(char separator) {
    string res = "";
    res += to_string(name) + " (" + nameStr + ")" + separator;
    res += to_string(descriptor) + " (" + descriptorStr + ")" + separator;
    res += string((isInstance ? "true" : "false")) + separator;
    res += type->toString() + separator;
    if (initialValue == NULL) {
        res += string("empty");
    }
    else {
        res += to_string(initialValue->getId());
    }
    return res;
}

void FieldsTableElement::fillLiterals(ConstantsTable* constantTable) {
    if (initialValue != NULL) {
		initialValue->fillLiterals(constantTable);
    }
}

//--------------------------------------------------------------FieldsTable--------------------------------------------------------------

void FieldsTable::addField(ConstantsTable* constantTable, string name, string descriptor, bool isInstance, Type* type, ExprNode* initValue) {
    int nameId = constantTable->findOrAddConstant(UTF8, name);
    int descriptorId = constantTable->findOrAddConstant(UTF8, descriptor);
    if (isInstance) {
        FieldsTableElement* field = new FieldsTableElement(nameId, descriptorId, isInstance, maxInstanceIndex, type, name, descriptor, initValue);
        maxInstanceIndex++;
        items[name] = field;
    }
    else {
        FieldsTableElement* field = new FieldsTableElement(nameId, descriptorId, isInstance, 0, type, name, descriptor, initValue);
        items[name] = field;
    }
}

void FieldsTable::toCSVFile(string filename, string filepath, char separator) {
    ofstream out(filepath + filename);
    out << "Name" << separator << "Descriptor" << separator << "IsInstance" << separator << "Type" << separator << "InitValueIdNode" <<  endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string str = iter->second->toCSVString(separator);
        out << str << endl;
        ++iter;
    }
    out.close();
}

//--------------------------------------------------------------MethodsTableElement--------------------------------------------------------------

MethodsTableElement::MethodsTableElement(int name, int descriptor, bool isClassMethod, StmtNode* bodyStart, Type* returnType, vector<Type*>* paramsTypes, vector<Type*>* keywordsTypes, string nameStr, string descriptorStr) {
    this->name = name;
	this->descriptor = descriptor;
	this->isClassMethod = isClassMethod;
	this->bodyStart = bodyStart;
	this->returnType = returnType;
	this->paramsTypes = paramsTypes;
	this->keywordsTypes = keywordsTypes;
	localVariables = new LocalVariablesTable();
	this->nameStr = nameStr;
	this->descriptorStr = descriptorStr;
}

string MethodsTableElement::toCSVString(string methodName, char separator) {
    string res = "";
    res += to_string(name) + " (" + nameStr + ")" + separator;
    res += to_string(descriptor) + " (" + descriptorStr + ")" + separator;
    res += string((isClassMethod ? "true" : "false")) + separator;
    res += returnType->toString() + separator;

    string paramsTypesStr = "";
    for (int i = 0; i < paramsTypes->size(); i++) {
        paramsTypesStr += paramsTypes->at(i)->toString();
        if (i != paramsTypes->size() - 1) {
            paramsTypesStr += ',';
        }
    }

    string keywordsTypesStr = "";
    for (int i = 0; i < keywordsTypes->size(); i++) {
        keywordsTypesStr += keywordsTypes->at(i)->toString();
        if (i != keywordsTypes->size() - 1) {
            keywordsTypesStr += ',';
        }
    }

    res += paramsTypesStr + separator;
    res += keywordsTypesStr + separator;
    if (bodyStart != NULL) {
        res += to_string(bodyStart->getId()) + separator;
    }
    else {
        res += string("empty") + separator;
    }

    if (localVariables->items.size() > 0) {
        res += methodName + "_LocalVariablesTable.csv";
    }
    else {
        res += string("emptyTable");
    }
    return res;
}

void MethodsTableElement::refTablesToCSVFile(string methodName, string filepath, char separator) {
    if (localVariables->items.size() > 0) {
        localVariables->toCSVFile(methodName + "_LocalVariablesTable.csv", filepath, separator);
    }
}

void MethodsTableElement::fillFieldRefs(ConstantsTable *constantTable, ClassesTableElement* classTableElement) {
    if (bodyStart) {
        bodyStart->fillFieldRefs(constantTable, localVariables, classTableElement);
    }
}

void MethodsTableElement::fillMethodRefs(ConstantsTable* constantTable, ClassesTableElement* classTableElement) {
    if (bodyStart) {
        bodyStart->fillMethodRefs(constantTable, localVariables, classTableElement, !isClassMethod);
    }
}

void MethodsTableElement::fillLiterals(ConstantsTable* constantTable) {
    if (bodyStart) {
        bodyStart->fillLiterals(constantTable);
    }
}

void MethodsTableElement::semanticTransform() {
    if (bodyStart != nullptr) {
        bodyStart->semanticTransform(localVariables);
        if (returnType->dataType != TypeKind::VOID) {
            addDefaultReturn(bodyStart);
        }
    }
}

void MethodsTableElement::addDefaultReturn(StmtNode *lastStatement) {
    if (lastStatement == nullptr) return;

    StmtListNode* stmtList = nullptr;
    
    if (bodyStart->getType() == StmtNode::COMPOUND) {
        stmtList = bodyStart->getCompound();
    } else {
        stmtList = StmtListNode::createStmtList(bodyStart);
    }
    
    StmtNode* defaultReturn = nullptr;
    
    if (returnType->dataType == TypeKind::VOID) {
        defaultReturn = StmtNode::createReturn(nullptr);
    } else {
        ExprNode* defaultValue = nullptr;
        
        switch (returnType->dataType) {
            case TypeKind::INT:
                defaultValue = ExprNode::createLiteral(ValueNode::createInt(0));
                break;
            case TypeKind::FLOAT:
                defaultValue = ExprNode::createLiteral(ValueNode::createFloat(0.0f));
                break;
            case TypeKind::CHAR:
                defaultValue = ExprNode::createLiteral(ValueNode::createChar('\0'));
                break;
            case TypeKind::BOOL:
                defaultValue = ExprNode::createLiteral(ValueNode::createBool(false));
                break;
            case TypeKind::CLASS_NAME:
            case TypeKind::TYPE_ID:
                defaultValue = ExprNode::createNil();
                break;
            default:
                defaultValue = ExprNode::createNil();
                break;
        }
        
        defaultReturn = StmtNode::createReturn(defaultValue);
    }
    
    if (stmtList != nullptr) {
        stmtList = StmtListNode::addStmtToList(stmtList, defaultReturn);
        StmtNode* newBody = StmtNode::createCompound(stmtList);
        bodyStart = newBody;
    }
}

//--------------------------------------------------------------MethodsTable--------------------------------------------------------------

MethodsTableElement* MethodsTable::addMethod(ConstantsTable* constantTable, string name, string descriptor, bool isClassMethod, StmtNode* bodyStart, Type* returnType, vector<Type*>* paramsTypes, vector<Type*>* keywordsTypes) {
    if (items.count(name) != 0) {
        string msg = "Method '" + name + "' already exists";
        throw new exception(msg.c_str());
    }
    int nameId = constantTable->findOrAddConstant(UTF8, name);
    int descriptorId = constantTable->findOrAddConstant(UTF8, descriptor);
    MethodsTableElement *method = new MethodsTableElement(nameId, descriptorId, isClassMethod, bodyStart, returnType, paramsTypes, keywordsTypes, name, descriptor);
    items[name] = method;
    return method;
}

void MethodsTable::toCSVFile(string filename, string filepath, char separator) {
    ofstream out(filepath + filename);
    out << "Name" << separator << "Descriptor" << separator << "IsClassMethod" << separator << "ReturnType" << separator << "ParamsTypes" << separator << "KeywordsTypes" << separator << "BodyStartStatementId" << separator << "LocalVariablesTableName" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string methodName = filename.substr(0, filename.find(".")) + "_" + iter->first;
        string str = iter->second->toCSVString(methodName, separator);
        out << str << endl;
        iter->second->refTablesToCSVFile(methodName, filepath, separator);
        ++iter;
    }
    out.close();
}

//--------------------------------------------------------------PropertiesTableElement--------------------------------------------------------------

PropertiesTableElement::PropertiesTableElement(int name, int descriptor, bool isReadonly, Type* type, string nameStr, string descriptorStr) {
    this->name = name;
    this->descriptor = descriptor;
    this->isReadonly = isReadonly;
    this->type = type;
    this->nameStr = nameStr;
    this->descriptorStr = descriptorStr;
}

string PropertiesTableElement::toCSVString(char separator) {
    string res = "";
    res += to_string(name) + " (" + nameStr + ")" + separator;
    res += to_string(descriptor) + " (" + descriptorStr + ")" + separator;
    res += string((isReadonly ? "true" : "false")) + separator;
    res += type->toString() + separator;
    return res;
}

//--------------------------------------------------------------PropertiesTable--------------------------------------------------------------

void PropertiesTable::addProperty(ConstantsTable* constantTable, string name, string descriptor, bool isReadonly, Type* type) {
    if (items.count(name) != 0) {
        string msg = "Property '" + name + "' already exists";
        throw new exception(msg.c_str());
    }
    int nameId = constantTable->findOrAddConstant(UTF8, name);
    int descriptorId = constantTable->findOrAddConstant(UTF8, descriptor);
    PropertiesTableElement *property = new PropertiesTableElement(nameId, descriptorId, isReadonly, type, name, descriptor);
    items[name] = property;
}

void PropertiesTable::toCSVFile(string filename, string filepath, char separator) {
    ofstream out(filepath + filename);
    out << "Name" << separator << "Descriptor" << separator << "IsReadonly" << separator << "Type" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string str = iter->second->toCSVString(separator);
        out << str << endl;
        ++iter;
    }
    out.close();
}

//--------------------------------------------------------------LocalVariablesTableElement--------------------------------------------------------------

LocalVariablesTableElement::LocalVariablesTableElement(int id, string name, Type* type) {
    this->id = id;
    this->name = name;
    this->type = type;
}

string LocalVariablesTableElement::toCSVString(char separator) {
    string res = "";
    res += to_string(id) + separator;
    res += name + separator;
    res += type->toString();
    return res;
}

//--------------------------------------------------------------LocalVariablesTable--------------------------------------------------------------

int LocalVariablesTable::findOrAddLocalVariable(string name, Type* type) {
    if (items.count(name) == 0) {
        items[name] = new LocalVariablesTableElement(maxId++, name, type);
    }
    else {
        string msg = "Variable '" + name + "' already exists";
        throw new exception(msg.c_str());
    }
    return items[name]->id;
}

void LocalVariablesTable::toCSVFile(string filename, string fileoath, char separator) {
    ofstream out(fileoath + filename);
    out << "Id" << separator << "Name" << separator << "Type" << endl;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string str = iter->second->toCSVString(separator);
        out << str << endl;
        ++iter;
    }
    out.close();
}

bool LocalVariablesTable::isContains(string name) {
    return items.count(name) != 0;
}
