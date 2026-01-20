#include <fstream>
#include <algorithm>
#include "tables.h"
#include "semantic_exceptions.h"
#include "output_utils.h"

map<string, FunctionsTableElement*> FunctionsTable::items;
map<string, ClassesTableElement*> ClassesTable::items;

//--------------------------------------------------------------Type--------------------------------------------------------------

Type::Type(TypeKind dataType, string className, list<ExprNode*>* arraySizes) {
    this->dataType = dataType;
    this->className = className;
    this->arraySizes = arraySizes;
    this->arrayDimension = arraySizes ? arraySizes->size() : 0;
    
    if (arraySizes) {
        for (ExprNode* size : *arraySizes) {
            if (size && size->getKind() == ExprKind::UNARY_MINUS) {
                ExprNode* operand = size->getOperand();
                if (operand && operand->getKind() == ExprKind::LITERAL) {
                    ValueNode* value = operand->getLiteral();
                    if (value && value->getValueKind() == ValueKind::INT_LIT) {
                        int intValue = value->getInt();
                        if (intValue > 0) {
                            throw array_exception(
                                "Negative array size '-" + to_string(intValue) + "'", "Type constructor", -1, -1, "Array size expression"
                            );
                        }
                    }
                }
            }
        }
    }
}

Type::Type(TypeKind dataType, string className) {
    this->dataType = dataType;
    this->className = className;
    this->arraySizes = nullptr;
    this->arrayDimension = 0;
}

Type::Type(TypeKind dataType) {
    this->dataType = dataType;
    this->className = "";
    this->arraySizes = nullptr;
    this->arrayDimension = 0;
}

Type::Type(TypeKind dataType, list<ExprNode*>* arraySizes) {
    this->dataType = dataType;
    this->className = "";
    this->arraySizes = arraySizes;
    this->arrayDimension = arraySizes ? arraySizes->size() : 0;
    if (arraySizes) {
        for (ExprNode* size : *arraySizes) {
            if (size && size->getKind() == ExprKind::UNARY_MINUS) {
                ExprNode* operand = size->getOperand();
                if (operand && operand->getKind() == ExprKind::LITERAL) {
                    ValueNode* value = operand->getLiteral();
                    if (value && value->getValueKind() == ValueKind::INT_LIT) {
                        int intValue = value->getInt();
                        if (intValue > 0) {
                            throw array_exception(
                                "Negative array size '-" + to_string(intValue) + "'", "Type constructor", -1, -1, "Array size expression"
                            );
                        }
                    }
                }
            }
        }
    }
}

Type::Type(TypeKind dataType, string className, ExprNode* arrSize) {
    this->dataType = dataType;
    this->className = className;
    if (arrSize) {
        this->arraySizes = new list<ExprNode*>{arrSize};
        this->arrayDimension = 1;
        if (arrSize->getKind() == ExprKind::UNARY_MINUS) {
            ExprNode* operand = arrSize->getOperand();
            if (operand && operand->getKind() == ExprKind::LITERAL) {
                ValueNode* value = operand->getLiteral();
                if (value && value->getValueKind() == ValueKind::INT_LIT) {
                    int intValue = value->getInt();
                    if (intValue > 0) {
                        throw array_exception(
                                "Negative array size '-" + to_string(intValue) + "'", "Type constructor", -1, -1, "Array size expression"
                        );
                    }
                }
            }
        }
    } else {
        this->arraySizes = nullptr;
        this->arrayDimension = 0;
    }
}

Type::Type(TypeKind dataType, string className, int arrSize) {
    this->dataType = dataType;
    this->className = className;
    if (arrSize > 0) {
        this->arraySizes = new list<ExprNode*>{ExprNode::createLiteral(ValueNode::createInt(arrSize))};
        this->arrayDimension = 1;
    } else {
        this->arraySizes = nullptr;
        this->arrayDimension = 0;
        if (arrSize < 0) {
            throw array_exception(
                "Array size cannot be negative", "Type constructor", -1, -1, "Array size: " + to_string(arrSize)
            );
        }
    }
}

Type::Type(TypeKind dataType, int arrSize) {
    this->dataType = dataType;
    this->className = "";
    if (arrSize > 0) {
        this->arraySizes = new list<ExprNode*>{ExprNode::createLiteral(ValueNode::createInt(arrSize))};
        this->arrayDimension = 1;
    } else {
        this->arraySizes = nullptr;
        this->arrayDimension = 0;
        if (arrSize < 0) {
            throw array_exception(
                "Array size cannot be negative", "Type constructor", -1, -1, "Array size: " + to_string(arrSize)
            );
        }
    }
}

string Type::toString() const {
    string res = "";
    switch (dataType) {
        case TypeKind::INT: res = "int"; break;
        case TypeKind::FLOAT: res = "float"; break;
        case TypeKind::BOOL: res = "bool"; break;
        case TypeKind::CHAR: res = "char"; break;
        case TypeKind::TYPE_ID: res = "id"; break;
        case TypeKind::CLASS_NAME: res = className; break;
        case TypeKind::VOID: res = "void"; break;
        default: res = "unknown"; break;
    }
    
    if (isArray()) {
        res += "[";
        if (arraySizes) {
            for (auto it = arraySizes->begin(); it != arraySizes->end(); ++it) {
                if (it != arraySizes->begin()) res += ",";
                if (*it) {
                    if ((*it)->getKind() == ExprKind::LITERAL) {
                        ValueNode* value = (*it)->getLiteral();
                        if (value && value->getValueKind() == ValueKind::INT_LIT) {
                            res += to_string(value->getInt());
                        }
                    } else {
                        res += "expr";
                    }
                }
            }
        }
        res += "]";
    }
    
    return res;
}

string Type::getDescriptor() const {
    string res = "";
    
    if (isArray()) {
        for (int i = 0; i < arrayDimension; i++) {
            res += "[";
        }
    }
    
    switch (dataType) {
        case TypeKind::INT: res += "I"; break;
        case TypeKind::FLOAT: res += "F"; break;
        case TypeKind::BOOL: res += "Z"; break;
        case TypeKind::CHAR: res += "C"; break;
        case TypeKind::TYPE_ID: res += "Ljava/lang/Object;"; break;
        case TypeKind::CLASS_NAME: res += "L" + className + ";"; break;
        case TypeKind::VOID: res += "V"; break;
        default: break;
    }
    
    return res;
}

bool Type::equal(Type* other) const {
    bool areBothArray = (arraySizes != nullptr && other->arraySizes != nullptr) || (arraySizes == nullptr && other->arraySizes == nullptr);
	return dataType == other->dataType && className == other->className && areBothArray;
}

int Type::getDefaultValue() {
    if (dataType == TypeKind::CLASS_NAME) {
        return 0;
    }
    return 0;
}

bool Type::isCastableTo(Type* other) {
    if (arraySizes != nullptr || other->arraySizes != nullptr) {
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

bool Type::isArray() const {
	return arraySizes != nullptr && !arraySizes->empty();
}

Type* Type::getSuperType() {
    if (dataType != TypeKind::CLASS_NAME) {
        throw type_exception(
            "Type is not a class", "Type::getSuperType", -1, -1, "Type: " + toString()
        );
    }

    ClassesTableElement* thisClass = ClassesTable::items[className];
    string superClassName = thisClass->getSuperClassName();

    if (superClassName == "") {
        return nullptr;
    }

    if (arraySizes && !arraySizes->empty()) {
        return new Type(TypeKind::CLASS_NAME, superClassName, arraySizes);
    } else {
        return new Type(TypeKind::CLASS_NAME, superClassName);
    }
}


//--------------------------------------------------------------ConstantsTableElement--------------------------------------------------------------

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, string utf8String) {
	DEBUG_LOG("DEBUG: creating ConstantsTableElement '" + utf8String + "' of type '" + constantTypeToString(type) + "' with id '" + to_string(id) + "'");
    this->id = id;
    this->type = type;
    this->utf8String = utf8String;
}

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, float floatNumber) {
	DEBUG_LOG("DEBUG: creating ConstantsTableElement '" + to_string(floatNumber) + "' of type '" + constantTypeToString(type) + "' with id '" + to_string(id) + "'");
    this->id = id;
    this->type = type;
    this->floatNumber = floatNumber;
}

ConstantsTableElement::ConstantsTableElement(int id, ConstantType type, int number, int firstRef, int secondRef) {
	DEBUG_LOG("DEBUG: creating ConstantsTableElement '" + utf8String + "' of type '" + constantTypeToString(type) 
				+ "' with id '" + to_string(id) + "'" + (firstRef > 0 ? ", first ref '" + to_string(firstRef) + "'" : "")
				+ (secondRef > 0 ? ", second ref '" + to_string(secondRef) + "'" : ""));
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
        case ConstantType::Utf8:
            res += string("UTF8") + separator;
            res += utf8String;
            break;
        case ConstantType::Integer:
            res += string("Integer") + separator;
            res += to_string(number);
            break;
        case ConstantType::Float:
            res += string("Float") + separator;
            res += to_string(floatNumber);
            break;
        case ConstantType::String:
            res += string("String") + separator;
            res += to_string(firstRef);
            break;
        case ConstantType::Class:
            res += string("Class") + separator;
            res += to_string(firstRef);
            break;
        case ConstantType::Name_And_Type:
            res += string("NameAndType") + separator;
            res += to_string(firstRef) + ", ";
            res += to_string(secondRef);
            break;
        case ConstantType::Field_Ref:
            res += string("FieldRef") + separator;
            res += to_string(firstRef) + ", ";
            res += to_string(secondRef);
            break;
        case ConstantType::Method_Ref:
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
    items[maxId] = new ConstantsTableElement(maxId, ConstantType::Utf8, "Code");
    maxId++;
}

int ConstantsTable::findOrAddConstant(ConstantType type, string utf8String) {
	DEBUG_LOG("DEBUG: calling findConstant for '" + utf8String + "'");
    int res = findConstant(type, utf8String, 0);
    if (res == -1) {
        res = maxId++;
        items[res] = new ConstantsTableElement(res, type, utf8String);
    }
    return res;
}

int ConstantsTable::findOrAddConstant(ConstantType type, float floatNumber) {
	DEBUG_LOG("DEBUG: calling findConstant for '" + to_string(floatNumber) + "'");
    int res = findConstant(type, "", floatNumber);
    if (res == -1) {
        res = maxId++;
        items[res] = new ConstantsTableElement(res, type, floatNumber);
    }
    return res;
}

int ConstantsTable::findOrAddConstant(ConstantType type, int number, int firstRef, int secondRef) {
	DEBUG_LOG("DEBUG: calling findConstant for '" + to_string(number) + "', first ref '" + to_string(firstRef) + "', and second ref '" + to_string(secondRef) + "'");
    int res = findConstant(type, "", 0, number, firstRef, secondRef);
    if (res == -1) {
        res = maxId++;
        items[res] = new ConstantsTableElement(res, type, number, firstRef, secondRef);
    }
    return res;
}

int ConstantsTable::findConstant(ConstantType type, string utf8string, float floatNumber, int number, int firstRef, int secondRef) {
	if (utf8string.data() == nullptr) {
        throw runtime_error(
            "utf8string is null. Source: ConstantsTable::findConstant"
        );
    }
    string compared = utf8string.empty() ? "" : utf8string;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        string curStr = iter->second->utf8String.empty() ? "" : iter->second->utf8String;
        if (iter->second->type == type && curStr == compared && iter->second->number == number && iter->second->firstRef == firstRef && iter->second->secondRef == secondRef) {
            DEBUG_LOG("DEBUG: Constant found. Constant id '" + to_string(iter->first) + "'");
			return iter->first;
        }
        ++iter;
    }
	DEBUG_LOG("DEBUG: Constant not found. Returned -1");
    return -1;
}

ConstantsTableElement* ConstantsTable::getConstant(int id) {
    if (id <= 0) {
		return nullptr;
	}
	return items[id];
}

string ConstantsTable::getConstantString(int id) {
	if (id <= 0) {
		return "";
	}
    if (items[id]->type != ConstantType::Utf8) {
        return "";
    }
	if (items[id]->utf8String.empty()) {
        return "";
    }
    return items[id]->utf8String;
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
	DEBUG_LOG("DEBUG: calling findOrAddFieldRefConstant for className '" + className + "' fieldName '" + fieldName + "' and descriptor '" + descriptor + "'");
    int classNameConst = this->findOrAddConstant(ConstantType::Utf8, className);
    int classConst = this->findOrAddConstant(ConstantType::Class, 0, classNameConst);
    int nameConst = this->findOrAddConstant(ConstantType::Utf8, fieldName);
    int descriptorConst = this->findOrAddConstant(ConstantType::Utf8, descriptor);
    int nameAndTypeConst = this->findOrAddConstant(ConstantType::Name_And_Type, 0, nameConst, descriptorConst);
    int fieldRefConst = this->findOrAddConstant(ConstantType::Field_Ref, 0, nameAndTypeConst, classConst);
    return fieldRefConst;
}

int ConstantsTable::findOrAddMethodRefConstant(string className, string methodName, string descriptor) {
	DEBUG_LOG("DEBUG: calling findOrAddMethodRefConstant for className '" + className + "' methodName '" + methodName + "' and descriptor '" + descriptor + "'");
    int classNameConst = this->findOrAddConstant(ConstantType::Utf8, className);
    int classConst = this->findOrAddConstant(ConstantType::Class, 0, classNameConst);
    int nameConst = this->findOrAddConstant(ConstantType::Utf8, methodName);
    int descriptorConst = this->findOrAddConstant(ConstantType::Utf8, descriptor);
    int nameAndTypeConst = this->findOrAddConstant(ConstantType::Name_And_Type, 0, nameConst, descriptorConst);
    int methodRefConst = this->findOrAddConstant(ConstantType::Method_Ref, 0, nameAndTypeConst, classConst);
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
    
    if (bodyStart->getKind() == StmtKind::COMPOUND) {
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
        throw function_exception(
            "Function '" + name + "' already exists", "FunctionsTable::addFunction", -1, -1, "Function name: " + name + ", descriptor: " + descriptor
        );
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
	DEBUG_LOG("DEBUG: FunctionsTable::fillFieldRefs()");
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    bool isDontContainsMain = true;
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        isDontContainsMain = isDontContainsMain && iter->first == "main";
        iter->second->fillFieldRefs(classTableElement->constantTable, classTableElement);
        ++iter;
    }

    if (!isDontContainsMain) {
        throw function_exception(
            "Function 'main' not found", "FunctionsTable::fillFieldRefs", -1, -1, "Required entry point for program"
        );
    }
}

void FunctionsTable::fillMethodRefs() {
	DEBUG_LOG("DEBUG: FunctionsTable::fillMethodRefs()");
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillMethodRefs(classTableElement->constantTable, classTableElement);
        ++iter;
    }
}

void FunctionsTable::fillLiterals() {
	DEBUG_LOG("DEBUG: FunctionsTable::fillLiterals()");
    ClassesTableElement *classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillLiterals(classTableElement->constantTable);
        ++iter;
    }
}

void FunctionsTable::convertToClassProgramMethods() {
	DEBUG_LOG("DEBUG: FunctionsTable::convertToClassProgramMethods()");
    if (items.count("main") == 0) {
        string msg = "Function 'main' not found";
        throw std::runtime_error(msg.c_str());
    }

    ClassesTableElement* classTableElement = ClassesTable::items["rtl/Program"];
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->convertToClassProgramMethods(classTableElement);
        ++iter;
    }
}

void FunctionsTable::semanticTransform() {
	DEBUG_LOG("DEBUG: FunctionsTable::semanticTransform()");
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->semanticTransform();
        ++iter;
    }
}

//--------------------------------------------------------------ClassesTableElement--------------------------------------------------------------

ClassesTableElement::ClassesTableElement(string name, const string& superclassName, bool isImplementation) {
	DEBUG_LOG("DEBUG: creating ClassesTableElement for class '" + name + "' with super class '" + superclassName + "' as an "
				+ (isImplementation ? "implementation" : "interface"));
	DEBUG_LOG("DEBUG: creating ConstantsTable");
    constantTable = new ConstantsTable();
	DEBUG_LOG("DEBUG: creating FieldsTable");
    fields = new FieldsTable();
	DEBUG_LOG("DEBUG: creating MethodsTable");
    methods = new MethodsTable();
	DEBUG_LOG("DEBUG: creating PropertiesTable");
    properties = new PropertiesTable();
	DEBUG_LOG("DEBUG: calling findOrAddConstant for name '" + name + "'");
    this->name = constantTable->findOrAddConstant(ConstantType::Utf8, name);

    if (!superclassName.empty()) {
        this->superclassName = constantTable->findOrAddConstant(ConstantType::Utf8, superclassName);
    }
	else {
		this->superclassName = 0;
	}

	DEBUG_LOG("DEBUG: creating a reference for '" + name + "'");
    thisClass = constantTable->findOrAddConstant(ConstantType::Class, 0, this->name);

    if (!superclassName.empty()) {
		DEBUG_LOG("DEBUG: creating a reference for '" + superclassName + "'");
        this->superclass = constantTable->findOrAddConstant(ConstantType::Class, 0, this->superclassName);
    }
	else {
		this->superclass = 0;
	}
    this->isImplementation = isImplementation;
}

string ClassesTableElement::toCSVString(char separator) {
    string res = "";
    res += to_string(name) + '(' + constantTable->getConstant(name)->utf8String + ')' + separator;
    if (superclassName != 0) {
        res += to_string(superclassName) + '(' + constantTable->getConstant(superclassName)->utf8String + ')' + separator;
    }
    else {
        res += string("empty") + separator;
    }
    res += string((isImplementation ? "true" : "false")) + separator;
    res += to_string(thisClass) + separator;
    res += to_string(superclass) + separator;

    if (fields->items.size() > 0) {
        res += constantTable->getConstant(name)->utf8String + "_FieldsTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }

    if (methods->items.size() >0 ) {
        res += constantTable->getConstant(name)->utf8String + "_MethodsTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }

    if (properties->items.size() > 0) {
        res += constantTable->getConstant(name)->utf8String + "_PropertiesTable.csv" + separator;
    }
    else {
        res += string("emptyTable") + separator;
    }	
    res += constantTable->getConstant(name)->utf8String + "_ConstantsTable.csv";
    return res;
}

void ClassesTableElement::refTablesToCSVFile(string filepath, char separator) {
    string className = constantTable->getConstant(name)->utf8String;
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
	if (superclassName == 0) {
        return "";
    }
    ConstantsTableElement* constElem = constantTable->getConstant(superclassName);
    if (constElem && constElem->type == ConstantType::Utf8) {
        return constElem->utf8String;
    }
    return "";
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
        if (superclassName != 0) {
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
            if (superclassName != 0) {
                return ClassesTable::items[getSuperClassName()]->getFieldForRef(name, descriptor, className);
            }
        }
    }
    return nullptr;
}

bool ClassesTableElement::isHaveOneOfSuperclass(string name) {
    if (superclassName == 0) {
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
        if (superclassName != 0) {
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
            if (superclassName != 0) {
                return ClassesTable::items[getSuperClassName()]->getMethodForRef(name, descriptor, className);
            }
        }
    }
    return nullptr;
}

void ClassesTableElement::semanticTransform() {
    for (auto iter = methods->items.cbegin(); iter != methods->items.cend(); ++iter) {
        iter->second->semanticTransform();
    }
}

//--------------------------------------------------------------ClassesTable--------------------------------------------------------------

ClassesTableElement* ClassesTable::addClass(string name, const string& superclassName, bool isImplementation, AstNode *classBlock) {
	DEBUG_LOG("DEBUG: calling ClassesTable::addClass()");
    string fullName = "global/" + name;
	DEBUG_LOG("DEBUG: created fullName '" + fullName + "'");
    string fullSuperclassName = "";
	DEBUG_LOG("DEBUG: creating fullSuperclassName");
    if (!superclassName.empty()) {
		DEBUG_LOG("DEBUG: super class not empty");
        if (superclassName == "NSObject" || superclassName == "NSString" || superclassName == "NSArray") {
            fullSuperclassName = "rtl/" + superclassName;
        }
        else {
            fullSuperclassName = "global/" + superclassName;
        }
    }

	DEBUG_LOG("DEBUG: created fullSuperclassName '" + fullSuperclassName + "'");
    ClassesTableElement *element = new ClassesTableElement(fullName, fullSuperclassName, isImplementation);

	DEBUG_LOG("DEBUG: checking for semantic errors");
    if (!isImplementation && items.count(fullName) && items[fullName]->isImplementation) {
        throw class_exception(
            "Class interface '" + name + "' declared after implementation", "ClassesTable::addClass", -1, -1, "Class: " + fullName
        );
    }
    else if (items.count(fullName) && items[fullName]->isImplementation == isImplementation) {
        throw class_exception(
            "Redefinition of class '" + name + "'", "ClassesTable::addClass", -1, -1,
			"Class: " + fullName + ", isImplementation: " + (isImplementation ? "true" : "false")
        );
    }
    else if (!superclassName.empty() && items.count(fullName) &&
				items[fullName] != nullptr && items[fullName]->constantTable->getConstantString(items[fullName]->superclassName) != fullSuperclassName) {
        throw class_exception(
            "Class '" + name + "' with different superclass", "ClassesTable::addClass", -1, -1,
            "Class: " + fullName + ", expected superclass: " + 
            items[fullName]->constantTable->getConstantString(items[fullName]->superclassName) + 
            ", got: " + fullSuperclassName
        );
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
		DEBUG_LOG("DEBUG: setting class name '" + fullName + "' in the ImplementationNode");
        implementation->setClassName(fullName);
        if (!fullSuperclassName.empty()) {
			DEBUG_LOG("DEBUG: setting super class name '" + fullSuperclassName + "' in the ImplementationNode");
            implementation->setSuperClassName(fullSuperclassName);
        }
        else {
            implementation->setSuperClassName("");
        }
    }
    else {
        InterfaceNode* interface = (InterfaceNode*)classBlock;
		DEBUG_LOG("DEBUG: setting class name '" + fullName + "' in the InterfaceNode");
        interface->setClassName(fullName);
        if (fullSuperclassName.empty()) {
			DEBUG_LOG("DEBUG: setting super class name '" + fullSuperclassName + "' in the InterfaceNode");
            interface->setSuperClassName(fullSuperclassName);
        }
        else {
            interface->setSuperClassName("");
        }
    }
    return items[fullName];
}

void ClassesTable::initRTL() {
	DEBUG_LOG("DEBUG: calling initRTL");
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
	DEBUG_LOG("DEBUG: ClassesTable::fillFieldRefs()");
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillFieldRefs();
        ++iter;
    }
}

void ClassesTable::fillMethodRefs() {
	DEBUG_LOG("DEBUG: ClassesTable::fillMethodRefs()");
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->fillMethodRefs();
        ++iter;
    }
}

void ClassesTable::fillLiterals() {
	DEBUG_LOG("DEBUG: ClassesTable::fillLiterals()");
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
        throw class_exception(
            "Class '" + name + "' not found", "ClassesTable::getFullClassName", -1, -1, "Full class name: " + fullName
        );
    }
    return fullName;
}

void ClassesTable::semanticTransform() {
	DEBUG_LOG("DEBUG: ClassesTable::semanticTransform()");
    auto iter = items.cbegin();
    while (iter != items.cend()) {
        iter->second->semanticTransform();
        ++iter;
    }
}

void ClassesTable::initClassProgram() {
	DEBUG_LOG("DEBUG: calling initClassProgram");	
    ClassesTableElement* Program = new ClassesTableElement("rtl/Program", "", true);
    items["rtl/Program"] = Program;
}

void ClassesTable::initClassInOutFuncs() {
	DEBUG_LOG("DEBUG: calling initClassInOutFuncs");
    ClassesTableElement* inOutFuncs = new ClassesTableElement("rtl/InOutFuncs", "", true);

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
	DEBUG_LOG("DEBUG: calling initClassNSObject");
    ClassesTableElement* nsobject = new ClassesTableElement("rtl/NSObject", "", true);
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

    int strNum = constantTable->findOrAddConstant(ConstantType::Utf8, "nsobject implementation");
    constantTable->findOrAddConstant(ConstantType::String, 0, strNum);

    items["rtl/NSObject"] = nsobject;
}

void ClassesTable::initClassNSString() {
	DEBUG_LOG("DEBUG: calling initClassNSString");
	ClassesTableElement* nsstring = new ClassesTableElement("rtl/NSString", "rtl/NSObject", true);
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
	DEBUG_LOG("DEBUG: calling initClassNSArray");
	ClassesTableElement* nsarray = new ClassesTableElement("rtl/NSArray", "rtl/NSObject", true);
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
    if (initialValue == nullptr) {
        res += string("empty");
    }
    else {
        res += to_string(initialValue->getId());
    }
    return res;
}

void FieldsTableElement::fillLiterals(ConstantsTable* constantTable) {
    if (initialValue != nullptr) {
		initialValue->fillLiterals(constantTable);
    }
}

//--------------------------------------------------------------FieldsTable--------------------------------------------------------------

void FieldsTable::addField(ConstantsTable* constantTable, string name, string descriptor, bool isInstance, Type* type, ExprNode* initValue) {
    int nameId = constantTable->findOrAddConstant(ConstantType::Utf8, name);
    int descriptorId = constantTable->findOrAddConstant(ConstantType::Utf8, descriptor);
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
    if (bodyStart != nullptr) {
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
    
    if (bodyStart->getKind() == StmtKind::COMPOUND) {
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
        throw method_exception(
            "Method '" + name + "' already exists", "MethodsTable::addMethod", -1, -1,
            "Method name: " + name + ", descriptor: " + descriptor + ", class method: " + (isClassMethod ? "true" : "false")
        );
    }
    int nameId = constantTable->findOrAddConstant(ConstantType::Utf8, name);
    int descriptorId = constantTable->findOrAddConstant(ConstantType::Utf8, descriptor);
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
        throw field_exception(
            "Property '" + name + "' already exists", "PropertiesTable::addProperty", -1, -1,
            "Property name: " + name + ", descriptor: " + descriptor + ", readonly: " + (isReadonly ? "true" : "false")
        );
    }
    int nameId = constantTable->findOrAddConstant(ConstantType::Utf8, name);
    int descriptorId = constantTable->findOrAddConstant(ConstantType::Utf8, descriptor);
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
        throw symbol_exception(
            "Variable '" + name + "' already declared in this scope", "LocalVariablesTable::findOrAddLocalVariable", -1, -1,
            "Variable name: " + name + ", type: " + type->toString()
        );
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
