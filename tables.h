#ifndef TABLES_H
#define TABLES_H

#include <map>
#include <vector>
#include <list>
#include "classes.h"
#include "semantic_exceptions.h"

using namespace std;

class LocalVariablesTable;
class FieldsTable;
class FieldsTableElement;
class MethodsTableElement;
class MethodsTable;
class PropertiesTable;

class Type {
public:
    TypeKind dataType;
    string className;
    list<ExprNode*>* arraySizes;
    int arrayDimension;

    Type(TypeKind dataType, string className, list<ExprNode*>* arraySizes);
    Type(TypeKind dataType, string className);
    Type(TypeKind dataType);
    Type(TypeKind dataType, list<ExprNode*>* arraySizes);
    Type(TypeKind dataType, string className, ExprNode* arrSize);
    Type(TypeKind dataType, string className, int arrSize);
    Type(TypeKind dataType, int arrSize);

    string toString() const;
    string getDescriptor() const;
    bool equal(Type* other) const;
    int getDefaultValue();
	bool isCastableTo(Type* other);
	bool isPrimitive();
    bool isArray() const;
	Type* getSuperType();
};

enum class ConstantType {
    Utf8,
    Integer,
    Float,
    String,
    Class,
    Name_And_Type,
    Field_Ref,
    Method_Ref
};

class ConstantsTableElement {
public:
    int id = 0;
    ConstantType type;
    string utf8String;
    int number = 0;
    float floatNumber = 0;
    int firstRef = 0;
    int secondRef = 0;

    ConstantsTableElement(int id, ConstantType type, string utf8string);
    ConstantsTableElement(int id, ConstantType type, float floatNumber);
    ConstantsTableElement(int id, ConstantType type, int number, int firstRef, int secondRef);

    string toCSVString(char separator = '|');
};

class ConstantsTable {
public:
    int maxId = 1;
    map<int, ConstantsTableElement*> items;

    int findOrAddConstant(ConstantType type, string utf8String);
    int findOrAddConstant(ConstantType type, float floatNumber);
    int findOrAddConstant(ConstantType type, int number = 0, int firstRef = 0, int secondRef = 0);
    ConstantsTableElement* getConstant(int id);
    string getConstantString(int id);

    ConstantsTable();

    void toCSVFile(string filename, string filepath, char separator = '|');
    int findOrAddFieldRefConstant(string className, string fieldName, string descriptor);
    int findOrAddMethodRefConstant(string className, string methodName, string descriptor);

private:
    int findConstant(ConstantType type, string utf8string, float floatNumber, int number = 0, int firstRef = 0, int secondRef = 0);
};

class FunctionsTableElement {
public:
    StmtNode *bodyStart = nullptr;
    LocalVariablesTable *localVariables = nullptr;
    string nameStr;
    string descriptorStr;
    vector<Type*> *parametersTypes;
    Type *returnType = nullptr;

    FunctionsTableElement(StmtNode *bodyStart, string nameStr, string descriptorStr, vector<Type*> *params, Type *returnType);

    string toCSVString(string funcName, char separator = '|');
    void refTablesToCSVFile(string filename, string filepath, char separator = '|');

    void fillFieldRefs(ConstantsTable *constantTable, ClassesTableElement *classTableElement);
    void fillMethodRefs(ConstantsTable *constantTable, ClassesTableElement *classTableElement);
    void fillLiterals(ConstantsTable *constantTable);

    void convertToClassProgramMethods(ClassesTableElement *classTableElement);
    void semanticTransform();
    void addDefaultReturn(StmtNode *lastStatement);
};

class FunctionsTable {
public:
    static map<string, FunctionsTableElement*> items;

    static FunctionsTableElement* addFunction(string name, string descriptor, StmtNode *bodyStart, vector<Type*> *params, Type *returnType);

    static void toCSVFile(string filename, string filepath, char separator = '|');

    static void fillFieldRefs();
    static void fillMethodRefs();
    static void fillLiterals();
    static void convertToClassProgramMethods();
    static void semanticTransform();
};

class ClassesTableElement {
public:
    int name;
    int superclassName;
    bool isImplementation;
    bool isHaveInterface = false;
    int thisClass;
    int superclass;
    FieldsTable *fields;
    MethodsTable *methods;
    PropertiesTable *properties;
    ConstantsTable *constantTable;

    ClassesTableElement(string name, const string& superclassName, bool isImplementation);

    string toCSVString(char separator = '|');
    void refTablesToCSVFile(string filepath, char separator = '|');
    string getClassName();
    string getSuperClassName();
    void fillFieldRefs();
    void fillMethodRefs();
    void fillLiterals();

    bool isContainsField(string fieldName);
    FieldsTableElement* getFieldForRef(string name, string *descriptor, string *className);
    bool isHaveOneOfSuperclass(string name);
    bool isContainsMethod(string methodName);
    MethodsTableElement* getMethodForRef(string name, string *descriptor, string *className);
    void semanticTransform();
};

class ClassesTable {
public:
    static map<string, ClassesTableElement*> items;

    static ClassesTableElement* addClass(string name, const string& superclassName, bool isImplementation, AstNode *classBlock);

	static void initRTL();
    static void toCSVFile(string filepath, char separator = '|');

	static void fillFieldRefs();
	static void fillMethodRefs();
    static void fillLiterals();

	static string getFullClassName(string name);
    static void semanticTransform();

private:
    static void initClassProgram();
	static void initClassInOutFuncs();
    static void initClassNSObject();
	static void initClassNSString();
    static void initClassNSArray();
};

class FieldsTableElement {
public:
    int name = 0;
    int descriptor = 0;
    bool isInstance = false;
    int instanceIndex = 0;
    Type *type = nullptr;
    string nameStr;
    string descriptorStr;
	ExprNode *initialValue = nullptr;

    FieldsTableElement(int name, int descriptor, bool isInstance, int instanceIndex, Type* type, string nameStr, string descriptorStr, ExprNode* initialValue);

    string toCSVString(char separator = '|');

    void fillLiterals(ConstantsTable* constantTable);
};

class FieldsTable {
public:
    int maxInstanceIndex = 1;
    map <string, FieldsTableElement*> items;

    void addField(ConstantsTable* constantTable, string name, string descriptor, bool isInstance, Type* type, ExprNode* initValue);
    void toCSVFile(string filename, string filepath, char separator = '|');
};

class MethodsTableElement {
public:
    int name = 0;
    int descriptor = 0;
    bool isClassMethod = false;
    StmtNode *bodyStart = nullptr;
    LocalVariablesTable *localVariables = nullptr;
    Type *returnType = nullptr;
    vector<Type*> *paramsTypes;
    vector<Type*> *keywordsTypes;
    string nameStr;
    string descriptorStr;

    MethodsTableElement(int name, int descriptor, bool isClassMethod, StmtNode* bodyStart, Type* returnType, vector<Type*>* paramsTypes, vector<Type*>* keywordsTypes, string nameStr, string descriptorStr);

    string toCSVString(string methodName, char separator = '|');
    void refTablesToCSVFile(string methodName, string filepath, char separator = '|');
    void fillFieldRefs(ConstantsTable *constantTable, ClassesTableElement* classTableElement);
	void fillMethodRefs(ConstantsTable* constantTable, ClassesTableElement* classTableElement);
	void fillLiterals(ConstantsTable* constantTable);
    void semanticTransform();
    void addDefaultReturn(StmtNode *lastStatement);
};

class MethodsTable {
public:
    map<string, MethodsTableElement*> items;

    MethodsTableElement* addMethod(ConstantsTable* constantTable, string name, string descriptor, bool isClassMethod, StmtNode* bodyStart, Type* returnType, vector<Type*>* paramsTypes, vector<Type*>* keywordsTypes);
    void toCSVFile(string filename, string filepath, char separator = '|');
};

class PropertiesTableElement {
public:
    int name = 0;
    int descriptor = 0;
    bool isReadonly = false;
    Type *type = nullptr;
    string nameStr;
    string descriptorStr;

    PropertiesTableElement(int name, int descriptor, bool isReadonly, Type* type, string nameStr, string descriptorStr);
    string toCSVString(char separator = '|');
};

class PropertiesTable {
public:
    map<string, PropertiesTableElement*> items;

    void addProperty(ConstantsTable* constantTable, string name, string descriptor, bool isReadonly, Type* type);
    void toCSVFile(string filename, string filepath, char separator = '|');
};

class LocalVariablesTableElement {
public:
    int id = 0;
    string name;
    Type *type = nullptr;

    LocalVariablesTableElement(int id, string name, Type* type);
    string toCSVString(char separator = '|');
};

class LocalVariablesTable {
public:
    int maxId = 0;
    map<string, LocalVariablesTableElement*> items;

    int findOrAddLocalVariable(string name, Type* type);
    void toCSVFile(string filename, string fileoath, char separator = '|');
    bool isContains(string name);
};

static Type* convertTypeNodeToType(TypeNode* typeNode) {
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

static Type* createArrayType(Type* baseType, list<ExprNode*>* arraySizes) {
    if (!arraySizes || arraySizes->empty()) {
        return baseType;
    }
    ExprNode* firstSize = arraySizes->front();
    return new Type(baseType->dataType, baseType->className, firstSize);
}

static string constantTypeToString(ConstantType type) {
    switch (type) {
        case ConstantType::Utf8:          return "Utf8";
        case ConstantType::Integer:       return "Integer";
        case ConstantType::Float:         return "Float";
        case ConstantType::String:        return "String";
        case ConstantType::Class:         return "Class";
        case ConstantType::Name_And_Type: return "Name_And_Type";
        case ConstantType::Field_Ref:     return "Field_Ref";
        case ConstantType::Method_Ref:    return "Method_Ref";
        default: throw std::invalid_argument("Unknown ConstantType");
    }
}

static string typeKindToString(TypeKind kind) {
    switch (kind) {
        case TypeKind::INT:         return "INT";
        case TypeKind::FLOAT:       return "FLOAT";
        case TypeKind::BOOL:        return "BOOL";
        case TypeKind::CHAR:        return "CHAR";
        case TypeKind::TYPE_ID:     return "TYPE_ID";
        case TypeKind::CLASS_NAME:  return "CLASS_NAME";
        case TypeKind::VOID:        return "VOID";
        default:                    return "NONE";
    }
}

#endif