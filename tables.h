#ifndef TABLES_H
#define TABLES_H

#include <map>
#include <vector>
#include "classes.h"

using namespace std;
using TypeKind = TypeNode::TypeKind;

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
    ExprNode *arrSize = NULL;

    Type(TypeKind dataType, string className, ExprNode* arrSize);
    Type(TypeKind dataType, string className);
    Type(TypeKind dataType);
    Type(TypeKind dataType, ExprNode* arrSize);
    Type(TypeKind dataType, string className, int arrSize);
    Type(TypeKind dataType, int arrSize);

    string toString();
    string getDescriptor();
    bool equal(Type* other);
    int getDefaultValue();
	bool isCastableTo(Type* other);
	bool isPrimitive();
	Type* getSuperType();
};

enum ConstantType {
    UTF8,
    INTEGER,
    FLOAT,
    STRING,
    CLASS,
    NAME_AND_TYPE,
    FIELD_REF,
    METHOD_REF
};

class ConstantsTableElement {
public:
    int id = 0;
    ConstantType type;
    string *utf8String = NULL;
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
    int findOrAddConstant(ConstantType type, int number = NULL, int firstRef = NULL, int secondRef = NULL);
    ConstantsTableElement* getConstant(int id);
    string getConstantString(int id);

    ConstantsTable();

    void toCSVFile(string filename, string filepath, char separator = '|');
    int findOrAddFieldRefConstant(string className, string fieldName, string descriptor);
    int findOrAddMethodRefConstant(string className, string methodName, string descriptor);

private:
    int findConstant(ConstantType type, string *utf8string, float floatNumber, int number = NULL, int firstRef = NULL, int secondRef = NULL);
};

class FunctionsTableElement {
public:
    StmtNode *bodyStart = NULL;
    LocalVariablesTable *localVariables = NULL;
    string nameStr;
    string descriptorStr;
    vector<Type*> *parametersTypes = NULL;
    Type *returnType = NULL;

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

    ClassesTableElement(string name, string *superclassName, bool isImplementation);

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

    static ClassesTableElement* addClass(string name, string *superclassName, bool isImplementation, AstNode *classBlock);

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
    Type *type;
    string nameStr;
    string descriptorStr;
	ExprNode *initialValue = NULL;

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
    StmtNode *bodyStart = NULL;
    LocalVariablesTable *localVariables = NULL;
    Type *returnType;
    vector<Type*> *paramsTypes = NULL;
    vector<Type*> *keywordsTypes = NULL;
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
    Type *type;
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
    Type *type;

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

#endif