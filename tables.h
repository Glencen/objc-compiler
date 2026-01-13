#ifndef TABLES_H
#define TABLES_H

#include <map>
#include <vector>
#include "classes.h"
using namespace std;

class LocalVariablesTable;
class FieldsTable;
class FieldsTableElement;
class MethodsTableElement;
class MethodsTable;
class PropertiesTable;
class Type;

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
    int firstRef = 0;
    int secondRef = 0;
};

class ConstantsTable {
public:
    int maxId = 1;
    map<int, ConstantsTableElement*> items;
};

class FunctionsTableElement {
public:
    StmtNode *bodyStart = NULL;
    LocalVariablesTable *localVariables = NULL;
    string nameStr;
    string descriptorStr;
	vector<Type*> *parametersTypes = NULL;
	Type *returnType = NULL;
};

class FunctionsTable {
public:
    static map<string, FunctionsTableElement*> items;
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
};

class ClassesTable {
public:
    static map<string, ClassesTableElement*> items;
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
};

class FieldsTable {
public:
	int maxInstanceIndex = 1;
    map <string, FieldsTableElement*> items;
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
};

class MethodsTable {
public:
    map<string, MethodsTableElement*> items;
};

class PropertiesTableElement {
public:
    int name = 0;
    int descriptor = 0;
    bool isReadonly = false;
    Type *type;
    string nameStr;
    string descriptorStr;
};

class PropertiesTable {
public:
    map<string, PropertiesTableElement*> items;
};

class LocalVariablesTableElement {
public:
    int id = 0;
    string name;
    Type *type;
};

class LocalVariablesTable {
public:
    int maxId = 0;
    map<string, LocalVariablesTableElement*> items;
};

class Type {
public:
    TypeNode::TypeKind dataType;
    string className;
    ExprNode *arrSize = NULL;
};

#endif