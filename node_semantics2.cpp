#include "context.h"

Type convertTypeNodeToType(TypeNode* typeNode, vector<int> arraySizes = {}) {
    if (!typeNode) return Type(TypeKind::NONE);
    
    TypeKind typeKind = typeNode->getKind();
    string className = *typeNode->getClassName()->getClassName();
    
    if (typeNode->isPrimitive()) {
        return Type(typeKind);
    } else if (typeNode->getKind() == TypeKind::CLASS_NAME) {
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

void ReceiverNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgArgNode--------------------------------------------------------------

void MsgArgNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgArgListNode--------------------------------------------------------------

void MsgArgListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------MsgSelectorNode--------------------------------------------------------------

void MsgSelectorNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ExprListNode--------------------------------------------------------------

void ExprListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ExprNode--------------------------------------------------------------

void ExprNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------TypeNode--------------------------------------------------------------

void TypeNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------DeclaratorListNode--------------------------------------------------------------

void DeclaratorListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------DeclNode--------------------------------------------------------------

void DeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------StmtListNode--------------------------------------------------------------

void StmtListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------StmtNode--------------------------------------------------------------

void StmtNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ArraySizeSpecNode--------------------------------------------------------------

void ArraySizeSpecNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamDeclNode--------------------------------------------------------------

void ParamDeclNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------ParamListNode--------------------------------------------------------------

void ParamListNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------FuncDefNode--------------------------------------------------------------

void FuncDefNode::analyzeSemantics(SemanticContext& context) {}

//--------------------------------------------------------------FuncDeclNode--------------------------------------------------------------

void FuncDeclNode::analyzeSemantics(SemanticContext& context) {}

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
        string propertyName = *property->getName()->getIdentifier();
        Type propertyType = convertTypeNodeToType(property->getType());
        bool isReadonly = property->getAttribute() == Attribute::READONLY;
        
        string ivarName = "_" + propertyName;
        
        if (!cls->lookupField(ivarName, false)) {
            auto ivar = make_unique<FieldInfo>(ivarName, propertyType, true, cls);
            cls->addField(move(ivar));
        }
        
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
    string classNameStr = *className->getClassName();
    string superclassNameStr = superClassName ? *superClassName->getClassName() : "";
    
    ClassInfo* cls = context.lookupClass(classNameStr);
    if (!cls) {
        ClassInfo* superclass = nullptr;
        if (!superclassNameStr.empty()) {
            superclass = context.lookupClass(superclassNameStr);
            if (!superclass) {
                throw class_exception("Undefined super class '" + superclassNameStr + "'", "InterfaceNode::analyzeSemantics",
                    -1, -1, "Class: " + classNameStr + "'");
            }
        }
        auto newClass = make_unique<ClassInfo>(classNameStr, superclass);
        cls = newClass.get();
        context.addClass(move(newClass));
    }
    cls->markAsInterface();
    
    context.enterClassScope(cls);
    
    try {
        if (instanceVars) {
            instanceVars->analyzeSemantics(context);
        }
        if (interfaceDeclList) {
            interfaceDeclList->analyzeSemantics(context);
        }

        processProperties(context);
    } catch (...) {
        context.leaveScope();
        throw;
    }
    
    context.leaveScope();
}

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
            if (funcDef) funcDef->analyzeSemantics(context);
            break;
        case ExternalDeclKind::FUNC_DECL:
            if (funcDecl) funcDecl->analyzeSemantics(context);
            break;
        case ExternalDeclKind::CLASS_FW_DECL_LIST:
            if (classNames) {
                auto* list = classNames->getClassFwDeclList();
                if (list) {
                    for (auto* classNameNode : *list) {
                        auto className = classNameNode->getClassName();
                        auto cls = make_unique<ClassInfo>(className);
                        context.addClass(move(cls));
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
    
    for (auto* decl : *externalDeclList) {
        decl->analyzeSemantics(context);
    }
}


//--------------------------------------------------------------ProgramNode--------------------------------------------------------------

void ProgramNode::analyzeSemantics(SemanticContext& context) {
    if (externalDeclList) {
        externalDeclList->analyzeSemantics(context);
    }
}
