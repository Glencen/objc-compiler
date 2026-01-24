#include "NSObject.h"
#include "context.h" // <-- здесь можно подключить, чтобы видеть ClassInfo
#include <iostream>

// Статический указатель на ClassInfo для NSObject
ClassInfo* NSObject::s_classInfo = nullptr;

NSObject::NSObject() { }
NSObject::~NSObject() { }

std::unique_ptr<NSObject> NSObject::allocStatic() {
    return std::make_unique<NSObject>();
}

NSObject* NSObject::initDynamic() {
    return this;
}

std::unique_ptr<NSObject> NSObject::newStatic() {
    auto obj = allocStatic();      // unique_ptr<NSObject>
    obj->initDynamic();            // вызываем initDynamic(), возвращаем тот же объект
    return obj;                    // возвращаем unique_ptr
}

ClassInfo* NSObject::getClassDynamic() {
    return s_classInfo;
}

ClassInfo* NSObject::getClassStatic() {
    return s_classInfo;
}

int NSObject::isSubclassOfClassStatic(ClassInfo* cls) {
    return s_classInfo ? s_classInfo->isSubclassOf(cls) : 0;
}

std::string NSObject::classNameDynamic() {
    return s_classInfo ? s_classInfo->name : "NSObject";
}

ClassInfo* NSObject::superclassDynamic() {
    return s_classInfo ? s_classInfo->superclass : nullptr;
}

std::string NSObject::descriptionDynamic() {
    return "<NSObject>";
}

int NSObject::isEqualDynamic(NSObject* other) {
    return this == other ? 1 : 0;
}

void initNSObjectClassInfo(ClassInfo* info) {
    NSObject::s_classInfo = info;
}
