#include "NSObject.h"
#include "ClassInfo.h"
#include <iostream>

// Статический указатель на ClassInfo для NSObject
ClassInfo* NSObject::s_classInfo = nullptr;

NSObject::NSObject() {
    // Конструктор по умолчанию
}

NSObject::~NSObject() {
    // Деструктор
}

// Статический аллокатор
std::unique_ptr<NSObject> NSObject::allocStatic() {
    auto obj = std::make_unique<NSObject>();
    return obj;
}

// Инициализация динамического объекта
NSObject* NSObject::initDynamic() {
    return this;
}

// Новый объект (создание + init)
std::unique_ptr<NSObject> NSObject::newStatic() {
    return allocStatic()->initDynamic();
}

// Динамический класс объекта
ClassInfo* NSObject::getClassDynamic() {
    return s_classInfo;
}

// Статический класс объекта
ClassInfo* NSObject::getClassStatic() {
    return s_classInfo;
}

// Проверка наследования
int NSObject::isSubclassOfClassStatic(ClassInfo* cls) {
    if (!s_classInfo) return 0;
    return s_classInfo->isSubclassOf(cls) ? 1 : 0;
}

// Имя класса
std::string NSObject::classNameDynamic() {
    return s_classInfo ? s_classInfo->name : "NSObject";
}

// Суперкласс
ClassInfo* NSObject::superclassDynamic() {
    return s_classInfo ? s_classInfo->superclass : nullptr;
}

// Описание объекта
std::string NSObject::descriptionDynamic() {
    return "<NSObject>";
}

// Сравнение объектов
int NSObject::isEqualDynamic(NSObject* other) {
    return this == other ? 1 : 0;
}

// Инициализация ClassInfo (может вызываться из SemanticContext)
void initNSObjectClassInfo(ClassInfo* info) {
    NSObject::s_classInfo = info;
}
