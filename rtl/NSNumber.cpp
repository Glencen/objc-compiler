#include "NSNumber.h"
#include "context.h"
#include <sstream>
#include <iostream>

// Статический ClassInfo
ClassInfo* NSNumber::s_classInfo = nullptr;

// ------------------- Конструкторы -------------------
NSNumber::NSNumber() : type(NumberType::INT), intValue(0) { }

NSNumber::NSNumber(int value) : type(NumberType::INT), intValue(value) { }

NSNumber::NSNumber(float value) : type(NumberType::FLOAT), floatValue(value) { }

// ------------------- Статические методы -------------------
NSNumber* NSNumber::numberWithIntStatic(int value) {
    return new NSNumber(value);
}

NSNumber* NSNumber::numberWithFloatStatic(float value) {
    return new NSNumber(value);
}

// ------------------- Динамические методы -------------------
int NSNumber::intValueDynamic() {
    if (type == NumberType::INT) return intValue;
    return static_cast<int>(floatValue);
}

float NSNumber::floatValueDynamic() {
    if (type == NumberType::FLOAT) return floatValue;
    return static_cast<float>(intValue);
}

std::string NSNumber::descriptionDynamic() {
    std::ostringstream oss;
    if (type == NumberType::INT) oss << intValue;
    else oss << floatValue;
    return oss.str();
}

NSNumber* NSNumber::initDynamic() {
    NSObject::initDynamic();
    return this;
}

// ------------------- Инициализация ClassInfo -------------------
void initNSNumberClassInfo(ClassInfo* info) {
    NSNumber::s_classInfo = info;
}
