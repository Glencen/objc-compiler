#pragma once
#include <string>
#include "NSObject.h"

class NSNumber : public NSObject {
    friend void initNSNumberClassInfo(ClassInfo* info);

public:
    // ------------------- Конструкторы -------------------
    NSNumber();                       // по умолчанию
    NSNumber(int value);
    NSNumber(float value);

    // ------------------- Статические методы -------------------
    static NSNumber* numberWithIntStatic(int value);
    static NSNumber* numberWithFloatStatic(float value);

    // ------------------- Динамические методы -------------------
    int intValueDynamic();
    float floatValueDynamic();
    std::string descriptionDynamic() override;

    NSNumber* initDynamic() override;

protected:
    static ClassInfo* s_classInfo;
    enum class NumberType { INT, FLOAT } type;
    union {
        int intValue;
        float floatValue;
    };
};

// ------------------- Инициализация ClassInfo -------------------
void initNSNumberClassInfo(ClassInfo* info);
