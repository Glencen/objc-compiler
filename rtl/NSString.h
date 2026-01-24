#pragma once
#include "NSObject.h"
#include <string>
#include <memory>

class NSString : public NSObject {
public:
    // ------------------- Конструкторы -------------------
    NSString();
    NSString(const std::string& str);
    NSString(const NSString& other);

    // ------------------- Статические методы -------------------
    static std::unique_ptr<NSString> stringStatic();
    static std::unique_ptr<NSString> stringWithCStringStatic(const char* cstr);
    static std::unique_ptr<NSString> stringWithStringStatic(const NSString& str);

    // ------------------- Динамические методы -------------------
    std::unique_ptr<char[]> cStringDynamic();
    std::unique_ptr<NSString> capitalizeStringDynamic();
    char characterAtIndexDynamic(int index);
    int hasPrefixDynamic(const NSString& prefix);
    int hasSuffixDynamic(const NSString& suffix);
    std::unique_ptr<NSString> init();
    int intValueDynamic();
    int isEqualDynamic(NSObject* other);
    int isEqualToStringDynamic(const NSString& other);
    int lengthDynamic();
    std::unique_ptr<NSString> lowercaseStringDynamic();
    std::unique_ptr<NSString> uppercaseStringDynamic();
    std::unique_ptr<NSString> stringByAppendingStringDynamic(const NSString& other);

    // ⚡ Виртуальный метод базового класса
    std::string descriptionDynamic() override;

    // ------------------- Инициализация ClassInfo -------------------
    static void initClassInfo(ClassInfo* info);

protected:
    std::string string;
    static ClassInfo* s_classInfo;
};
