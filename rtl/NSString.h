#pragma once
#include "NSObject.h"
#include <string>
#include <memory>

class NSString : public NSObject {
public:
    // Конструкторы
    NSString();                        // пустая строка
    explicit NSString(const std::string& str); // строка из std::string
    NSString(const NSString& other);   // копия другой строки

    virtual ~NSString() = default;

    // ===============================
    // Статические методы
    // ===============================
    static std::unique_ptr<NSString> stringStatic();
    static std::unique_ptr<NSString> stringWithCStringStatic(const char* cstr);
    static std::unique_ptr<NSString> stringWithStringStatic(const NSString& str);

    // ===============================
    // Динамические методы
    // ===============================
    virtual std::unique_ptr<char[]> cStringDynamic();
    virtual std::unique_ptr<NSString> capitalizeStringDynamic();
    virtual char characterAtIndexDynamic(int index);
    virtual int hasPrefixDynamic(const NSString& prefix);
    virtual int hasSuffixDynamic(const NSString& suffix);
    virtual std::unique_ptr<NSString> init();
    virtual int intValueDynamic();
    virtual int isEqualDynamic(NSObject* other);
    virtual int isEqualToStringDynamic(const NSString& other);
    virtual int lengthDynamic();
    virtual std::unique_ptr<NSString> lowercaseStringDynamic();
    virtual std::unique_ptr<NSString> uppercaseStringDynamic();
    virtual std::unique_ptr<NSString> stringByAppendingStringDynamic(const NSString& other);
    virtual std::unique_ptr<NSString> descriptionDynamic();

protected:
    std::string string;

public:
    // Ссылка на ClassInfo для NSString
    static ClassInfo* s_classInfo;
};
