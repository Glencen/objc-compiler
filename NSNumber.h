#pragma once
#include <string>
#include "NSString.h"
#include "NSObject.h"

class NSNumber : public NSObject {
private:
    enum class Type { INT, FLOAT } type;
    union {
        int iValue;
        float fValue;
    };

public:
    NSNumber(int v) : type(Type::INT), iValue(v) {}
    NSNumber(float v) : type(Type::FLOAT), fValue(v) {}
    ~NSNumber() = default;

    int intValueDynamic() const {
        return type == Type::INT ? iValue : static_cast<int>(fValue);
    }

    float floatValueDynamic() const {
        return type == Type::FLOAT ? fValue : static_cast<float>(iValue);
    }

    NSString* descriptionDynamic() override {
        if (type == Type::INT) {
            return NSString::stringWithCStringStatic(std::to_string(iValue).c_str());
        } else {
            return NSString::stringWithCStringStatic(std::to_string(fValue).c_str());
        }
    }

    // Можно добавить numberWithIntStatic / numberWithFloatStatic
    static NSNumber* numberWithIntStatic(int v) { return new NSNumber(v); }
    static NSNumber* numberWithFloatStatic(float v) { return new NSNumber(v); }
};
