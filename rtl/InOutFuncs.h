#pragma once
#include <iostream>
#include <memory>
#include <string>
#include "NSObject.h"
#include "NSString.h"
#include "NSNumber.h"
#include "NSArray.h"

class InOutFuncs : public NSObject {
public:
    // ------------------- Print Methods -------------------
    static void printInt(int value);
    static void printFloat(float value);
    static void printChar(char value);
    static void printNSString(const NSString& str);
    static void printNSNumber(const NSNumber& num);
    static void printNSArray(const NSArray& arr);
    static void printNSObject(const NSObject& obj);

    // ------------------- Read Methods -------------------
    static int readInt();
    static float readFloat();
    static char readChar();
    static std::shared_ptr<NSString> readNSString();
    static std::shared_ptr<NSNumber> readNSNumberInt();
    static std::shared_ptr<NSNumber> readNSNumberFloat();
    static std::shared_ptr<NSArray> readNSArray();
};
