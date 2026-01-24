#include "InOutFuncs.h"
#include <sstream>
#include <vector>

// ------------------- Print Methods -------------------

void InOutFuncs::printInt(int value) {
    std::cout << value;
}

void InOutFuncs::printFloat(float value) {
    std::cout << value;
}

void InOutFuncs::printChar(char value) {
    std::cout << value;
}

void InOutFuncs::printNSString(const NSString& str) {
    std::cout << str.cStringDynamic().get();
}

void InOutFuncs::printNSNumber(const NSNumber& num) {
    std::cout << num.descriptionDynamic();
}

void InOutFuncs::printNSArray(const NSArray& arr) {
    std::cout << "[";
    for (int i = 0; i < arr.countDynamic(); ++i) {
        auto obj = arr.objectAtIndexDynamic(i);
        if (obj) {
            std::cout << obj->descriptionDynamic();
            if (i != arr.countDynamic() - 1) std::cout << ", ";
        }
    }
    std::cout << "]";
}

void InOutFuncs::printNSObject(const NSObject& obj) {
    std::cout << obj.descriptionDynamic();
}

// ------------------- Read Methods -------------------

int InOutFuncs::readInt() {
    int value;
    std::cin >> value;
    return value;
}

float InOutFuncs::readFloat() {
    float value;
    std::cin >> value;
    return value;
}

char InOutFuncs::readChar() {
    char value;
    std::cin >> value;
    return value;
}

std::shared_ptr<NSString> InOutFuncs::readNSString() {
    std::string s;
    std::cin >> s;
    return NSString::stringWithCStringStatic(s.c_str());
}

std::shared_ptr<NSNumber> InOutFuncs::readNSNumberInt() {
    int value;
    std::cin >> value;
    return NSNumber::numberWithIntStatic(value);
}

std::shared_ptr<NSNumber> InOutFuncs::readNSNumberFloat() {
    float value;
    std::cin >> value;
    return NSNumber::numberWithFloatStatic(value);
}

std::shared_ptr<NSArray> InOutFuncs::readNSArray() {
    int n;
    std::cin >> n;
    std::vector<std::shared_ptr<NSObject>> items;
    for (int i = 0; i < n; ++i) {
        std::string s;
        std::cin >> s;
        items.push_back(NSString::stringWithCStringStatic(s.c_str()));
    }
    return NSArray::arrayWithObjectsStatic(items);
}
