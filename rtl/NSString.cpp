#include "NSString.h"
#include <algorithm>
#include <cctype>
#include <cstring>
#include <iostream>

// Статический ClassInfo
ClassInfo* NSString::s_classInfo = nullptr;

// ------------------- Конструкторы -------------------
NSString::NSString() : string("") {}
NSString::NSString(const std::string& str) : string(str) {}
NSString::NSString(const NSString& other) : string(other.string) {}

// ------------------- Статические методы -------------------
std::unique_ptr<NSString> NSString::stringStatic() {
    return std::make_unique<NSString>("");
}

std::unique_ptr<NSString> NSString::stringWithCStringStatic(const char* cstr) {
    return std::make_unique<NSString>(std::string(cstr));
}

std::unique_ptr<NSString> NSString::stringWithStringStatic(const NSString& str) {
    return std::make_unique<NSString>(str);
}

// ------------------- Динамические методы -------------------
std::unique_ptr<char[]> NSString::cStringDynamic() const {
    auto res = std::make_unique<char[]>(string.size() + 1);
    for (size_t i = 0; i < string.size(); ++i) {
        res[i] = string[i];
    }
    res[string.size()] = '\0';
    return res;
}


std::unique_ptr<NSString> NSString::capitalizeStringDynamic() {
    std::string res = string;
    bool capitalizeNext = true;
    for (char& c : res) {
        if (isspace(static_cast<unsigned char>(c))) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            c = static_cast<char>(std::toupper(c));
            capitalizeNext = false;
        }
    }
    return std::make_unique<NSString>(res);
}

char NSString::characterAtIndexDynamic(int index) {
    return (index >= 0 && index < static_cast<int>(string.size())) ? string[index] : '\0';
}

int NSString::hasPrefixDynamic(const NSString& prefix) {
    return string.rfind(prefix.string, 0) == 0 ? 1 : 0;
}

int NSString::hasSuffixDynamic(const NSString& suffix) {
    if (suffix.string.size() > string.size()) return 0;
    return string.compare(string.size() - suffix.string.size(), suffix.string.size(), suffix.string) == 0 ? 1 : 0;
}

std::unique_ptr<NSString> NSString::init() {
    NSObject::initDynamic();
    return std::make_unique<NSString>(*this);
}

int NSString::intValueDynamic() {
    return std::stoi(string);
}

int NSString::isEqualDynamic(NSObject* other) {
    auto strObj = dynamic_cast<NSString*>(other);
    if (!strObj) return 0;
    return string == strObj->string ? 1 : 0;
}

int NSString::isEqualToStringDynamic(const NSString& other) {
    return string == other.string ? 1 : 0;
}

int NSString::lengthDynamic() {
    return static_cast<int>(string.size());
}

std::unique_ptr<NSString> NSString::lowercaseStringDynamic() {
    std::string res = string;
    std::transform(res.begin(), res.end(), res.begin(), ::tolower);
    return std::make_unique<NSString>(res);
}

std::unique_ptr<NSString> NSString::uppercaseStringDynamic() {
    std::string res = string;
    std::transform(res.begin(), res.end(), res.begin(), ::toupper);
    return std::make_unique<NSString>(res);
}

std::unique_ptr<NSString> NSString::stringByAppendingStringDynamic(const NSString& other) {
    return std::make_unique<NSString>(string + other.string);
}

// ⚡ Виртуальный метод базового класса NSObject
std::string NSString::descriptionDynamic() {
    return string;
}

// ------------------- Инициализация ClassInfo -------------------
void NSString::initClassInfo(ClassInfo* info) {
    s_classInfo = info;
}
