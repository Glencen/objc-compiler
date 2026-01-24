#include "NSArray.h"
#include "NSString.h"
#include "context.h"
#include <algorithm>
#include <iostream>

// Статический ClassInfo
ClassInfo* NSArray::s_classInfo = nullptr;

// ------------------- Конструкторы -------------------
NSArray::NSArray() : array() { }

NSArray::NSArray(const NSArray& other) : array(other.array) { }

NSArray::NSArray(const std::vector<std::shared_ptr<NSObject>>& objects) : array(objects) { }

// ------------------- Статические методы -------------------
std::shared_ptr<NSArray> NSArray::arrayStatic() {
    return std::make_shared<NSArray>();
}

std::shared_ptr<NSArray> NSArray::arrayWithArrayStatic(const NSArray& array) {
    return std::make_shared<NSArray>(array);
}

std::shared_ptr<NSArray> NSArray::arrayWithObjectsStatic(const std::vector<std::shared_ptr<NSObject>>& objects) {
    return std::make_shared<NSArray>(objects);
}

std::shared_ptr<NSArray> NSArray::arrayWithObjectStatic(std::shared_ptr<NSObject> object) {
    return std::make_shared<NSArray>(std::vector<std::shared_ptr<NSObject>>{object});
}

// ------------------- Динамические методы -------------------
std::shared_ptr<NSArray> NSArray::arrayByAddingObjectDynamic(std::shared_ptr<NSObject> object) {
    auto res = array;
    res.push_back(object);
    return std::make_shared<NSArray>(res);
}

std::shared_ptr<NSArray> NSArray::arrayByAddingObjectsFromArrayDynamic(const NSArray& anotherArray) {
    auto res = array;
    res.insert(res.end(), anotherArray.array.begin(), anotherArray.array.end());
    return std::make_shared<NSArray>(res);
}

std::shared_ptr<NSObject> NSArray::objectAtIndexDynamic(int index) {
    if (index < 0 || index >= static_cast<int>(array.size())) return nullptr;
    return array[index];
}

int NSArray::countDynamic() const {
    return static_cast<int>(array.size());
}

int NSArray::containsObjectDynamic(std::shared_ptr<NSObject> object) {
    return std::any_of(array.begin(), array.end(),
                       [&](auto& o) { return o == object; }) ? 1 : 0;
}

std::shared_ptr<NSString> NSArray::componentsJoinedByStringDynamic(const NSString& separator) {
    std::string sep = separator.cStringDynamic().get();
    std::string res;
    for (size_t i = 0; i < array.size(); ++i) {
        auto desc = array[i]->descriptionDynamic();
        res += desc;
        if (i != array.size() - 1) res += sep;
    }
    return NSString::stringWithCStringStatic(res.c_str());
}

std::shared_ptr<NSObject> NSArray::firstObjectDynamic() {
    return array.empty() ? nullptr : array[0];
}

std::shared_ptr<NSObject> NSArray::lastObjectDynamic() {
    return array.empty() ? nullptr : array.back();
}

std::shared_ptr<NSObject> NSArray::firstObjectCommonWithArrayDynamic(const NSArray& otherArray) {
    for (auto& o : otherArray.array) {
        if (containsObjectDynamic(o)) return o;
    }
    return nullptr;
}

int NSArray::indexOfObjectDynamic(std::shared_ptr<NSObject> object) {
    for (size_t i = 0; i < array.size(); ++i) {
        if (array[i] == object) return static_cast<int>(i);
    }
    return -1; // NSNotFound
}

int NSArray::isEqualToArrayDynamic(const NSArray& otherArray) {
    if (array.size() != otherArray.array.size()) return 0;
    for (size_t i = 0; i < array.size(); ++i) {
        if (array[i]->isEqualDynamic(otherArray.array[i].get()) == 0) return 0;
    }
    return 1;
}

NSArray* NSArray::initDynamic() {
    NSObject::initDynamic();
    return this;
}

std::string NSArray::descriptionDynamic() {
    return "NSArray instance";
}

// ------------------- Инициализация ClassInfo -------------------
void initNSArrayClassInfo(ClassInfo* info) {
    NSArray::s_classInfo = info;
}
