#pragma once
#include <vector>
#include <memory>
#include <string>
#include "NSObject.h"

// Forward declaration
class NSString;

class NSArray : public NSObject {
    friend void initNSArrayClassInfo(ClassInfo* info);

public:
    // ------------------- Конструкторы -------------------
    NSArray();
    NSArray(const NSArray& other);
    NSArray(const std::vector<std::shared_ptr<NSObject>>& objects);

    // ------------------- Статические методы -------------------
    static std::shared_ptr<NSArray> arrayStatic();
    static std::shared_ptr<NSArray> arrayWithArrayStatic(const NSArray& array);
    static std::shared_ptr<NSArray> arrayWithObjectsStatic(const std::vector<std::shared_ptr<NSObject>>& objects);
    static std::shared_ptr<NSArray> arrayWithObjectStatic(std::shared_ptr<NSObject> object);

    // ------------------- Динамические методы -------------------
    std::shared_ptr<NSArray> arrayByAddingObjectDynamic(std::shared_ptr<NSObject> object);
    std::shared_ptr<NSArray> arrayByAddingObjectsFromArrayDynamic(const NSArray& anotherArray);
    std::shared_ptr<NSObject> objectAtIndexDynamic(int index) const;
    int countDynamic() const;
    int containsObjectDynamic(std::shared_ptr<NSObject> object) const;
    std::shared_ptr<NSString> componentsJoinedByStringDynamic(const NSString& separator);
    std::shared_ptr<NSObject> firstObjectDynamic() const;
    std::shared_ptr<NSObject> lastObjectDynamic() const;
    std::shared_ptr<NSObject> firstObjectCommonWithArrayDynamic(const NSArray& otherArray);
    int indexOfObjectDynamic(std::shared_ptr<NSObject> object) const;
    int isEqualToArrayDynamic(const NSArray& otherArray) const;
    NSArray* initDynamic() override;
    std::string descriptionDynamic() const override;

protected:
    static ClassInfo* s_classInfo;
    std::vector<std::shared_ptr<NSObject>> array;
};

// ------------------- Инициализация ClassInfo -------------------
void initNSArrayClassInfo(ClassInfo* info);
