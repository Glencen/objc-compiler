#pragma once
#include <string>
#include <memory>

// Forward declaration класса ClassInfo, чтобы не включать context.h
class ClassInfo;

class NSObject {
    friend void initNSObjectClassInfo(ClassInfo* info);
public:
    NSObject();
    virtual ~NSObject();

    // Статические методы создания объекта
    static std::unique_ptr<NSObject> allocStatic();
    virtual NSObject* initDynamic();
    static std::unique_ptr<NSObject> newStatic();

    // Методы работы с классом
    virtual ClassInfo* getClassDynamic();
    static ClassInfo* getClassStatic();

    static int isSubclassOfClassStatic(ClassInfo* cls);

    // Информационные методы
    virtual std::string classNameDynamic();
    virtual ClassInfo* superclassDynamic();
    virtual std::string descriptionDynamic();

    virtual int isEqualDynamic(NSObject* other);

protected:
    static ClassInfo* s_classInfo;
};

// Функция для связывания с ClassInfo (вызывается из context.cpp)
void initNSObjectClassInfo(ClassInfo* info);
