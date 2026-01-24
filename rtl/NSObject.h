#pragma once
#include <string>
#include <memory>

class ClassInfo;

class NSObject {
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
    // Ссылка на ClassInfo для этого класса
    static ClassInfo* s_classInfo;
};
