#ifndef BYTECODE_CONTEXT_H
#define BYTECODE_CONTEXT_H

#include <cstdint>
#include <fstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>
#include "context.h"

class BytecodeContext {
public:
    struct Label {
        int position = -1;
        std::vector<int> fixups;
    };

    struct LocalInfo {
        int index = -1;
        Type type = Type(TypeKind::NONE);
    };

    struct MethodBuilder {
        std::string name;
        std::string descriptor;
        uint16_t accessFlags = 0;
        std::vector<uint8_t> code;
        int maxStack = 0;
        int currentStack = 0;
        int maxLocals = 0;
        std::unordered_map<std::string, LocalInfo> locals;
        std::vector<Label*> labels;
    };

    struct FieldBuilder {
        std::string name;
        std::string descriptor;
        uint16_t accessFlags = 0;
    };

    explicit BytecodeContext(const std::string& className, const std::string& outputPath);

    void beginClass(const std::string& className);
    void beginClass(const std::string& className, const std::string& outputPath);
    void setSuperClassName(const std::string& name);
    std::string makeClassOutputPath(const std::string& className) const;
    void endClass();
    void pushClassState();
    void popClassState();

    MethodBuilder* beginMethod(const std::string& name, const std::string& descriptor, uint16_t accessFlags);
    void endMethod();
    void addField(const std::string& name, const std::string& descriptor, uint16_t accessFlags);

    void writeClassFile();

    const std::string& getClassName() const;

    void setCurrentClassName(const std::string& name);
    void setCurrentMethodInfo(MethodInfo* methodInfo, bool isStatic);
    MethodInfo* getCurrentMethodInfo() const;
    bool isCurrentMethodStatic() const;

    int defineLocal(const std::string& name, const Type& type);
    bool hasLocal(const std::string& name) const;
    const LocalInfo* getLocal(const std::string& name) const;

    Label* createLabel();
    void markLabel(Label* label);
    void emitJump(uint8_t opcode, Label* label);

    void emitU1(uint8_t v);
    void emitU2(uint16_t v);
    void emitU4(uint32_t v);

    void emitOpcode(uint8_t opcode);
    void emitIConst(int value);
    void emitFConst(float value);
    void emitAConstNull();
    void emitLdcString(const std::string& value);
    void emitLoad(const Type& type, int index);
    void emitStore(const Type& type, int index);
    void emitDup();
    void emitDupX1();
    void emitDupX2();
    void emitPop();
    void emitBinaryInt(uint8_t opcode);
    void emitBinaryFloat(uint8_t opcode);
    void emitNeg(const Type& type);
    void emitIInc(int index, int amount);
    void emitFcmpl();
    void emitReturn(const Type& type);

    void emitGetField(const std::string& owner, const std::string& name, const std::string& desc);
    void emitPutField(const std::string& owner, const std::string& name, const std::string& desc);
    void emitGetStatic(const std::string& owner, const std::string& name, const std::string& desc);
    void emitPutStatic(const std::string& owner, const std::string& name, const std::string& desc);
    void emitInvokeSpecial(const std::string& owner, const std::string& name, const std::string& desc);
    void emitInvokeVirtual(const std::string& owner, const std::string& name, const std::string& desc);
    void emitInvokeStatic(const std::string& owner, const std::string& name, const std::string& desc);
    void emitNewObject(const std::string& owner);
    void emitCheckCast(const std::string& className);
    void emitANewArray(const std::string& className);
    void emitMultiANewArray(const std::string& descriptor, uint8_t dims);
    void emitAAStore();
    void emitNewArray(TypeKind elementKind);
    void emitArrayLoad(TypeKind elementKind);
    void emitArrayStore(TypeKind elementKind);

private:
    struct CpEntry {
        uint8_t tag = 0;
        std::vector<uint8_t> data;
    };

    struct ClassBuilder {
        std::string name;
        std::vector<FieldBuilder> fields;
        std::vector<MethodBuilder> methods;
    };

    struct ClassState {
        std::string className;
        std::string superClassName;
        std::string outputPath;
        ClassBuilder classBuilder;
        std::vector<CpEntry> constantPool;
    };

    std::string className;
    std::string superClassName;
    std::string outputPath;
    std::string outputDir;
    ClassBuilder currentClass;
    MethodBuilder* currentMethod = nullptr;
    MethodInfo* currentMethodInfo = nullptr;
    bool currentMethodStatic = false;

    std::vector<CpEntry> constantPool;
    std::vector<ClassState> classStack;

    int addUtf8(const std::string& value);
    int addClass(const std::string& className);
    int addString(const std::string& value);
    int addInteger(int value);
    int addFloat(float value);
    int addNameAndType(const std::string& name, const std::string& desc);
    int addFieldRef(const std::string& owner, const std::string& name, const std::string& desc);
    int addMethodRef(const std::string& owner, const std::string& name, const std::string& desc);

    void patchLabel(Label* label);
    void updateStack(int delta);
    int computeDescriptorArgSlots(const std::string& desc) const;
    int computeDescriptorReturnSlots(const std::string& desc) const;
    static TypeKind normalizeToJvmPrimitive(TypeKind kind);
};

#endif
