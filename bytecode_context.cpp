#include "bytecode_context.h"
#include <cstring>
#include <filesystem>

namespace {
constexpr uint16_t ACC_PUBLIC = 0x0001;
constexpr uint16_t ACC_PRIVATE = 0x0002;
constexpr uint16_t ACC_PROTECTED = 0x0004;
constexpr uint16_t ACC_STATIC = 0x0008;
constexpr uint16_t ACC_SUPER = 0x0020;

constexpr uint8_t OP_NOP = 0x00;
constexpr uint8_t OP_ACONST_NULL = 0x01;
constexpr uint8_t OP_ICONST_M1 = 0x02;
constexpr uint8_t OP_ICONST_0 = 0x03;
constexpr uint8_t OP_ICONST_1 = 0x04;
constexpr uint8_t OP_ICONST_2 = 0x05;
constexpr uint8_t OP_ICONST_3 = 0x06;
constexpr uint8_t OP_ICONST_4 = 0x07;
constexpr uint8_t OP_ICONST_5 = 0x08;
constexpr uint8_t OP_FCONST_0 = 0x0b;
constexpr uint8_t OP_FCONST_1 = 0x0c;
constexpr uint8_t OP_FCONST_2 = 0x0d;
constexpr uint8_t OP_BIPUSH = 0x10;
constexpr uint8_t OP_SIPUSH = 0x11;
constexpr uint8_t OP_LDC = 0x12;
constexpr uint8_t OP_LDC_W = 0x13;

constexpr uint8_t OP_ILOAD = 0x15;
constexpr uint8_t OP_FLOAD = 0x17;
constexpr uint8_t OP_ALOAD = 0x19;
constexpr uint8_t OP_ILOAD_0 = 0x1a;
constexpr uint8_t OP_ILOAD_1 = 0x1b;
constexpr uint8_t OP_ILOAD_2 = 0x1c;
constexpr uint8_t OP_ILOAD_3 = 0x1d;
constexpr uint8_t OP_FLOAD_0 = 0x22;
constexpr uint8_t OP_FLOAD_1 = 0x23;
constexpr uint8_t OP_FLOAD_2 = 0x24;
constexpr uint8_t OP_FLOAD_3 = 0x25;
constexpr uint8_t OP_ALOAD_0 = 0x2a;
constexpr uint8_t OP_ALOAD_1 = 0x2b;
constexpr uint8_t OP_ALOAD_2 = 0x2c;
constexpr uint8_t OP_ALOAD_3 = 0x2d;

constexpr uint8_t OP_ISTORE = 0x36;
constexpr uint8_t OP_FSTORE = 0x38;
constexpr uint8_t OP_ASTORE = 0x3a;
constexpr uint8_t OP_ISTORE_0 = 0x3b;
constexpr uint8_t OP_ISTORE_1 = 0x3c;
constexpr uint8_t OP_ISTORE_2 = 0x3d;
constexpr uint8_t OP_ISTORE_3 = 0x3e;
constexpr uint8_t OP_FSTORE_0 = 0x43;
constexpr uint8_t OP_FSTORE_1 = 0x44;
constexpr uint8_t OP_FSTORE_2 = 0x45;
constexpr uint8_t OP_FSTORE_3 = 0x46;
constexpr uint8_t OP_ASTORE_0 = 0x4b;
constexpr uint8_t OP_ASTORE_1 = 0x4c;
constexpr uint8_t OP_ASTORE_2 = 0x4d;
constexpr uint8_t OP_ASTORE_3 = 0x4e;
constexpr uint8_t OP_AASTORE = 0x53;
constexpr uint8_t OP_IASTORE = 0x4f;
constexpr uint8_t OP_FASTORE = 0x51;
constexpr uint8_t OP_IALOAD = 0x2e;
constexpr uint8_t OP_FALOAD = 0x30;
constexpr uint8_t OP_AALOAD = 0x32;

constexpr uint8_t OP_DUP = 0x59;
constexpr uint8_t OP_DUP_X1 = 0x5a;
constexpr uint8_t OP_DUP_X2 = 0x5b;
constexpr uint8_t OP_POP = 0x57;

constexpr uint8_t OP_IADD = 0x60;
constexpr uint8_t OP_FADD = 0x62;
constexpr uint8_t OP_ISUB = 0x64;
constexpr uint8_t OP_FSUB = 0x66;
constexpr uint8_t OP_IMUL = 0x68;
constexpr uint8_t OP_FMUL = 0x6a;
constexpr uint8_t OP_IDIV = 0x6c;
constexpr uint8_t OP_FDIV = 0x6e;
constexpr uint8_t OP_INEG = 0x74;
constexpr uint8_t OP_FNEG = 0x76;

constexpr uint8_t OP_IINC = 0x84;

constexpr uint8_t OP_FCMPL = 0x95;

constexpr uint8_t OP_IFEQ = 0x99;
constexpr uint8_t OP_IFNE = 0x9a;
constexpr uint8_t OP_IFLT = 0x9b;
constexpr uint8_t OP_IFGE = 0x9c;
constexpr uint8_t OP_IFGT = 0x9d;
constexpr uint8_t OP_IFLE = 0x9e;
constexpr uint8_t OP_IF_ICMPEQ = 0x9f;
constexpr uint8_t OP_IF_ICMPNE = 0xa0;
constexpr uint8_t OP_IF_ICMPLT = 0xa1;
constexpr uint8_t OP_IF_ICMPGE = 0xa2;
constexpr uint8_t OP_IF_ICMPGT = 0xa3;
constexpr uint8_t OP_IF_ICMPLE = 0xa4;
constexpr uint8_t OP_GOTO = 0xa7;

constexpr uint8_t OP_GETSTATIC = 0xb2;
constexpr uint8_t OP_PUTSTATIC = 0xb3;
constexpr uint8_t OP_GETFIELD = 0xb4;
constexpr uint8_t OP_PUTFIELD = 0xb5;
constexpr uint8_t OP_INVOKEVIRTUAL = 0xb6;
constexpr uint8_t OP_INVOKESPECIAL = 0xb7;
constexpr uint8_t OP_INVOKESTATIC = 0xb8;
constexpr uint8_t OP_NEW = 0xbb;
constexpr uint8_t OP_NEWARRAY = 0xbc;
constexpr uint8_t OP_ANEWARRAY = 0xbd;

constexpr uint8_t OP_IRETURN = 0xac;
constexpr uint8_t OP_FRETURN = 0xae;
constexpr uint8_t OP_ARETURN = 0xb0;
constexpr uint8_t OP_RETURN = 0xb1;
} // namespace

BytecodeContext::BytecodeContext(const std::string& className, const std::string& outputPath)
    : className(className), superClassName("java/lang/Object"), outputPath(outputPath) {
    outputDir = std::filesystem::path(outputPath).parent_path().string();
    if (outputDir.empty()) {
        outputDir = ".";
    }
    beginClass(className);
}

void BytecodeContext::beginClass(const std::string& name) {
    currentClass.name = name;
    className = name;
    superClassName = "java/lang/Object";
    currentClass.fields.clear();
    currentClass.methods.clear();
    currentMethod = nullptr;
    constantPool.clear();
}

void BytecodeContext::beginClass(const std::string& name, const std::string& outPath) {
    outputPath = outPath;
    outputDir = std::filesystem::path(outputPath).parent_path().string();
    if (outputDir.empty()) {
        outputDir = ".";
    }
    beginClass(name);
}

void BytecodeContext::setSuperClassName(const std::string& name) {
    superClassName = name;
}

std::string BytecodeContext::makeClassOutputPath(const std::string& name) const {
    std::filesystem::path dir(outputDir.empty() ? "." : outputDir);
    return (dir / (name + ".class")).string();
}

void BytecodeContext::endClass() {
    writeClassFile();
}

BytecodeContext::MethodBuilder* BytecodeContext::beginMethod(
    const std::string& name,
    const std::string& descriptor,
    uint16_t accessFlags) {
    currentClass.methods.push_back(MethodBuilder{});
    MethodBuilder& method = currentClass.methods.back();
    method.name = name;
    method.descriptor = descriptor;
    method.accessFlags = accessFlags;
    method.maxLocals = (accessFlags & ACC_STATIC) ? 0 : 1;
    method.maxStack = 0;
    method.currentStack = 0;
    currentMethod = &method;
    return currentMethod;
}

void BytecodeContext::endMethod() {
    if (!currentMethod) {
        return;
    }
    for (Label* label : currentMethod->labels) {
        patchLabel(label);
    }
    currentMethod = nullptr;
}

void BytecodeContext::addField(const std::string& name, const std::string& descriptor, uint16_t accessFlags) {
    currentClass.fields.push_back(FieldBuilder{name, descriptor, accessFlags});
}

const std::string& BytecodeContext::getClassName() const {
    return className;
}

void BytecodeContext::setCurrentClassName(const std::string& name) {
    className = name;
}

void BytecodeContext::setCurrentMethodInfo(MethodInfo* methodInfo, bool isStatic) {
    currentMethodInfo = methodInfo;
    currentMethodStatic = isStatic;
}

MethodInfo* BytecodeContext::getCurrentMethodInfo() const {
    return currentMethodInfo;
}

bool BytecodeContext::isCurrentMethodStatic() const {
    return currentMethodStatic;
}

int BytecodeContext::defineLocal(const std::string& name, const Type& type) {
    if (!currentMethod) {
        return -1;
    }
    auto it = currentMethod->locals.find(name);
    if (it != currentMethod->locals.end()) {
        return it->second.index;
    }
    int index = currentMethod->maxLocals;
    currentMethod->locals[name] = LocalInfo{index, type};
    currentMethod->maxLocals += 1;
    return index;
}

bool BytecodeContext::hasLocal(const std::string& name) const {
    if (!currentMethod) {
        return false;
    }
    return currentMethod->locals.find(name) != currentMethod->locals.end();
}

const BytecodeContext::LocalInfo* BytecodeContext::getLocal(const std::string& name) const {
    if (!currentMethod) {
        return nullptr;
    }
    auto it = currentMethod->locals.find(name);
    if (it == currentMethod->locals.end()) {
        return nullptr;
    }
    return &it->second;
}

BytecodeContext::Label* BytecodeContext::createLabel() {
    if (!currentMethod) {
        return nullptr;
    }
    auto* label = new Label();
    currentMethod->labels.push_back(label);
    return label;
}

void BytecodeContext::markLabel(Label* label) {
    if (!currentMethod || !label) {
        return;
    }
    label->position = static_cast<int>(currentMethod->code.size());
    patchLabel(label);
}

void BytecodeContext::emitJump(uint8_t opcode, Label* label) {
    if (!currentMethod) {
        return;
    }
    int insnPos = static_cast<int>(currentMethod->code.size());
    emitOpcode(opcode);
    emitU2(0);
    if (label) {
        label->fixups.push_back(insnPos);
    }
    switch (opcode) {
        case OP_IFEQ:
        case OP_IFNE:
        case OP_IFLT:
        case OP_IFGE:
        case OP_IFGT:
        case OP_IFLE:
            updateStack(-1);
            break;
        case OP_IF_ICMPEQ:
        case OP_IF_ICMPNE:
        case OP_IF_ICMPLT:
        case OP_IF_ICMPGE:
        case OP_IF_ICMPGT:
        case OP_IF_ICMPLE:
            updateStack(-2);
            break;
        default:
            break;
    }
}

void BytecodeContext::emitU1(uint8_t v) {
    if (!currentMethod) {
        return;
    }
    currentMethod->code.push_back(v);
}

void BytecodeContext::emitU2(uint16_t v) {
    if (!currentMethod) {
        return;
    }
    currentMethod->code.push_back(static_cast<uint8_t>((v >> 8) & 0xff));
    currentMethod->code.push_back(static_cast<uint8_t>(v & 0xff));
}

void BytecodeContext::emitU4(uint32_t v) {
    if (!currentMethod) {
        return;
    }
    currentMethod->code.push_back(static_cast<uint8_t>((v >> 24) & 0xff));
    currentMethod->code.push_back(static_cast<uint8_t>((v >> 16) & 0xff));
    currentMethod->code.push_back(static_cast<uint8_t>((v >> 8) & 0xff));
    currentMethod->code.push_back(static_cast<uint8_t>(v & 0xff));
}

void BytecodeContext::emitOpcode(uint8_t opcode) {
    emitU1(opcode);
}

void BytecodeContext::emitIConst(int value) {
    if (!currentMethod) {
        return;
    }
    if (value >= -1 && value <= 5) {
        emitOpcode(static_cast<uint8_t>(OP_ICONST_0 + value));
        updateStack(1);
        return;
    }
    if (value >= -128 && value <= 127) {
        emitOpcode(OP_BIPUSH);
        emitU1(static_cast<uint8_t>(value & 0xff));
        updateStack(1);
        return;
    }
    if (value >= -32768 && value <= 32767) {
        emitOpcode(OP_SIPUSH);
        emitU2(static_cast<uint16_t>(value & 0xffff));
        updateStack(1);
        return;
    }
    int idx = addInteger(value);
    if (idx <= 0xff) {
        emitOpcode(OP_LDC);
        emitU1(static_cast<uint8_t>(idx));
    } else {
        emitOpcode(OP_LDC_W);
        emitU2(static_cast<uint16_t>(idx));
    }
    updateStack(1);
}

void BytecodeContext::emitFConst(float value) {
    if (!currentMethod) {
        return;
    }
    if (value == 0.0f) {
        emitOpcode(OP_FCONST_0);
        updateStack(1);
        return;
    }
    if (value == 1.0f) {
        emitOpcode(OP_FCONST_1);
        updateStack(1);
        return;
    }
    if (value == 2.0f) {
        emitOpcode(OP_FCONST_2);
        updateStack(1);
        return;
    }
    int idx = addFloat(value);
    if (idx <= 0xff) {
        emitOpcode(OP_LDC);
        emitU1(static_cast<uint8_t>(idx));
    } else {
        emitOpcode(OP_LDC_W);
        emitU2(static_cast<uint16_t>(idx));
    }
    updateStack(1);
}

void BytecodeContext::emitAConstNull() {
    emitOpcode(OP_ACONST_NULL);
    updateStack(1);
}

void BytecodeContext::emitLdcString(const std::string& value) {
    int idx = addString(value);
    if (idx <= 0xff) {
        emitOpcode(OP_LDC);
        emitU1(static_cast<uint8_t>(idx));
    } else {
        emitOpcode(OP_LDC_W);
        emitU2(static_cast<uint16_t>(idx));
    }
    updateStack(1);
}

void BytecodeContext::emitLoad(const Type& type, int index) {
    TypeKind kind = normalizeToJvmPrimitive(type.dataType);
    if (type.isArray()) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_ALOAD_0 + index));
        } else {
            emitOpcode(OP_ALOAD);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(1);
        return;
    }
    if (kind == TypeKind::FLOAT) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_FLOAD_0 + index));
        } else {
            emitOpcode(OP_FLOAD);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(1);
        return;
    }
    if (kind == TypeKind::INT || kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_ILOAD_0 + index));
        } else {
            emitOpcode(OP_ILOAD);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(1);
        return;
    }
    if (index <= 3) {
        emitOpcode(static_cast<uint8_t>(OP_ALOAD_0 + index));
    } else {
        emitOpcode(OP_ALOAD);
        emitU1(static_cast<uint8_t>(index));
    }
    updateStack(1);
}

void BytecodeContext::emitStore(const Type& type, int index) {
    TypeKind kind = normalizeToJvmPrimitive(type.dataType);
    if (type.isArray()) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_ASTORE_0 + index));
        } else {
            emitOpcode(OP_ASTORE);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(-1);
        return;
    }
    if (kind == TypeKind::FLOAT) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_FSTORE_0 + index));
        } else {
            emitOpcode(OP_FSTORE);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(-1);
        return;
    }
    if (kind == TypeKind::INT || kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        if (index <= 3) {
            emitOpcode(static_cast<uint8_t>(OP_ISTORE_0 + index));
        } else {
            emitOpcode(OP_ISTORE);
            emitU1(static_cast<uint8_t>(index));
        }
        updateStack(-1);
        return;
    }
    if (index <= 3) {
        emitOpcode(static_cast<uint8_t>(OP_ASTORE_0 + index));
    } else {
        emitOpcode(OP_ASTORE);
        emitU1(static_cast<uint8_t>(index));
    }
    updateStack(-1);
}

void BytecodeContext::emitDup() {
    emitOpcode(OP_DUP);
    updateStack(1);
}

void BytecodeContext::emitDupX1() {
    emitOpcode(OP_DUP_X1);
    updateStack(1);
}

void BytecodeContext::emitDupX2() {
    emitOpcode(OP_DUP_X2);
    updateStack(1);
}

void BytecodeContext::emitPop() {
    emitOpcode(OP_POP);
    updateStack(-1);
}

void BytecodeContext::emitBinaryInt(uint8_t opcode) {
    emitOpcode(opcode);
    updateStack(-1);
}

void BytecodeContext::emitBinaryFloat(uint8_t opcode) {
    emitOpcode(opcode);
    updateStack(-1);
}

void BytecodeContext::emitNeg(const Type& type) {
    TypeKind kind = normalizeToJvmPrimitive(type.dataType);
    if (kind == TypeKind::FLOAT) {
        emitOpcode(OP_FNEG);
        return;
    }
    emitOpcode(OP_INEG);
}

void BytecodeContext::emitIInc(int index, int amount) {
    emitOpcode(OP_IINC);
    emitU1(static_cast<uint8_t>(index));
    emitU1(static_cast<uint8_t>(amount));
}

void BytecodeContext::emitFcmpl() {
    emitOpcode(OP_FCMPL);
    updateStack(-1);
}

void BytecodeContext::emitReturn(const Type& type) {
    TypeKind kind = normalizeToJvmPrimitive(type.dataType);
    if (kind == TypeKind::FLOAT) {
        emitOpcode(OP_FRETURN);
        updateStack(-1);
        return;
    }
    if (kind == TypeKind::INT || kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        emitOpcode(OP_IRETURN);
        updateStack(-1);
        return;
    }
    if (kind == TypeKind::VOID) {
        emitOpcode(OP_RETURN);
        return;
    }
    emitOpcode(OP_ARETURN);
    updateStack(-1);
}

void BytecodeContext::emitGetField(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addFieldRef(owner, name, desc);
    emitOpcode(OP_GETFIELD);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(0);
}

void BytecodeContext::emitPutField(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addFieldRef(owner, name, desc);
    emitOpcode(OP_PUTFIELD);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(-2);
}

void BytecodeContext::emitGetStatic(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addFieldRef(owner, name, desc);
    emitOpcode(OP_GETSTATIC);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(1);
}

void BytecodeContext::emitPutStatic(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addFieldRef(owner, name, desc);
    emitOpcode(OP_PUTSTATIC);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(-1);
}

void BytecodeContext::emitInvokeSpecial(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addMethodRef(owner, name, desc);
    emitOpcode(OP_INVOKESPECIAL);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(-computeDescriptorArgSlots(desc));
    updateStack(-1);
    updateStack(computeDescriptorReturnSlots(desc));
}

void BytecodeContext::emitInvokeVirtual(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addMethodRef(owner, name, desc);
    emitOpcode(OP_INVOKEVIRTUAL);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(-computeDescriptorArgSlots(desc));
    updateStack(-1);
    updateStack(computeDescriptorReturnSlots(desc));
}

void BytecodeContext::emitInvokeStatic(const std::string& owner, const std::string& name, const std::string& desc) {
    int idx = addMethodRef(owner, name, desc);
    emitOpcode(OP_INVOKESTATIC);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(-computeDescriptorArgSlots(desc));
    updateStack(computeDescriptorReturnSlots(desc));
}

void BytecodeContext::emitNewObject(const std::string& owner) {
    int idx = addClass(owner);
    emitOpcode(OP_NEW);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(1);
}

void BytecodeContext::emitANewArray(const std::string& className) {
    int idx = addClass(className);
    emitOpcode(OP_ANEWARRAY);
    emitU2(static_cast<uint16_t>(idx));
    updateStack(0);
}

void BytecodeContext::emitAAStore() {
    emitOpcode(OP_AASTORE);
    updateStack(-3);
}

void BytecodeContext::emitNewArray(TypeKind elementKind) {
    uint8_t atype = 10; // int
    switch (elementKind) {
        case TypeKind::BOOL: atype = 4; break;
        case TypeKind::CHAR: atype = 5; break;
        case TypeKind::FLOAT: atype = 6; break;
        case TypeKind::INT: atype = 10; break;
        default: atype = 10; break;
    }
    emitOpcode(OP_NEWARRAY);
    emitU1(atype);
    updateStack(0);
}

void BytecodeContext::emitArrayLoad(TypeKind elementKind) {
    switch (elementKind) {
        case TypeKind::FLOAT:
            emitOpcode(OP_FALOAD);
            break;
        case TypeKind::CLASS_NAME:
        case TypeKind::TYPE_ID:
            emitOpcode(OP_AALOAD);
            break;
        default:
            emitOpcode(OP_IALOAD);
            break;
    }
    updateStack(-1);
}

void BytecodeContext::emitArrayStore(TypeKind elementKind) {
    switch (elementKind) {
        case TypeKind::FLOAT:
            emitOpcode(OP_FASTORE);
            break;
        case TypeKind::CLASS_NAME:
        case TypeKind::TYPE_ID:
            emitOpcode(OP_AASTORE);
            break;
        default:
            emitOpcode(OP_IASTORE);
            break;
    }
    updateStack(-3);
}

int BytecodeContext::addUtf8(const std::string& value) {
    CpEntry entry;
    entry.tag = 1;
    entry.data.push_back(static_cast<uint8_t>((value.size() >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(value.size() & 0xff));
    entry.data.insert(entry.data.end(), value.begin(), value.end());
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addClass(const std::string& className) {
    int nameIndex = addUtf8(className);
    CpEntry entry;
    entry.tag = 7;
    entry.data.push_back(static_cast<uint8_t>((nameIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(nameIndex & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addString(const std::string& value) {
    int utf8Index = addUtf8(value);
    CpEntry entry;
    entry.tag = 8;
    entry.data.push_back(static_cast<uint8_t>((utf8Index >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(utf8Index & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addInteger(int value) {
    CpEntry entry;
    entry.tag = 3;
    entry.data.push_back(static_cast<uint8_t>((value >> 24) & 0xff));
    entry.data.push_back(static_cast<uint8_t>((value >> 16) & 0xff));
    entry.data.push_back(static_cast<uint8_t>((value >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(value & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addFloat(float value) {
    uint32_t bits;
    std::memcpy(&bits, &value, sizeof(float));
    CpEntry entry;
    entry.tag = 4;
    entry.data.push_back(static_cast<uint8_t>((bits >> 24) & 0xff));
    entry.data.push_back(static_cast<uint8_t>((bits >> 16) & 0xff));
    entry.data.push_back(static_cast<uint8_t>((bits >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(bits & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addNameAndType(const std::string& name, const std::string& desc) {
    int nameIndex = addUtf8(name);
    int descIndex = addUtf8(desc);
    CpEntry entry;
    entry.tag = 12;
    entry.data.push_back(static_cast<uint8_t>((nameIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(nameIndex & 0xff));
    entry.data.push_back(static_cast<uint8_t>((descIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(descIndex & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addFieldRef(const std::string& owner, const std::string& name, const std::string& desc) {
    int classIndex = addClass(owner);
    int nameTypeIndex = addNameAndType(name, desc);
    CpEntry entry;
    entry.tag = 9;
    entry.data.push_back(static_cast<uint8_t>((classIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(classIndex & 0xff));
    entry.data.push_back(static_cast<uint8_t>((nameTypeIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(nameTypeIndex & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

int BytecodeContext::addMethodRef(const std::string& owner, const std::string& name, const std::string& desc) {
    int classIndex = addClass(owner);
    int nameTypeIndex = addNameAndType(name, desc);
    CpEntry entry;
    entry.tag = 10;
    entry.data.push_back(static_cast<uint8_t>((classIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(classIndex & 0xff));
    entry.data.push_back(static_cast<uint8_t>((nameTypeIndex >> 8) & 0xff));
    entry.data.push_back(static_cast<uint8_t>(nameTypeIndex & 0xff));
    constantPool.push_back(entry);
    return static_cast<int>(constantPool.size());
}

void BytecodeContext::patchLabel(Label* label) {
    if (!currentMethod || !label || label->position < 0) {
        return;
    }
    for (int insnPos : label->fixups) {
        int offset = label->position - insnPos;
        currentMethod->code[insnPos + 1] = static_cast<uint8_t>((offset >> 8) & 0xff);
        currentMethod->code[insnPos + 2] = static_cast<uint8_t>(offset & 0xff);
    }
    label->fixups.clear();
}

void BytecodeContext::updateStack(int delta) {
    if (!currentMethod) {
        return;
    }
    currentMethod->currentStack += delta;
    if (currentMethod->currentStack > currentMethod->maxStack) {
        currentMethod->maxStack = currentMethod->currentStack;
    }
    if (currentMethod->currentStack < 0) {
        currentMethod->currentStack = 0;
    }
}

int BytecodeContext::computeDescriptorArgSlots(const std::string& desc) const {
    int slots = 0;
    bool inArgs = false;
    for (size_t i = 0; i < desc.size(); ++i) {
        char c = desc[i];
        if (c == '(') {
            inArgs = true;
            continue;
        }
        if (!inArgs) {
            continue;
        }
        if (c == ')') {
            break;
        }
        if (c == 'L') {
            while (i < desc.size() && desc[i] != ';') {
                i++;
            }
            slots += 1;
            continue;
        }
        if (c == '[') {
            while (i < desc.size() && desc[i] == '[') {
                i++;
            }
            if (i < desc.size() && desc[i] == 'L') {
                while (i < desc.size() && desc[i] != ';') {
                    i++;
                }
            }
            slots += 1;
            continue;
        }
        if (c == 'J' || c == 'D') {
            slots += 2;
        } else {
            slots += 1;
        }
    }
    return slots;
}

int BytecodeContext::computeDescriptorReturnSlots(const std::string& desc) const {
    auto pos = desc.find(')');
    if (pos == std::string::npos || pos + 1 >= desc.size()) {
        return 0;
    }
    char c = desc[pos + 1];
    if (c == 'V') {
        return 0;
    }
    if (c == 'J' || c == 'D') {
        return 2;
    }
    return 1;
}

TypeKind BytecodeContext::normalizeToJvmPrimitive(TypeKind kind) {
    if (kind == TypeKind::BOOL || kind == TypeKind::CHAR) {
        return TypeKind::INT;
    }
    return kind;
}

void BytecodeContext::writeClassFile() {
    std::ofstream out(outputPath, std::ios::binary);
    if (!out.is_open()) {
        return;
    }

    std::vector<MethodBuilder> methods = currentClass.methods;

    bool hasInit = false;
    for (const auto& method : methods) {
        if (method.name == "<init>") {
            hasInit = true;
            break;
        }
    }
    if (!hasInit) {
        MethodBuilder initMethod;
        initMethod.name = "<init>";
        initMethod.descriptor = "()V";
        initMethod.accessFlags = ACC_PUBLIC;
        initMethod.code = {
            OP_ALOAD_0,
            OP_INVOKESPECIAL,
            0x00, 0x00,
            OP_RETURN
        };
        const std::string initOwner = superClassName.empty() ? "java/lang/Object" : superClassName;
        int initMethodRef = addMethodRef(initOwner, "<init>", "()V");
        initMethod.code[2] = static_cast<uint8_t>((initMethodRef >> 8) & 0xff);
        initMethod.code[3] = static_cast<uint8_t>(initMethodRef & 0xff);
        initMethod.maxStack = 1;
        initMethod.maxLocals = 1;
        methods.insert(methods.begin(), initMethod);
    }

    int thisClassIndex = addClass(currentClass.name);
    int superClassIndex = addClass(superClassName.empty() ? "java/lang/Object" : superClassName);
    int codeUtf8Index = addUtf8("Code");

    std::vector<int> fieldNameIndices;
    std::vector<int> fieldDescIndices;
    fieldNameIndices.reserve(currentClass.fields.size());
    fieldDescIndices.reserve(currentClass.fields.size());
    for (const auto& field : currentClass.fields) {
        fieldNameIndices.push_back(addUtf8(field.name));
        fieldDescIndices.push_back(addUtf8(field.descriptor));
    }

    std::vector<int> methodNameIndices;
    std::vector<int> methodDescIndices;
    methodNameIndices.reserve(methods.size());
    methodDescIndices.reserve(methods.size());
    for (const auto& method : methods) {
        methodNameIndices.push_back(addUtf8(method.name));
        methodDescIndices.push_back(addUtf8(method.descriptor));
    }

    auto writeU1 = [&out](uint8_t v) { out.put(static_cast<char>(v)); };
    auto writeU2 = [&out](uint16_t v) {
        out.put(static_cast<char>((v >> 8) & 0xff));
        out.put(static_cast<char>(v & 0xff));
    };
    auto writeU4 = [&out](uint32_t v) {
        out.put(static_cast<char>((v >> 24) & 0xff));
        out.put(static_cast<char>((v >> 16) & 0xff));
        out.put(static_cast<char>((v >> 8) & 0xff));
        out.put(static_cast<char>(v & 0xff));
    };

    writeU4(0xCAFEBABE);
    writeU2(0);
    writeU2(49);

    writeU2(static_cast<uint16_t>(constantPool.size() + 1));
    for (const auto& entry : constantPool) {
        writeU1(entry.tag);
        out.write(reinterpret_cast<const char*>(entry.data.data()), entry.data.size());
    }

    writeU2(static_cast<uint16_t>(ACC_PUBLIC | ACC_SUPER));
    writeU2(static_cast<uint16_t>(thisClassIndex));
    writeU2(static_cast<uint16_t>(superClassIndex));

    writeU2(0);
    writeU2(static_cast<uint16_t>(currentClass.fields.size()));
    for (size_t i = 0; i < currentClass.fields.size(); ++i) {
        const auto& field = currentClass.fields[i];
        writeU2(field.accessFlags);
        writeU2(static_cast<uint16_t>(fieldNameIndices[i]));
        writeU2(static_cast<uint16_t>(fieldDescIndices[i]));
        writeU2(0);
    }

    writeU2(static_cast<uint16_t>(methods.size()));
    for (size_t i = 0; i < methods.size(); ++i) {
        const auto& method = methods[i];
        int nameIndex = methodNameIndices[i];
        int descIndex = methodDescIndices[i];
        writeU2(method.accessFlags);
        writeU2(static_cast<uint16_t>(nameIndex));
        writeU2(static_cast<uint16_t>(descIndex));
        writeU2(1);

        std::vector<uint8_t> code = method.code;
        uint32_t codeLength = static_cast<uint32_t>(code.size());

        writeU2(static_cast<uint16_t>(codeUtf8Index));
        uint32_t attributeLength = 12 + codeLength;
        writeU4(attributeLength);
        writeU2(static_cast<uint16_t>(method.maxStack));
        writeU2(static_cast<uint16_t>(method.maxLocals));
        writeU4(codeLength);
        if (codeLength > 0) {
            out.write(reinterpret_cast<const char*>(code.data()), code.size());
        }
        writeU2(0);
        writeU2(0);
    }

    writeU2(0);
    out.close();
}
void BytecodeContext::pushClassState() {
    ClassState state;
    state.className = className;
    state.superClassName = superClassName;
    state.outputPath = outputPath;
    state.classBuilder = currentClass;
    state.constantPool = constantPool;
    classStack.push_back(std::move(state));
}

void BytecodeContext::popClassState() {
    if (classStack.empty()) return;
    ClassState state = std::move(classStack.back());
    classStack.pop_back();
    className = state.className;
    superClassName = state.superClassName;
    outputPath = state.outputPath;
    currentClass = std::move(state.classBuilder);
    constantPool = std::move(state.constantPool);
    currentMethod = nullptr;
}
