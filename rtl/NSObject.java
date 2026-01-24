package rtl;

public class NSObject {

    // Статический указатель на ClassInfo для NSObject
    private static ClassInfo s_classInfo;

    // ------------------- Конструктор -------------------
    public NSObject() { }

    // ------------------- Инициализация -------------------
    public NSObject initDynamic() {
        return this;
    }

    // ------------------- Статические фабрики -------------------
    public static NSObject allocStatic() {
        return new NSObject();
    }

    public static NSObject newStatic() {
        NSObject obj = allocStatic();
        obj.initDynamic();
        return obj;
    }

    // ------------------- Методы для работы с классом -------------------
    public ClassInfo getClassDynamic() {
        return s_classInfo;
    }

    public static ClassInfo getClassStatic() {
        return s_classInfo;
    }

    public static boolean isSubclassOfClassStatic(ClassInfo cls) {
        return s_classInfo != null && s_classInfo.isSubclassOf(cls);
    }

    public String classNameDynamic() {
        return s_classInfo != null ? s_classInfo.getName() : "NSObject";
    }

    public ClassInfo superclassDynamic() {
        return s_classInfo != null ? s_classInfo.getSuperclass() : null;
    }

    // ------------------- Описание объекта -------------------
    public String descriptionDynamic() {
        return "<NSObject>";
    }

    public boolean isEqualDynamic(NSObject other) {
        return this == other;
    }

    // ------------------- Инициализация ClassInfo -------------------
    public static void initNSObjectClassInfo(ClassInfo info) {
        s_classInfo = info;
    }
}
