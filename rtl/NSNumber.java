package rtl;

// NSNumber.java
public class NSNumber extends NSObject {

    public enum NumberType { INT, FLOAT }

    private NumberType type;
    private int intValue;
    private float floatValue;

    // ------------------- Конструкторы -------------------
    public NSNumber() {
        super.initDynamic();
        this.type = NumberType.INT;
        this.intValue = 0;
    }

    public NSNumber(int value) {
        super.initDynamic();
        this.type = NumberType.INT;
        this.intValue = value;
    }

    public NSNumber(float value) {
        super.initDynamic();
        this.type = NumberType.FLOAT;
        this.floatValue = value;
    }

    // ------------------- Статические методы -------------------
    public static NSNumber numberWithIntStatic(int value) {
        return new NSNumber(value);
    }

    public static NSNumber numberWithFloatStatic(float value) {
        return new NSNumber(value);
    }

    // ------------------- Динамические методы -------------------
    public int intValueDynamic() {
        if (type == NumberType.INT) return intValue;
        return (int) floatValue;
    }

    public float floatValueDynamic() {
        if (type == NumberType.FLOAT) return floatValue;
        return (float) intValue;
    }

    @Override
    public String descriptionDynamic() {
        return type == NumberType.INT ? Integer.toString(intValue) : Float.toString(floatValue);
    }

    @Override
    public String toString() {
        return descriptionDynamic();
    }

    @Override
    public NSNumber initDynamic() {
        super.initDynamic();
        return this;
    }

    // ------------------- Инициализация ClassInfo -------------------
    public static void initClassInfo(ClassInfo info) {
        NSNumber.s_classInfo = info;
    }

    // Статический ClassInfo для NSNumber
    private static ClassInfo s_classInfo = null;

    public static ClassInfo getClassInfoStatic() {
        return s_classInfo;
    }
}
