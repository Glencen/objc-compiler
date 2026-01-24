package rtl;

import java.util.Locale;

public class NSString extends NSObject {

    // ------------------- Поле -------------------
    private String string;

    // ------------------- Статический ClassInfo -------------------
    public static ClassInfo s_classInfo;

    // ------------------- Конструкторы -------------------
    public NSString() {
        this.string = "";
    }

    public NSString(String str) {
        this.string = str;
    }

    public NSString(NSString other) {
        this.string = other.string;
    }

    // ------------------- Статические методы -------------------
    public static NSString stringStatic() {
        return new NSString("");
    }

    public static NSString stringWithCStringStatic(String cstr) {
        return new NSString(cstr);
    }

    public static NSString stringWithStringStatic(NSString str) {
        return new NSString(str);
    }

    // ------------------- Динамические методы -------------------
    public char characterAtIndexDynamic(int index) {
        if (index < 0 || index >= string.length()) return '\0';
        return string.charAt(index);
    }

    public int hasPrefixDynamic(NSString prefix) {
        return string.startsWith(prefix.string) ? 1 : 0;
    }

    public int hasSuffixDynamic(NSString suffix) {
        return string.endsWith(suffix.string) ? 1 : 0;
    }

    public int lengthDynamic() {
        return string.length();
    }

    public int intValueDynamic() {
        try {
            return Integer.parseInt(string);
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    public NSString capitalizeStringDynamic() {
        StringBuilder res = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : string.toCharArray()) {
            if (Character.isWhitespace(c)) {
                capitalizeNext = true;
                res.append(c);
            } else if (capitalizeNext) {
                res.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                res.append(c);
            }
        }
        return new NSString(res.toString());
    }

    public NSString lowercaseStringDynamic() {
        return new NSString(string.toLowerCase(Locale.ROOT));
    }

    public NSString uppercaseStringDynamic() {
        return new NSString(string.toUpperCase(Locale.ROOT));
    }

    public NSString stringByAppendingStringDynamic(NSString other) {
        return new NSString(string + other.string);
    }

    public boolean isEqualToStringDynamic(NSString other) {
        return this.string.equals(other.string);
    }

    @Override
    public boolean isEqualDynamic(NSObject other) {
        if (!(other instanceof NSString)) return false;
        NSString strObj = (NSString) other;
        return this.string.equals(strObj.string);
    }

    @Override
    public NSString initDynamic() {
        super.initDynamic();
        return this;
    }

    @Override
    public String descriptionDynamic() {
        return string;
    }

    // ------------------- Инициализация ClassInfo -------------------
    public static void initClassInfo(ClassInfo info) {
        s_classInfo = info;
    }

    // ------------------- Getter для строки -------------------
    public String getString() {
        return string;
    }
}
