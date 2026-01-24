package rtl;

import java.util.ArrayList;
import java.util.List;

public class NSArray extends NSObject {

    private List<NSObject> array;

    // ------------------- Конструкторы -------------------
    public NSArray() {
        this.array = new ArrayList<>();
    }

    public NSArray(NSArray other) {
        this.array = new ArrayList<>(other.array);
    }

    public NSArray(List<NSObject> objects) {
        this.array = new ArrayList<>(objects);
    }

    // ------------------- Статические методы -------------------
    public static NSArray arrayStatic() {
        return new NSArray();
    }

    public static NSArray arrayWithArrayStatic(NSArray array) {
        return new NSArray(array);
    }

    public static NSArray arrayWithObjectsStatic(NSObject... objects) {
        NSArray nsArray = new NSArray();
        for (NSObject obj : objects) {
            nsArray.array.add(obj);
        }
        return nsArray;
    }

    public static NSArray arrayWithObjectStatic(NSObject object) {
        return arrayWithObjectsStatic(object);
    }

    // ------------------- Динамические методы -------------------
    public NSArray arrayByAddingObjectDynamic(NSObject object) {
        NSArray res = new NSArray(this);
        res.array.add(object);
        return res;
    }

    public NSArray arrayByAddingObjectsFromArrayDynamic(NSArray anotherArray) {
        NSArray res = new NSArray(this);
        res.array.addAll(anotherArray.array);
        return res;
    }

    public NSObject objectAtIndexDynamic(int index) {
        if (index < 0 || index >= array.size()) return null;
        return array.get(index);
    }

    public int countDynamic() {
        return array.size();
    }

    public int containsObjectDynamic(NSObject object) {
        return array.contains(object) ? 1 : 0;
    }

    public NSString componentsJoinedByStringDynamic(NSString separator) {
        StringBuilder sb = new StringBuilder();
        String sep = separator.toString();
        for (int i = 0; i < array.size(); i++) {
            sb.append(array.get(i).descriptionDynamic());
            if (i != array.size() - 1) sb.append(sep);
        }
        return NSString.stringWithStringStatic(new NSString(sb.toString()));
    }

    public NSObject firstObjectDynamic() {
        return array.isEmpty() ? null : array.get(0);
    }

    public NSObject lastObjectDynamic() {
        return array.isEmpty() ? null : array.get(array.size() - 1);
    }

    public NSObject firstObjectCommonWithArrayDynamic(NSArray otherArray) {
        for (NSObject o : otherArray.array) {
            if (array.contains(o)) return o;
        }
        return null;
    }

    public int indexOfObjectDynamic(NSObject object) {
        int index = array.indexOf(object);
        return index >= 0 ? index : -1; // NSNotFound
    }

    public boolean isEqualToArrayDynamic(NSArray otherArray) {
        if (array.size() != otherArray.array.size()) return false;
        for (int i = 0; i < array.size(); i++) {
            if (!array.get(i).isEqualDynamic(otherArray.array.get(i))) return false;
        }
        return true;
    }


    @Override
    public NSArray initDynamic() {
        super.initDynamic();
        return this;
    }

    @Override
    public String descriptionDynamic() {
        return "NSArray instance";
    }

    // ------------------- Статический ClassInfo -------------------
    public static ClassInfo s_classInfo;

    public static void initClassInfo(ClassInfo info) {
        s_classInfo = info;
    }
}
