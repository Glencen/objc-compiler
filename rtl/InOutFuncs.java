package rtl;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class InOutFuncs {

    private static final Scanner scanner = new Scanner(System.in);

    // ------------------- Print Methods -------------------

    public static void printInt(int value) {
        System.out.print(value);
    }

    public static void printFloat(float value) {
        System.out.print(value);
    }

    public static void printChar(char value) {
        System.out.print(value);
    }

    public static void printNSString(NSString str) {
        System.out.print(str.toString());
    }

    public static void printNSNumber(NSNumber num) {
        System.out.print(num.toString());
    }

    public static void printNSArray(NSArray arr) {
        System.out.print("[");
        for (int i = 0; i < arr.countDynamic(); i++) {
            NSObject obj = arr.objectAtIndexDynamic(i);
            if (obj != null) {
                System.out.print(obj.toString());
                if (i != arr.countDynamic() - 1) System.out.print(", ");
            }
        }
        System.out.print("]");
    }

    public static void printNSObject(NSObject obj) {
        System.out.print(obj.toString());
    }

    public static void printInt$withInt(int a, int b) {
        printInt(a);
        printInt(b);
    }

    // ------------------- Read Methods -------------------

    public static int readInt() {
        return scanner.nextInt();
    }

    public static float readFloat() {
        return scanner.nextFloat();
    }

    public static char readChar() {
        String s = scanner.next();
        return s.isEmpty() ? '\0' : s.charAt(0);
    }

    public static NSString readNSString() {
        String s = scanner.next();
        return new NSString(s);
    }

    public static NSNumber readNSNumberInt() {
        int value = scanner.nextInt();
        return new NSNumber(value);
    }

    public static NSNumber readNSNumberFloat() {
        float value = scanner.nextFloat();
        return new NSNumber(value);
    }

    public static NSArray readNSArray() {
        int n = scanner.nextInt();
        List<NSObject> items = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            String s = scanner.next();
            items.add(new NSString(s));
        }
        return new NSArray(items);
    }
}
