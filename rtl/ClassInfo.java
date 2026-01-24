package rtl;

import java.util.*;

public class ClassInfo {
    private String name;
    private ClassInfo superclass;
    private boolean hasInterface = false;
    private boolean hasImplementation = false;

    // Конструктор
    public ClassInfo(String name, ClassInfo superclass) {
        this.name = name;
        this.superclass = superclass;
    }

    // Методы для маркировки класса
    public void markAsInterface() {
        hasInterface = true;
    }

    public void markAsImplementation() {
        hasImplementation = true;
    }

    public boolean isComplete() {
        return hasInterface && hasImplementation;
    }

    // Проверка наследования
    public boolean isSubclassOf(ClassInfo other) {
        if (other == null) return false;
        for (ClassInfo current = this.superclass; current != null; current = current.superclass) {
            if (current == other) return true;
        }
        return false;
    }

    // Геттеры и сеттеры
    public String getName() {
        return name;
    }

    public ClassInfo getSuperclass() {
        return superclass;
    }

    public void setSuperclass(ClassInfo superclass) {
        this.superclass = superclass;
    }

    // Для отладки
    @Override
    public String toString() {
        String base = name;
        if (superclass != null) {
            base += " extends " + superclass.name;
        }
        return base;
    }
}
