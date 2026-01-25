#include <stdio.h>

// Класс без NSObject
@interface MyClass
- (int)add:(int)a with:(int)b;
@end

@implementation MyClass
- (int)add:(int)a with:(int)b {
    return a + b;
}
@end

int main() {
    // Создание объекта "по-старинке"
    MyClass obj_instance;
    MyClass *obj = &obj_instance;

    int r = [obj add:4 with:5];

    printf("%d\n", r);

    return 0;
}
