int add(int a, int b) {
    return a + b;
}


@interface Base : NSObject
- (int)value;
@end

@implementation Base
- (int)value { return 10; }
@end

@interface Derived : Base
- (int)value;
@end

@implementation Derived
- (int)value { return [super value] + 2; }
@end

@interface Counter : NSObject
@property int x;
@property (readonly) int ro;
@property (class) int total;
- (int)getX;
- (void)setX:(int)v;
- (int)getRo;
@end

@implementation Counter
@property int x;
@property (readonly) int ro;
@end

int main() {
    // literals + arithmetic + comparisons
    int a = 5;
    int b = 0xA; // hex
    int c = add(a, b);
    [InOutFuncs printInt:c];
    [InOutFuncs printChar:'\n'];

    // float + bool + logic
    float f = 1.5;
    f = f * 2.0;
    [InOutFuncs printFloat:f];
    [InOutFuncs printChar:'\n'];

    bool flag = true;
    if (flag) {
        if (a < b) {
            [InOutFuncs printInt:1];
        } else {
            [InOutFuncs printInt:0];
        }
    } else {
        [InOutFuncs printInt:0];
    }
    [InOutFuncs printChar:'\n'];

    // strings
    NSString *s = @"hello";
    [InOutFuncs printNSString:s];
    [InOutFuncs printChar:'\n'];

    // C arrays + loops
    int arr[3];
    arr[0] = 1; arr[1] = 2; arr[2] = 3;
    int i = 0;
    while (i < 3) {
        [InOutFuncs printInt: arr[i]];
        i = i + 1;
    }
    [InOutFuncs printChar:'\n'];

    // do-while
    int j = 0;
    do {
        j = j + 1;
    } while (j < 2);
    [InOutFuncs printInt:j];
    [InOutFuncs printChar:'\n'];

    // multidim array
    int m[2][2];
    m[0][0] = 7; m[0][1] = 8; m[1][0] = 9; m[1][1] = 10;
    [InOutFuncs printInt:m[1][1]];
    [InOutFuncs printChar:'\n'];

    // classes, inheritance, super
    Base *base = [Base new];
    Derived *der = [Derived new];
    [InOutFuncs printInt:[base value]];
    [InOutFuncs printChar:' '];
    [InOutFuncs printInt:[der value]];
    [InOutFuncs printChar:'\n'];

    // properties + class property (get/setX style)
    Counter *cnt = [Counter new];
    [cnt setX:42];
    [InOutFuncs printInt:[cnt getX]];
    [InOutFuncs printChar:'\n'];
    [Counter setTotal:3];
    [InOutFuncs printInt:[Counter getTotal]];
    [InOutFuncs printChar:'\n'];

    // NSArray + for-in
    NSArray *list = @[@1, @2, @3];
    id it;
    for (it in list) {
        [InOutFuncs printNSObject:it];
    }
    [InOutFuncs printChar:'\n'];

    // nil
    id n = nil;
    if (n != nil) {
        [InOutFuncs printInt:1];
    } else {
        [InOutFuncs printInt:0];
    }
    [InOutFuncs printChar:'\n'];

    return 0;
}
