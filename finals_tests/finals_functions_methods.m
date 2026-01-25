int add(int a, int b) { return a + b; }

@interface Calc : NSObject
- (int)mul:(int)a with:(int)b;
+ (int)staticAdd:(int)a with:(int)b;
@end

@implementation Calc
- (int)mul:(int)a with:(int)b { return a * b; }
+ (int)staticAdd:(int)a with:(int)b { return a + b; }
@end

int main() {
    [InOutFuncs printInt: add(2, 3)];
    [InOutFuncs printChar:'\n'];

    Calc *c = [Calc new];
    [InOutFuncs printInt: [c mul:2 with:4]];
    [InOutFuncs printChar:'\n'];

    [InOutFuncs printInt: [Calc staticAdd:5 with:6]];
    [InOutFuncs printChar:'\n'];
    return 0;
}
