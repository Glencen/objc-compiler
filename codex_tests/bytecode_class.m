@interface MyClass : NSObject
- (int)add:(int)a with:(int)b;
@end

@implementation MyClass
- (int)add:(int)a with:(int)b {
    return a + b;
}
@end

int main() {
    MyClass *obj = [MyClass new];
    int r = [obj add: 4 with: 5];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
