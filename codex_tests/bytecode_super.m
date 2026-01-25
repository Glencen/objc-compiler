@interface Base : NSObject
- (int)value;
@end

@implementation Base
- (int)value {
    return 1;
}
@end

@interface Derived : Base
- (int)value;
@end

@implementation Derived
- (int)value {
    return [super value] + 1;
}
@end

int main() {
    Derived *obj = [Derived new];
    int r = [obj value];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
