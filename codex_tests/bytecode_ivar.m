@interface MyIvar : NSObject {
    @public int x;
}
- (void)setX:(int)v;
- (int)sum:(int)v;
@end

@implementation MyIvar
- (void)setX:(int)v {
    x = v;
}
- (int)sum:(int)v {
    return x + v;
}
@end

int main() {
    MyIvar *obj = [MyIvar new];
    [obj setX: 7];
    int r = [obj sum: 5];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
