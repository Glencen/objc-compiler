@interface MyIvar2 : NSObject {
    @public int x;
}
- (void)setX:(int)v;
@end

@implementation MyIvar2
- (void)setX:(int)v {
    x = v;
}
@end

int main() {
    MyIvar2 *obj = [MyIvar2 new];
    [obj setX: 3];
    int a = obj->x;
    int b = obj.x;
    [InOutFuncs printInt: a];
    [InOutFuncs printInt: b];
    [InOutFuncs printChar: '\n'];
    return 0;
}
