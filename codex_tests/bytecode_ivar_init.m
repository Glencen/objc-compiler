@interface MyInit : NSObject {
    @public int x = 4;
    @public int y = 8;
}
- (int)sum;
@end

@implementation MyInit
- (int)sum {
    return x + y;
}
@end

int main() {
    MyInit *obj = [MyInit new];
    int r = [obj sum];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
