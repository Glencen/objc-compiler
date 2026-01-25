@interface MyPropRO : NSObject
@property (readonly) int value;
@end

@implementation MyPropRO
@property (readonly) int value;
@end

int main() {
    MyPropRO *obj = [MyPropRO new];
    int r = [obj getValue];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
