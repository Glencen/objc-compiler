@interface MyProp : NSObject
@property int value;
@end

@implementation MyProp
@property int value;
@end

int main() {
    MyProp *obj = [MyProp new];
    [obj setValue: 5];
    int r = [obj getValue];
    [InOutFuncs printInt: r];
    [InOutFuncs printChar: '\n'];
    return 0;
}
