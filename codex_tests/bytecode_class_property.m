// class property smoke test
@interface MyCounter : NSObject
@property (class) int value;
@end

@implementation MyCounter
@end

int main() {
    [MyCounter setValue:5];
    int v = [MyCounter getValue];
    [InOutFuncs printInt:v];
    [InOutFuncs printChar:'\n'];
    return 0;
}
