@interface Counter : NSObject
@property (class) int total;
+ (int)inc;
@end

@implementation Counter
+ (int)inc {
    int v = [Counter getTotal];
    v = v + 1;
    [Counter setTotal:v];
    return v;
}
@end

int main() {
    [Counter setTotal:0];
    [InOutFuncs printInt:[Counter getTotal]]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printInt:[Counter inc]]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printInt:[Counter getTotal]]; [InOutFuncs printChar:'\n'];
    return 0;
}
