@interface Base : NSObject
- (int)value;
@end

@implementation Base
- (int)value { return 10; }
@end

@interface Derived : Base
- (int)value;
@end

@implementation Derived
- (int)value { return [super value] + 2; }
@end

@interface PropDemo : NSObject
@property int x;
@property (readonly) int ro;
@end

@implementation PropDemo
@property int x;
@property (readonly) int ro;
@end

int main() {
    Base *b = [Base new];
    Derived *d = [Derived new];
    [InOutFuncs printInt:[b value]];
    [InOutFuncs printChar:' '];
    [InOutFuncs printInt:[d value]];
    [InOutFuncs printChar:'\n'];

    PropDemo *p = [PropDemo new];
    [p setX:5];
    [InOutFuncs printInt:[p getX]];
    [InOutFuncs printChar:'\n'];
    return 0;
}
