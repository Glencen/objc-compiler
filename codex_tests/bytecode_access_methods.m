@interface AccessMethodsBase : NSObject
@public
- (int)pub;
@protected
- (int)prot;
@private
- (int)priv;
@end

@implementation AccessMethodsBase
@public - (int)pub { return 1; }
@protected - (int)prot { return 2; }
@private - (int)priv { return 3; }
@end

@interface AccessMethodsChild : AccessMethodsBase
- (int)useProt;
@end

@implementation AccessMethodsChild
- (int)useProt { return [self prot]; }
@end

int main() {
    AccessMethodsChild *c = [AccessMethodsChild new];
    [InOutFuncs printInt: [c pub]];
    [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: [c useProt]];
    [InOutFuncs printChar: '\n'];
    return 0;
}
