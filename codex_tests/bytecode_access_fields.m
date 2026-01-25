@interface AccessBase : NSObject {
@public
    int pubField;
@protected
    int protField;
@private
    int privField;
}
- (void)show;
@end

@implementation AccessBase
- (void)show {
    [InOutFuncs printInt: pubField];
    [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: protField];
    [InOutFuncs printChar: '\n'];
}
@end

@interface AccessChild : AccessBase
@end

@implementation AccessChild
- (void)setFields {
    pubField = 7;
    protField = 5;
}
- (void)show {
    [InOutFuncs printInt: pubField];
    [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: protField];
    [InOutFuncs printChar: '\n'];
}
@end

int main() {
    AccessChild *c = [AccessChild new];
    [c setFields];
    [c show];
    return 0;
}
