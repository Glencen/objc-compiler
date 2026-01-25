@interface AccessBase : NSObject {
@public
    int pubField;
@protected
    int protField;
@private
    int privField;
}
@public - (int)getPub;
@protected - (int)getProt;
@private - (int)getPriv;
@end

@implementation AccessBase
@public - (int)getPub { return pubField; }
@protected - (int)getProt { return protField; }
@private - (int)getPriv { return privField; }
@end

@interface AccessChild : AccessBase
- (int)useProt;
@end

@implementation AccessChild
- (int)useProt { return [self getProt]; }
@end

int main() {
    AccessChild *c = [AccessChild new];
    // public access ok
    c->pubField = 7;
    [InOutFuncs printInt:[c getPub]];
    [InOutFuncs printChar:'\n'];
    // protected access via subclass method
    [InOutFuncs printInt:[c useProt]];
    [InOutFuncs printChar:'\n'];
    return 0;
}
