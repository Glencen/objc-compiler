@interface Parent : NSObject {
@public
    int publicField;
@protected
    int protectedField;
@private
    int privateField;
}
- (void)showFields;
@end

@implementation Parent
- (void)showFields {
    [InOutFuncs printNSString: @"Parent: public="]; 
    [InOutFuncs printInt: publicField];
    [InOutFuncs printNSString: @" protected="]; 
    [InOutFuncs printInt: protectedField];
    [InOutFuncs printChar: '\n'];
}
@end

@interface Child : Parent
- (void)showFields;
@end

@implementation Child
- (void)showFields {
    [InOutFuncs printNSString: @"Child: public="]; 
    [InOutFuncs printInt: publicField];
    [InOutFuncs printNSString: @" protected="]; 
    [InOutFuncs printInt: protectedField];
    [InOutFuncs printChar: '\n'];
    [super showFields];
}
@end

int main() {
    Child *c = [[Child alloc] init];
    c->publicField = 1;
    [c showFields];
    return 0;
}
