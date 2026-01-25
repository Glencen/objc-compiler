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
    NSLog(@"Parent: public=%d protected=%d", publicField, protectedField);
}
@end

@interface Child : Parent
- (void)showFields;
@end

@implementation Child
- (void)showFields {
    NSLog(@"Child: public=%d protected=%d", publicField, protectedField);
    [super showFields];
}
@end

int main() {
   
    Child *c = [[Child alloc] init];
    c->publicField = 1; 
    [c showFields];
    
    return 0;
}
