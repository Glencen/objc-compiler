@interface BaseClass : NSObject
- (void)cool;
@end

@interface ChildClass : BaseClass
@end

@interface AnotherClass : BaseClass
@end

@implementation BaseClass
- (void)cool {
    NSLog(@"BaseClass::cool");
}
@end

@implementation ChildClass
- (void)cool {
    NSLog(@"ChildClass::cool");
}
@end

@implementation AnotherClass
- (void)cool {
    NSLog(@"AnotherClass::cool");
    [super cool];
}
@end

int main() {
    NSArray *arr = @[[[BaseClass alloc] init],
                        [[ChildClass alloc] init],
                        [[AnotherClass alloc] init]];

    for (BaseClass *obj in arr) {
        [obj cool];
    }
    
    return 0;
}
