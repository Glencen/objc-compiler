@interface BaseClass : NSObject
- (void)cool;
@end

@interface ChildClass : BaseClass
@end

@interface AnotherClass : BaseClass
@end

@implementation BaseClass
- (void)cool {
    [InOutFuncs printNSString: @"BaseClass::cool\n"]; 
}
@end

@implementation ChildClass
- (void)cool {
    [InOutFuncs printNSString: @"ChildClass::cool\n"]; 
}
@end

@implementation AnotherClass
- (void)cool {
    [InOutFuncs printNSString: @"AnotherClass::cool\n"]; 
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
