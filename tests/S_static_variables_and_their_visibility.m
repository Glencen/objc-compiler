@interface Counter : NSObject
+ (int)getCount;
+ (void)setCount:(int)value;
@end

@implementation Counter
+ (int)getCount {
    return 0;
}

+ (void)setCount:(int)value {
    [InOutFuncs printInt: value];
    [InOutFuncs printChar: '\n'];
}
@end

@interface Another : Counter
@end

@implementation Another
@end

int main() {
    [InOutFuncs printInt: [Counter getCount]];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printInt: [Another getCount]];
    [InOutFuncs printChar: '\n'];

    [Another setCount: 5];

    [InOutFuncs printInt: [Counter getCount]];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printInt: [Another getCount]];
    [InOutFuncs printChar: '\n'];

    return 0;
}
