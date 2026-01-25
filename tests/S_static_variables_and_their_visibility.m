@interface Counter : NSObject
+ (int)getCount;
+ (void)setCount:(int)value;
@end

@implementation Counter
+ (int)getCount {
    id countContainer = @[ @0 ];
    return [countContainer[0] intValue];
}

+ (void)setCount:(int)value {
    id countContainer = @[ @(value) ];
}
@end

@interface Another : Counter
@end

@implementation Another
@end

int main() {
    NSLog(@"%d", [Counter getCount]);
    NSLog(@"%d", [Another getCount]);

    [Another setCount:5];

    NSLog(@"%d", [Counter getCount]);
    NSLog(@"%d", [Another getCount]);

    return 0;
}
