@interface MyObject : NSObject
@property (readwrite) int value;
@end

@implementation MyObject
@end

void changeByValue(int a, float b, bool flag, NSString *str, NSMutableArray *arr, MyObject *obj) {
    a = 42;
    b = 3.14;
    flag = NO;
    str = @"changed";
    [arr replaceObjectAtIndex:0 withObject:@99];
    obj.value = 99;
    
    NSLog(@"Inside changeByValue:");
    NSLog(@"int: %d, float: %f, bool: %d, string: %@, array[0]: %@, object.value: %d",
          a, b, flag, str, [arr objectAtIndex:0], obj.value);
}

int main() {
    int a = 1;
    float b = 1.5;
    bool flag = true;
    NSString *str = @"original";
    NSMutableArray *arr = @[@1, @2, nil];
    MyObject *obj = [MyObject new];
    obj.value = 1;
    
    NSLog(@"Initial values:");
    NSLog(@"int: %d, float: %f, bool: %d, string: %@, array[0]: %@, object.value: %d",
          a, b, flag, str, [arr objectAtIndex:0], obj.value);
    
    changeByValue(a, b, flag, str, arr, obj);
    
    NSLog(@"After changeByValue:");
    NSLog(@"int: %d, float: %f, bool: %d, string: %@, array[0]: %@, object.value: %d",
          a, b, flag, str, [arr objectAtIndex:0], obj.value);
    
    return 0;
}
