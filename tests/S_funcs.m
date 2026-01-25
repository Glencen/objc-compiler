@interface MyObject : NSObject
@property (readwrite) int value;
@end

@implementation MyObject
@end

void changeByValue(int a, float b, bool flag, NSString *str, NSArray *arr, MyObject *obj) {
    a = 42;
    b = 3.14;
    flag = false;
    str = @"changed";
    obj.value = 99;

    [InOutFuncs printNSString: @"Inside changeByValue:\n"]; 
    [InOutFuncs printNSString: @"int: "]; [InOutFuncs printInt: a]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"float: "]; [InOutFuncs printFloat: b]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"bool: "]; [InOutFuncs printInt: flag]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"string: "]; [InOutFuncs printNSString: str]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"array0: "]; [InOutFuncs printNSObject: [arr objectAtIndex: 0]]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"object.value: "]; [InOutFuncs printInt: obj.value]; [InOutFuncs printChar: '\n'];
}

int main() {
    int a = 1;
    float b = 1.5;
    bool flag = true;
    NSString *str = @"original";
    NSArray *arr = @[@1, @2];
    MyObject *obj = [MyObject new];
    obj.value = 1;

    [InOutFuncs printNSString: @"Initial values:\n"]; 
    [InOutFuncs printNSString: @"int: "]; [InOutFuncs printInt: a]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"float: "]; [InOutFuncs printFloat: b]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"bool: "]; [InOutFuncs printInt: flag]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"string: "]; [InOutFuncs printNSString: str]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"array0: "]; [InOutFuncs printNSObject: [arr objectAtIndex: 0]]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"object.value: "]; [InOutFuncs printInt: obj.value]; [InOutFuncs printChar: '\n'];

    changeByValue(a, b, flag, str, arr, obj);

    [InOutFuncs printNSString: @"After changeByValue:\n"]; 
    [InOutFuncs printNSString: @"int: "]; [InOutFuncs printInt: a]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"float: "]; [InOutFuncs printFloat: b]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"bool: "]; [InOutFuncs printInt: flag]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"string: "]; [InOutFuncs printNSString: str]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"array0: "]; [InOutFuncs printNSObject: [arr objectAtIndex: 0]]; [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: @"object.value: "]; [InOutFuncs printInt: obj.value]; [InOutFuncs printChar: '\n'];

    return 0;
}
