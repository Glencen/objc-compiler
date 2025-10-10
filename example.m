#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    {
        int a = 10;
        float b = 3.14;
        NSString *str = @"Hello World";
        NSString *teststring = @"a\nb\tc";
        char testchar1 = '\\';
        char testchar2 = '\x1F';
        char testchar3 = '\177';
        char c = 'A';
        
        if (a > 5 && b < 10.0) {
            NSLog(@"%@", str);
        }
        
        for (int i = 0; i < 10; i++) {
            a += i;
        }
        
        while (a > 0) {
            a--;
        }
        
        switch (c) {
            case 'A':
                NSLog(@"Letter A");
                break;
            default:
                break;
        }
        
        // Single line comment
        /* Multi-line
           comment */
        
        NSArray *array = @[@"one", @"two"];
        NSDictionary *dict = @{@"key": @"value"};
        
        BOOL flag = YES;
        if (flag == NO) {
            return 0;
        }
        
        SEL selector = @selector(description);
        Class class = [NSString class];
        id obj = nil;
        
        @interface MyClass : NSObject
        @property (strong) NSString *name;
        - (void)method;
        @end
        
        @implementation MyClass
        - (void)method {
            self.name = @"Test";
        }
        @end
        
        int hex = 0xFF;
        int oct = 077;
        float sci = 1.23e-5;
        
        NSString *path = @"file.txt";
        const int MAX_SIZE = 100;
        
        return 0;
    }
}