@interface MyClass : NSObject

- (void)sayHello;

@end


@implementation MyClass

- (void)sayHello
{
    NSLog(@"Hello from one file!");
}

@end


int main(int argc, char argv[])
{
    {
        MyClass *obj = [[MyClass alloc] init];
        [obj sayHello];
    }

    return 0;
}
