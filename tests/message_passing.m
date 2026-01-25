@implementation Msg

- (void)test:(id)obj
{
    [self test:obj];
    [super test:obj];
    [Msg test:obj];

    [obj doSomething:1 with:2];
}

@end
