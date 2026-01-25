@implementation Expr

- (void)test
{
    int x = 1 + 2 * 3;
    int y = (1 + 2) * 3;
    int z = -x + !y;

    x++;
    y--;

    x = y = 3;
}

@end
