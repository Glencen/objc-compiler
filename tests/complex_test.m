@class Node, Utils;

int max(int a, int b)
{
    return (a > b);
}

int factorial(int n)
{
    if (n <= 1)
        return 1;
    return n * factorial(n - 1);
}

@interface Node : NSObject
{
@public
    int value;
@protected
    int childrenCount;
@private
    int hidden;
}

@property (readonly) int value;
@property (readwrite) id data;

- (id)initWithValue:(int)v;
- (void)addChild:(Node *)child;
- (int)sum:(int)a with:(int)b and:(int)c;
+ (Node *)create:(int)v;
@end

@implementation Node
- (id)initWithValue:(int)v
{
    if (self = [super init])
    {
        value = v;
        childrenCount = 0;
        hidden = -1;
    }
    return self;
}

- (void)addChild:(Node *)child
{
    childrenCount++;
}

- (int)sum:(int)a with:(int)b and:(int)c
{
    return a + b + c;
}

+ (Node *)create:(int)v
{
    return [[Node alloc] initWithValue:v];
}
@end

@interface Utils : NSObject
- (void)bubbleSort:(int[])arr size:(int)n;
@end

@implementation Utils
- (void)bubbleSort:(int[])arr size:(int)n
{
    for (int i = 0; i < n - 1; i++)
    {
        for (int j = 0; j < n - i - 1; j++)
        {
            if (arr[j] > arr[j + 1])
            {
                int t = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = t;
            }
        }
    }
}
@end

int main()
{
    int a[5] = {5, 3, 4, 1, 2};

    int x = max(10, 20);
    int f = factorial(5);

    Node *node = [Node create:x];

    Utils *u = [[Utils alloc] init];
    [u bubbleSort:a size:5];

    if (x > 0 && f > 0)
        x++;
    else
        x--;

    int i = 0;
    while (i < 3)
    {
        i++;
    }

    do
    {
        i--;
    } while (i > 0);

    for (i = 0; i < 5; i++)
    {
        a[i] = a[i] + 1;
    }

    id obj = @"hello";
    id num = @42;
    id flt = @3.140000;

    return 0;
}
