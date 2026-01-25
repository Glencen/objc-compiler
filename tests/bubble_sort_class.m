@interface Sorter : NSObject

- (void)bubbleSort:(int[])arr size:(int)n;

@end


@implementation Sorter

- (void)bubbleSort:(int[])arr size:(int)n
{
    int i, j, temp;

    for (i = 0; i < n - 1; i++)
    {
        for (j = 0; j < n - i - 1; j++)
        {
            if (arr[j] > arr[j + 1])
            {
                temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

@end


int main()
{
    int a[5] = {4, 2, 5, 1, 3};

    Sorter *s = [[Sorter alloc] init];

    [s bubbleSort:a size:5];

    return 0;
}
