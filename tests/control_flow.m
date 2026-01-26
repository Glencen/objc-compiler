int main()
{
    int a = 0;
    int b = 0;
    id array = nil;

    for (int i = 0; i < 3; i++)
    {
        NSLog(@"%d", i);
    }

    int j = 0;
    while (j < 3)
    {
        j++;
    }

    int k = 0;
    do
    {
        k++;
    }
    while (k < 3);

    for (id item in array)
    {
        NSLog(@"%@", item);
    }

    if (a > b)
    {
        NSLog(@"a > b");
    }
    else
    {
        NSLog(@"a <= b");
    }

    return 0;
}
