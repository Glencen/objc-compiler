int factorial(int n)
{
    if (n <= 1)
        return 1;

    return n * factorial(n - 1);
}


float average(float a, float b)
{
    return (a + b) / 2.000000;
}


bool trueReturn(int n)
{
    return true;
}

void printRange(int start, int end)
{
    for (int i = start; i <= end; i++)
    {
        NSLog(@"%d", i);
    }
}


int main()
{
    int fact5 = factorial(5);

    NSLog(@"factorial(5) = %d", fact5);

    NSLog(@"Printing range 1-5:");
    printRange(1, 5);

    float avg = average(10.000000, 20.000000);

    NSLog(@"average(10, 20) = %.2f", avg);

    bool evenCheck = trueReturn(4);

    return 0;
}
