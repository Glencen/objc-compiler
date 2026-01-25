int bubbleSort(int arr[], int n)
{
    int i, j;
    int temp;

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

    return 0;
}

int main()
{
    int a[5] = {5, 3, 4, 1, 2};

    bubbleSort(a, 5);

    return 0;
}
