#include <stdio.h>

int main() {
    int arr[5];
    arr[0] = 5;
    arr[1] = 1;
    arr[2] = 4;
    arr[3] = 2;
    arr[4] = 8;

    // Сортировка пузырьком
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 4 - i; j++) {
            if (arr[j] > arr[j + 1]) {
                int tmp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
        }
    }

    // Вывод массива
    for (int k = 0; k < 5; k++) {
        printf("%d ", arr[k]);
    }
    printf("\n");

    return 0;
}
