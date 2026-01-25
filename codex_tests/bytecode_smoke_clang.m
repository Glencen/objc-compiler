#include <stdio.h>

int main() {
    int a = 2;
    int b = 3;
    int c = a + b * 4;

    printf("%d\n", c);

    if (c > 10) {
        printf("%d", 1);
    } else {
        printf("%d", 0);
    }
    printf("\n");

    int i = 0;
    while (i < 3) {
        printf("%d", i);
        i++;
    }
    printf("\n");

    for (int j = 0; j < 3; j++) {
        printf("%d", j);
    }
    printf("\n");

    return 0;
}