#include <stdio.h>

// Функция, аналогичная addTwice
int addTwice(int x) {
    return x + x;
}

int main() {
    int a = 2;
    int b = 3;
    int c = a + b * 4;
    int d = addTwice(c);

    // Печать чисел
    printf("%d\n", c);
    printf("%d\n", d);

    // Работа со строками (C-строки вместо NSString)
    const char *s = "hello";
    const char *t = s;
    printf("%s\n", s);

    // Сравнение указателей
    if (s == t) {
        printf("1\n");
    } else {
        printf("0\n");
    }

    if (s && t) {
        printf("1\n");
    } else {
        printf("0\n");
    }

    // Проверка числа
    if (c > 10) {
        printf("1\n");
    } else {
        printf("0\n");
    }

    // while
    int i = 0;
    while (i < 3) {
        printf("%d", i);
        i++;
    }
    printf("\n");

    // for
    for (int j = 0; j < 3; j++) {
        printf("%d", j);
    }
    printf("\n");

    return 0;
}
