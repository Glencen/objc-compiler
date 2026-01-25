int main() {
    int a = 2;
    int b = 3;
    int c = a + b * 4;

    [InOutFuncs printInt: c];
    [InOutFuncs printChar: '\n'];

    if (c > 10) {
        [InOutFuncs printInt: 1];
    } else {
        [InOutFuncs printInt: 0];
    }
    [InOutFuncs printChar: '\n'];

    int i = 0;
    while (i < 3) {
        [InOutFuncs printInt: i];
        i++;
    }
    [InOutFuncs printChar: '\n'];

    for (int j = 0; j < 3; j++) {
        [InOutFuncs printInt: j];
    }
    [InOutFuncs printChar: '\n'];

    return 0;
}
