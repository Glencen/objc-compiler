int addTwice(int x) {
    return x + x;
}

int main() {
    int a = 2;
    int b = 3;
    int c = a + b * 4;
    int d = addTwice(c);

    [InOutFuncs printInt: c];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printInt: d];
    [InOutFuncs printChar: '\n'];

    NSString *s = "hello";
    NSString *t = s;
    [InOutFuncs printNSString: s];
    [InOutFuncs printChar: '\n'];
    if (s == t) {
        [InOutFuncs printInt: 1];
    } else {
        [InOutFuncs printInt: 0];
    }
    [InOutFuncs printChar: '\n'];
    if (s && t) {
        [InOutFuncs printInt: 1];
    } else {
        [InOutFuncs printInt: 0];
    }
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
