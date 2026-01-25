int main() {
    int a = 2;
    int b = 3;
    int c = a + b * 4;
    char ch = 'A';
    NSString *s = @"ok";

    [InOutFuncs printInt:c];
    [InOutFuncs printChar:'\n'];
    [InOutFuncs printChar:ch];
    [InOutFuncs printChar:'\n'];
    [InOutFuncs printNSString:s];
    [InOutFuncs printChar:'\n'];

    if (c > 10) {
        [InOutFuncs printInt:1];
    } else {
        [InOutFuncs printInt:0];
    }
    [InOutFuncs printChar:'\n'];
    return 0;
}
