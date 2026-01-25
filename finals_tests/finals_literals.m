int main() {
    int d = 123;
    int o = 077;
    int h = 0x1a3b;
    char c1 = 'a';
    char c2 = '\n';
    char c3 = '\x1F';
    NSString *s1 = @"a\nb\tc";
    float f1 = 123.456;
    float f2 = .789;
    float f3 = 1.23e-4;

    [InOutFuncs printInt:d]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printInt:o]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printInt:h]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printChar:c1]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printChar:c2];
    [InOutFuncs printChar:c3];
    [InOutFuncs printChar:'\n'];
    [InOutFuncs printNSString:s1]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printFloat:f1]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printFloat:f2]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printFloat:f3]; [InOutFuncs printChar:'\n'];
    return 0;
}
