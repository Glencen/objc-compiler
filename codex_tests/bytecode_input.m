int main() {
    int a = [InOutFuncs readInt];
    float f = [InOutFuncs readFloat];
    char c = [InOutFuncs readChar];
    NSString *s = [InOutFuncs readNSString];

    [InOutFuncs printInt: a];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printFloat: f];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printChar: c];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSString: s];
    [InOutFuncs printChar: '\n'];
    return 0;
}
