int main() {
    float a = 1.5;
    float b = 2.0;
    float c = a * b + 0.5;
    [InOutFuncs printFloat:c];
    [InOutFuncs printChar:'\n'];
    if (c > 3.0) {
        [InOutFuncs printInt:1];
    } else {
        [InOutFuncs printInt:0];
    }
    [InOutFuncs printChar:'\n'];
    return 0;
}
