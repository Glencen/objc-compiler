int main() {
    int i = 0;
    for (i = 0; i < 3; i = i + 1) {
        [InOutFuncs printInt:i];
    }
    [InOutFuncs printChar:'\n'];

    int j = 0;
    while (j < 2) {
        [InOutFuncs printInt:j];
        j = j + 1;
    }
    [InOutFuncs printChar:'\n'];

    int k = 0;
    do {
        k = k + 1;
    } while (k < 2);
    [InOutFuncs printInt:k];
    [InOutFuncs printChar:'\n'];

    NSArray *list = @[@1, @2];
    id it;
    for (it in list) {
        [InOutFuncs printNSObject:it];
    }
    [InOutFuncs printChar:'\n'];

    return 0;
}
