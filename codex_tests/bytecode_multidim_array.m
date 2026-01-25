int main() {
    int a[2][3];
    a[0][0] = 1;
    a[0][1] = 2;
    a[0][2] = 3;
    a[1][0] = 4;
    a[1][1] = 5;
    a[1][2] = 6;

    int sum = 0;
    sum = sum + a[0][0] + a[0][1] + a[0][2];
    sum = sum + a[1][0] + a[1][1] + a[1][2];

    [InOutFuncs printInt: a[1][2]];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printInt: sum];
    [InOutFuncs printChar: '\n'];
    return 0;
}
