int main() {
    int arr[3];
    arr[0] = 1; arr[1] = 2; arr[2] = 3;
    [InOutFuncs printInt:arr[1]];
    [InOutFuncs printChar:'\n'];

    NSArray *list = @[@1, @2, @3];
    [InOutFuncs printNSObject:[list objectAtIndex:1]];
    [InOutFuncs printChar:'\n'];
    return 0;
}
