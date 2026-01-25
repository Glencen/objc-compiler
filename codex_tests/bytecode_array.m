int main() {
    NSArray *arr = @[ "a", "b", "c" ];
    [InOutFuncs printInt: [arr count]];
    [InOutFuncs printChar: '\n'];
    [InOutFuncs printNSObject: [arr objectAtIndex: 1]];
    [InOutFuncs printChar: '\n'];
    return 0;
}
