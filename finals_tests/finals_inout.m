int main() {
    [InOutFuncs printInt:42]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printFloat:3.14]; [InOutFuncs printChar:'\n'];
    [InOutFuncs printChar:'Z']; [InOutFuncs printChar:'\n'];
    NSString *s = @"str";
    [InOutFuncs printNSString:s]; [InOutFuncs printChar:'\n'];
    NSNumber *n = @7;
    [InOutFuncs printNSNumber:n]; [InOutFuncs printChar:'\n'];
    NSArray *arr = @[@1, @2];
    [InOutFuncs printNSArray:arr]; [InOutFuncs printChar:'\n'];
    return 0;
}
