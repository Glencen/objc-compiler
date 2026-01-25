bool hi(bool a) {
    [InOutFuncs printNSString: @"hello"]; 
    [InOutFuncs printChar: '\n'];
    return a;
}

int main() {
    int number = 7;

    if (number > 10 && hi(true)) {
        [InOutFuncs printNSString: @"number > 10 && hi(true)\n"]; 
    }
    if (number > 5 && hi(true)) {
        [InOutFuncs printNSString: @"number > 5 && hi(true)\n"]; 
    }

    if (number > 10 || hi(true)) {
        [InOutFuncs printNSString: @"number > 10 || hi(true)\n"]; 
    }
    if (number > 5 || hi(true)) {
        [InOutFuncs printNSString: @"number > 5 || hi(true)\n"]; 
    }

    if (number > 10 && hi(false)) {
        [InOutFuncs printNSString: @"number > 10 && hi(false)\n"]; 
    }
    if (number > 5 && hi(false)) {
        [InOutFuncs printNSString: @"number > 5 && hi(false)\n"]; 
    }

    if (number > 10 || hi(false)) {
        [InOutFuncs printNSString: @"number > 10 || hi(false)\n"]; 
    }
    if (number > 5 || hi(false)) {
        [InOutFuncs printNSString: @"number > 5 || hi(false)\n"]; 
    }

    return 0;
}
