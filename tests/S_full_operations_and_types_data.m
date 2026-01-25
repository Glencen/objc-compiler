int main() {
    int a = 5, b = 3;
    float x = 1.5, y = 2.5;
    char c1 = 'A', c2 = 'B';
    NSString *s1 = @"hello";
    NSString *s2 = @"world";
    bool flag = true;

    int sum = a + b;
    int diff = a - b;
    int mul = a * b;
    int divi = a / b;
    [InOutFuncs printNSString: @"int: "];
    [InOutFuncs printInt: sum]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: diff]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: mul]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: divi]; [InOutFuncs printChar: '\n'];

    float fsum = x + y;
    float fdiv = x / y;
    [InOutFuncs printNSString: @"float: "];
    [InOutFuncs printFloat: fsum]; [InOutFuncs printChar: ' '];
    [InOutFuncs printFloat: fdiv]; [InOutFuncs printChar: '\n'];

    bool eq = (a == b);
    bool neq = (a != b);
    bool ge = (a >= b);
    bool le = (a <= b);
    [InOutFuncs printNSString: @"int cmp: "];
    [InOutFuncs printInt: eq]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: neq]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: ge]; [InOutFuncs printChar: ' '];
    [InOutFuncs printInt: le]; [InOutFuncs printChar: '\n'];

    bool charCmp = (c1 == c2);
    [InOutFuncs printNSString: @"char cmp: "];
    [InOutFuncs printInt: charCmp];
    [InOutFuncs printChar: '\n'];

    bool strCmp = [s1 isEqualToString: s2];
    [InOutFuncs printNSString: @"NSString cmp: "];
    [InOutFuncs printInt: strCmp];
    [InOutFuncs printChar: '\n'];

    bool logic = (flag && (a > b)) || !flag;
    [InOutFuncs printNSString: @"logic: "];
    [InOutFuncs printInt: logic];
    [InOutFuncs printChar: '\n'];

    a = a + 2;
    b = b * 3;
    x = x - 1.0;
    y = y / 0.5;
    [InOutFuncs printNSString: @"assignments: a="];
    [InOutFuncs printInt: a]; [InOutFuncs printNSString: @" b="];
    [InOutFuncs printInt: b]; [InOutFuncs printNSString: @" x="];
    [InOutFuncs printFloat: x]; [InOutFuncs printNSString: @" y="];
    [InOutFuncs printFloat: y]; [InOutFuncs printChar: '\n'];

    return 0;
}
