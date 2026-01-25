int main() {
    int a = 5, b = 3;
    float x = 1.5, y = 2.5;
    char c1 = 'A', c2 = 'B';
    NSString *s1 = @"hello";
    NSString *s2 = @"world";
    bool flag = YES;

    // Арифметика int
    int sum = a + b;
    int diff = a - b;
    int mul = a * b;
    int divi = a / b;
    NSLog(@"int: %d %d %d %d", sum, diff, mul, divi);

    // Арифметика float
    float fsum = x + y;
    float fdiv = x / y;
    NSLog(@"float: %f %f", fsum, fdiv);

    // Сравнения int
    bool eq = (a == b);
    bool neq = (a != b);
    bool ge = (a >= b);
    bool le = (a <= b);
    NSLog(@"int cmp: %d %d %d %d", eq, neq, ge, le);

    // Символы
    bool charCmp = (c1 == c2);
    NSLog(@"char cmp: %d", charCmp);

    // Строки
    bool strCmp = [s1 isEqualToString:s2];
    NSLog(@"NSString cmp: %d", strCmp);

    // Логика
    bool logic = (flag && (a > b)) || !flag;
    NSLog(@"logic: %d", logic);

    // Присваивания
    a += 2; 
    b *= 3;
    x -= 1.0; 
    y /= 0.5;
    NSLog(@"assignments: a=%d b=%d x=%f y=%f", a, b, x, y);
    
    return 0;
}
