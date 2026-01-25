bool hi(bool a) {
    NSLog(@"hello");
    return a;
}

int main() {
    int number = 8;

    if (number > 10 && hi(YES)) NSLog(@"%ld > 10 && hi(YES)", number);
    if (number > 5 && hi(YES)) NSLog(@"%ld > 5 && hi(YES)", number);

    if (number > 10 || hi(YES)) NSLog(@"%ld > 10 || hi(YES)", number);
    if (number > 5 || hi(YES)) NSLog(@"%ld > 5 || hi(YES)", number);

    if (number > 10 && hi(NO)) NSLog(@"%ld > 10 && hi(NO)", number);
    if (number > 5 && hi(NO)) NSLog(@"%ld > 5 && hi(NO)", number);

    if (number > 10 || hi(NO)) NSLog(@"%ld > 10 || hi(NO)", number);
    if (number > 5 || hi(NO)) NSLog(@"%ld > 5 || hi(NO)", number);
    
    return 0;
}
