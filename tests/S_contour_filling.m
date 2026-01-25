void floodFill(NSMutableArray *matrix, int x, int y, int oldColor, int newColor, int n, int m) {
    if (x < 0 || x >= n || y < 0 || y >= m) return;

    if ([matrix[x][y] integerValue] != oldColor) return;

    matrix[x][y] = @(newColor);

    floodFill(matrix, x + 1, y, oldColor, newColor, n, m);
    floodFill(matrix, x - 1, y, oldColor, newColor, n, m);
    floodFill(matrix, x, y + 1, oldColor, newColor, n, m);
    floodFill(matrix, x, y - 1, oldColor, newColor, n, m);
}

int main() {
    
    int n = 10, m = 10;
    NSMutableArray *matrix = [NSMutableArray arrayWithArray:@[
        @[@0,@0,@0,@0,@0,@0,@0,@0,@0,@0],
        @[@1,@1,@1,@1,@1,@1,@1,@1,@1,@0],
        @[@0,@0,@0,@0,@0,@0,@0,@0,@1,@0],
        @[@0,@1,@1,@1,@1,@1,@1,@0,@1,@0],
        @[@0,@1,@0,@0,@0,@0,@1,@0,@1,@0],
        @[@0,@1,@0,@1,@1,@0,@1,@0,@1,@0],
        @[@0,@1,@0,@1,@1,@1,@1,@0,@1,@0],
        @[@0,@1,@0,@0,@0,@0,@0,@0,@1,@0],
        @[@0,@1,@1,@1,@1,@1,@1,@1,@1,@0],
        @[@0,@0,@0,@0,@0,@0,@0,@0,@0,@0]
    ]];

    int x = 1, y = 1, newColor = 2;
    int oldColor = [matrix[x][y] integerValue];
    if (oldColor != newColor) {
        floodFill(matrix, x, y, oldColor, newColor, n, m);
    }

    for (int i = 0; i < n; i++) {
        NSMutableArray *row = matrix[i];
        NSMutableArray *strRow = [NSMutableArray array];
        for (NSNumber *num in row) [strRow addObject:[num stringValue]];
        NSLog(@"%@", [strRow componentsJoinedByString:@" "]);
    }
    
    return 0;
}
