int main() {
    int n = 10;
    int m = 10;
    int matrix[100];

    int i = 0;
    while (i < n) {
        int j = 0;
        while (j < m) {
            int v = 0;
            if (i == 1 || i == 8 || j == 1 || j == 8) {
                if (i > 0 && i < 9 && j > 0 && j < 9) {
                    v = 1;
                }
            }
            matrix[i * m + j] = v;
            j = j + 1;
        }
        i = i + 1;
    }

    int x = 1, y = 1, newColor = 2;
    int oldColor = matrix[x * m + y];

    if (oldColor != newColor) {
        int qx[1000];
        int qy[1000];
        int head = 0;
        int tail = 0;

        qx[tail] = x; qy[tail] = y; tail = tail + 1;

        while (head < tail) {
            int cx = qx[head];
            int cy = qy[head];
            head = head + 1;

            if (cx >= 0 && cx < n && cy >= 0 && cy < m) {
                int idx = cx * m + cy;
                if (matrix[idx] == oldColor) {
                    matrix[idx] = newColor;

                    int nx = cx + 1;
                    int ny = cy;
                    if (nx >= 0 && nx < n && ny >= 0 && ny < m) {
                        int nidx = nx * m + ny;
                        if (matrix[nidx] == oldColor) {
                            qx[tail] = nx; qy[tail] = ny; tail = tail + 1;
                        }
                    }

                    nx = cx - 1; ny = cy;
                    if (nx >= 0 && nx < n && ny >= 0 && ny < m) {
                        int nidx = nx * m + ny;
                        if (matrix[nidx] == oldColor) {
                            qx[tail] = nx; qy[tail] = ny; tail = tail + 1;
                        }
                    }

                    nx = cx; ny = cy + 1;
                    if (nx >= 0 && nx < n && ny >= 0 && ny < m) {
                        int nidx = nx * m + ny;
                        if (matrix[nidx] == oldColor) {
                            qx[tail] = nx; qy[tail] = ny; tail = tail + 1;
                        }
                    }

                    nx = cx; ny = cy - 1;
                    if (nx >= 0 && nx < n && ny >= 0 && ny < m) {
                        int nidx = nx * m + ny;
                        if (matrix[nidx] == oldColor) {
                            qx[tail] = nx; qy[tail] = ny; tail = tail + 1;
                        }
                    }
                }
            }
        }
    }

    i = 0;
    while (i < n) {
        int j = 0;
        while (j < m) {
            int v = matrix[i * m + j];
            [InOutFuncs printInt: v];
            if (j < m - 1) [InOutFuncs printChar: ' '];
            j = j + 1;
        }
        [InOutFuncs printChar: '\n'];
        i = i + 1;
    }

    return 0;
}
