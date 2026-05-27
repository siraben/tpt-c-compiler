int matrix[3][4];
int seed = 2;

int main(void) {
    register int row = 1;
    register int col = 2;
    matrix[row][col] = seed + row + col;
    matrix[2][3] = matrix[row][col] + 1;
    putchar('0' + matrix[row][col]);
    putchar('0' + matrix[2][3]);
    return 0;
}
