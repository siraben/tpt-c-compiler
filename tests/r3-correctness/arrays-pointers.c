int main(void) {
    int values[5];
    int *p;
    values[0] = 2;
    values[1] = 3;
    values[2] = 4;
    p = &values[0];
    p[3] = p[0] + p[1] + p[2];
    *(p + 4) = p[3] - p[1];
    putchar('0' + values[3]);
    putchar('0' + values[4]);
    return 0;
}
