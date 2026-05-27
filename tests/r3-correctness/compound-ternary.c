int main(void) {
    int a = 3;
    int b = 4;
    int c = a++ + ++b;
    c += a > b ? a : b;
    c *= 2;
    c >>= 1;
    c ^= 3;
    putchar('0' + (a % 10));
    putchar('0' + (b % 10));
    putchar('0' + (c % 10));
    return 0;
}
