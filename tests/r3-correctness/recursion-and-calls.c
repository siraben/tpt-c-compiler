int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int mix(int x, int y, int z) {
    return x * 2 + y - z;
}

int main(void) {
    int a = fib(6);
    int b = mix(4, 7, 3);
    putchar('0' + a);
    putchar('0' + (b % 10));
    return 0;
}
