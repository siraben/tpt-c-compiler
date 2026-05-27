int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

int main(void) {
    int (*fp)(int, int);
    fp = add;
    putchar('0' + fp(2, 3));
    fp = sub;
    putchar('0' + fp(7, 4));
    return 0;
}
