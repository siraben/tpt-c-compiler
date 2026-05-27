int main(void) {
    char a = getchar();
    char b = getchar_nb();
    char c = getchar_nb();
    if (a >= 'a' && a <= 'z') {
        putchar(a - 32);
    } else {
        putchar(a);
    }
    putchar(b);
    if (c == 0) {
        putchar('0');
    } else {
        putchar(c);
    }
    return 0;
}
