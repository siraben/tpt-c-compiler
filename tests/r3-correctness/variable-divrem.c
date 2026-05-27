int main(void) {
    int total = 0;
    for (int i = 1; i < 6; i = i + 1) {
        total = total + i;
    }
    putchar('0' + (total / 4));
    putchar('0' + (total % 4));
    return 0;
}
