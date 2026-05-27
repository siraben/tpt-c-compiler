int main(void) {
    int i = 0;
    int sum = 0;
    do {
        i = i + 1;
        if (i == 2) {
            continue;
        }
        sum = sum + i;
        ;
    } while (i < 4);
    do
        ;
    while (0);
    putchar('0' + sum);
    return 0;
}
