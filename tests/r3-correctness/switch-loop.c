int main(void) {
    int sum = 0;
    for (int i = 0; i < 8; i = i + 1) {
        switch (i) {
        case 0:
            sum = sum + 1;
            break;
        case 1:
        case 2:
            continue;
        case 5:
            break;
        default:
            sum = sum + i;
        }
        if (sum > 9) {
            break;
        }
    }
    putchar('0' + sum);
    return 0;
}
