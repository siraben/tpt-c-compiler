enum Mode {
    ZERO,
    TWO = 2,
    THREE,
    SIX = 6
};

int main(void) {
    enum Mode m = THREE;
    switch (m) {
    case ZERO:
        putchar('0');
        break;
    case THREE:
        putchar('0' + THREE);
        break;
    default:
        putchar('x');
    }
    putchar('0' + SIX);
    return 0;
}
