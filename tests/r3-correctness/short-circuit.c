int counter = 0;

int bump(void) {
    counter = counter + 1;
    return counter;
}

int main(void) {
    int a = 0;
    if (0 && bump()) {
        a = 9;
    }
    if (1 || bump()) {
        a = a + 2;
    }
    if (bump() && bump()) {
        a = a + counter;
    }
    putchar('0' + a);
    putchar('0' + counter);
    return 0;
}
