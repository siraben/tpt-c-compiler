union Slot {
    int i;
    char c;
};

int main(void) {
    union Slot s;
    s.i = 4;
    s.c = s.c + 2;
    putchar('0' + s.i);
    return 0;
}
