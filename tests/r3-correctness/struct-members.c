struct Pair {
    int x;
    int y;
};

int main(void) {
    struct Pair p;
    struct Pair *q;
    p.x = 2;
    p.y = 5;
    q = &p;
    q->x = q->x + 1;
    putchar('0' + p.x + p.y);
    return 0;
}
