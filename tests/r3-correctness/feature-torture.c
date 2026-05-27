struct Inner {
    int bias;
    int value;
};

struct Box {
    int tag;
    struct Inner inner;
    int total;
};

union Scratch {
    int word;
    char byte;
};

enum Mode {
    MODE_ZERO,
    MODE_ADD = 3,
    MODE_MUL,
    MODE_SKIP = 8
};

int plus(int a, int b) {
    return a + b;
}

int mix(int a, int b) {
    return a * 2 - b;
}

int fold_box(struct Box *box, int (*op)(int, int), enum Mode mode) {
    int base = box->inner.value + box->inner.bias;
    switch (mode) {
    case MODE_ADD:
        return op(base, box->tag);
    case MODE_MUL:
        return base * box->tag;
    case MODE_SKIP:
        return 0;
    default:
        return base - box->tag;
    }
}

int main(void) {
    struct Box boxes[2];
    struct Box *p;
    union Scratch scratch;
    int (*fp)(int, int);
    int i;
    int sum = 0;

    boxes[0].tag = 2;
    boxes[0].inner.bias = 1;
    boxes[0].inner.value = 4;
    boxes[0].total = 0;

    boxes[1].tag = 3;
    boxes[1].inner.bias = 2;
    boxes[1].inner.value = 5;
    boxes[1].total = 0;

    p = &boxes[0];
    p->total = fold_box(p, plus, MODE_ADD);

    p = &boxes[1];
    fp = mix;
    p->total = fold_box(p, fp, MODE_ADD);

    for (i = 0; i < 2; i = i + 1) {
        switch (boxes[i].tag) {
        case 2:
            sum = sum + boxes[i].total;
            break;
        case 3:
            sum = sum + boxes[i].total - 1;
            break;
        default:
            sum = sum + 9;
        }
    }

    scratch.word = sum;
    scratch.byte = scratch.byte + (MODE_MUL - MODE_ADD);

    putchar('0' + (boxes[0].total % 10));
    putchar('0' + (boxes[1].total / 10));
    putchar('0' + (scratch.word % 10));
    putchar('0' + (fold_box(&boxes[1], plus, MODE_MUL) % 10));
    return 0;
}
