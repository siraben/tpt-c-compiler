#include "parity_defs.h"

#define WIDTH (2 + 3)
#define SELECTED 1

extern int bump(int x);

int bump(int x) {
    return x + INCLUDED_VALUE;
}

int takes_void_pointer(void *p) {
    if (p == 0) {
        return 5;
    }
    return 1;
}

int main(void) {
    const unsigned short s = 4;
    volatile int total = 0;
    int data[WIDTH + 1];
    int *p;
    int *q;

#if SELECTED
    total += 2;
#else
    total += 200;
#endif

    data[0] = s;
    data[1] = bump(data[0]);
    p = data + 5;
    q = data + 1;
    total += p - q;

    switch (BASE + 1) {
        case ADD2(1, 3):
            total += data[1];
            break;
        default:
            total += 100;
    }

    total += takes_void_pointer(0);
    goto done;
    total += 1000;

done:
    __print_unsigned_int(total);
    return 0;
}
