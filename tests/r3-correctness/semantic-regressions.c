int pick_first(int value, ...) {
    return value;
}

int main(void) {
    signed signed_value = 4;
    unsigned char small = 2;
    long wide = 3;
    int outer = 1;

label:
    __print_char_array("x");

    {
        int outer = 2;
        __print_unsigned_int(outer);
    }

    __print_unsigned_int(outer);
    __print_unsigned_int(pick_first(7, 8, 9));
    __print_unsigned_int(signed_value);
    __print_unsigned_int(wide + small);

    {
        unsigned int high = 65535u;
        unsigned int low = 1u;
        putchar(high > low ? 'A' : 'a');
        putchar(low < high ? 'B' : 'b');
        putchar(high <= low ? 'c' : 'C');
        putchar(low >= high ? 'd' : 'D');
    }

    return 0;
}
