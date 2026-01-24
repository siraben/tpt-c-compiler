/*
    Minimal C-only test: print 123 using div/mod digit extraction.
    Uses the built-in putchar from the standard library.
*/

int main() {
    int n = 123;
    int digits[5];
    int i = 0;

    if (n == 0) {
        putchar('0');
    } else {
        while (n > 0) {
            digits[i] = n % 10;
            i = i + 1;
            n = n / 10;
        }
        while (i > 0) {
            i = i - 1;
            putchar(digits[i] + '0');
        }
    }

    putchar('\n');
    return 0;
}
