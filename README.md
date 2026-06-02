
# tpt-c-compiler

A C-dialect compiler that emits TPTASM for [@LBPHacker](https://github.com/LBPHacker)'s R3 computer family in The Powder Toy.

The active compiler is the Haskell implementation in `src/`. The older Lua implementation is still present in the repository for reference, but new compiler work, optimization work, and emulator correctness tests target the Haskell executable.

# Dependencies

## Nix Development Shell

The recommended workflow is:

```bash
nix develop
cabal run -v0 exe:tptcc-hs -- input.c --output output.asm
```

The flake provides the Haskell toolchain used by the compiler. The R3 correctness suite also expects an R3 emulator checkout, usually at `$HOME/R3emu`, unless `R3EMU_ROOT` or `R3EMU_BIN` is set.

## R3, TPTASM, And R3Plot

To load programs in The Powder Toy, you need an R3 in the simulation area. You can generate one with R3Plot or use an existing R3 save. TPTASM can be installed through The Powder Toy Script Manager or Jacob1's Mod.

After compiling, pass the generated assembly path to TPTASM from The Powder Toy's console:

```lua
tptasm("path/to/output.asm")
```

# Usage

```bash
cabal run -v0 exe:tptcc-hs -- input.c [--output output.asm] [--size total-memory-size] [--term-height terminal-rows] [--term-width terminal-cols] [--offset global-offset]
```

| Optional Argument | Description | Default |
| - | - | - |
| `--output` | Assembly output path | `<input-file-name>.asm` |
| `--size` | Total memory size in words | `2048` |
| `--term-height` | Terminal character rows | `8` |
| `--term-width` | Terminal character columns | `12` |
| `--offset` | Global memory offset | `0` |

Debug dump modes are available for compiler development:

```bash
cabal run -v0 exe:tptcc-hs -- --dump-tokens input.c
cabal run -v0 exe:tptcc-hs -- --dump-ast input.c
cabal run -v0 exe:tptcc-hs -- --dump-type-events input.c
cabal run -v0 exe:tptcc-hs -- --dump-ir-globals input.c
cabal run -v0 exe:tptcc-hs -- --dump-simple-tac input.c
cabal run -v0 exe:tptcc-hs -- --dump-ssa input.c
```

# C Dialect Status

The language is C89-like, plus a few practical extensions used by the examples, such as mixed declarations/statements and declarations in `for` initializers. It is not a conforming C89 implementation yet.

Currently supported:

- integer and character scalar code using `char`, `short`, `int`, `long`, `signed`, and `unsigned`
- pointers, `void *`, arrays, function calls, recursion, and function pointers
- structs, unions, enums, member access with `.` and `->`
- local, global, `extern`, `static`, `register`, and `typedef` declarations
- `const` and `volatile` qualifiers are accepted
- `#include`, object-like and simple function-like `#define`, `#undef`, `#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`, and `#endif`
- `if`, `else`, `while`, `do while`, `for`, `switch`, `case`, `default`, `break`, `continue`, `goto`, labels, and `return`
- arithmetic, bitwise, logical, comparison, assignment, compound assignment, increment/decrement, casts, `sizeof`, ternary, and comma expressions
- integer constant expressions in enum values, array sizes, and `case` labels
- string and character literals with common escapes
- inline `asm(...)` blocks for R3-specific code
- SSA-based optimization and graph-colouring register allocation

Major C89 gaps:

- preprocessor support is intentionally small: no system include search path, token pasting, stringizing, predefined macros, or full expression evaluator
- no separate compilation or linker
- `const` and `volatile` are parsed but not enforced semantically
- no floating-point types, floating constants, or floating arithmetic
- no old-style K&R function definitions, implicit `int`, or implicit function declarations
- no true variadic call support or `stdarg`
- no bitfields
- incomplete and recursive struct declarations are limited
- struct layout is word-slot based and does not model C alignment or padding
- aggregate initialization is limited compared with C89
- pointer semantics are still word-addressed and do not fully model strict object/function pointer rules

# Testing

Run the emulator-backed C correctness suite with:

```bash
nix develop --command bash scripts/r3-correctness.sh
```

The suite compiles checked-in fixtures from `tests/r3-correctness/`, assembles them with the R3 toolchain, runs them in the emulator, and checks terminal output.

# Standard Library Documentation
Many of the methods in this library have the prefix "`__`" which usually indicates that these methods are for the compiler's internal use. However, since a formal standard library is still being designed, these temporary methods can still be tremendously useful.

## `void __print_unsigned_int(int i)`
Displays an unsigned integer.
- **Parameters**
	-  `i` — The integer (interpreted as unsigned) to display
- **Returns**
	- `void`

---
	
## `void __print_signed_int(int i)`
Displays a signed integer.

- **Parameters**
	-  `i` — The integer (interpreted as signed) to display
- **Returns**
	- `void`

---

## `void putchar(char c)`

Displays a single character.

- **Parameters**
  - `c` — The character to output.
- **Returns**
  - `void`

---

## `void vscroll()`

Shifts the terminal's content upwards.

- **Parameters**
  - None.
- **Returns**
  - `void`

---

## `void hscroll()`

Shifts the terminal's content leftwards.

- **Parameters**
  - None.
- **Returns**
  - `void`

---

## `char getchar()`

Reads a single entered character.

- **Parameters**
  - None.
- **Returns**
  - The character read.

---

## `char getchar_nb()`

Reads a single entered character. Unlike `getchar()`, this function does not wait until a character is entered.

- **Parameters**
  - None.
- **Returns**
  - The character read.

---

## `void __scan_unsigned_int(int *out)`

Reads an unsigned integer from the input and stores it in the provided integer

- **Parameters**
  - `out` — Address of the integer to store the result in
- **Returns**
  - `void`

---

## `void set_colour(int background, int foreground)`

Sets the background and foreground colour. The following built-in colours are listed below

- **Parameters**
  - `background` — Background colour
  - `foreground` — Foreground colour
- **Returns**
  - `void`

| Colour Name     | Value |
|-----------------|-------|
| BLACK           | 0     |
| DARK_BLUE       | 1     |
| DARK_GREEN      | 2     |
| DARK_CYAN       | 3     |
| DARK_RED        | 4     |
| DARK_MAGENTA    | 5     |
| DARK_YELLOW     | 6     |
| GREY            | 7     |
| DARK_GREY       | 8     |
| BLUE            | 9     |
| GREEN           | 10    |
| CYAN            | 11    |
| RED             | 12    |
| MAGENTA         | 13    |
| YELLOW          | 14    |
| WHITE           | 15    |

---

## `void set_text_colour(int colour)`

Sets the foreground colour. Note that this function may also modify the background colour if the colour input parameter is greater than 15.

- **Parameters**
  - `colour` — Colour to set foreground to.
- **Returns**
  - `void`

---

## `void set_cursor(int row, int column)`

Moves the cursor to a specified position.

- **Parameters**
  - `row` — Row to move cursor to.
  - `column` — Column to move cursor to.
- **Returns**
  - `void`

---

## `void set_terminal_mode(int mode)`

Sets the terminal’s mode. The following mode settings are listed below. The semantics of these terminal modes are described [here](https://github.com/LBPHacker/R316/blob/v2/manual.md#scrollprint-sub-range-scroll-selection-and-print-character).

- **Parameters**
  - `mode` — The mode to set the terminal to.
- **Returns**
  - `void`

| Terminal Setting                 | Value |
|----------------------------------|-------|
| TERM_ENABLE_NL                   | 0x20  |
| TERM_ENABLE_TERM_MODE_SCROLL     | 0x10  |
| TERM_ENABLE_SCROLLMASK           | 0x08  |
| TERM_ENABLE_ROW_ORIENTED         | 0x04  |
| TERM_ENABLE_ENABLE_COLOUR        | 0x02  |
| TERM_ENABLE_TERM_MODE            | 0x01  |
| TERM_DEFAULT                     | 0x25  |


---

## `int get_terminal_mode()`

Retrieves the current terminal mode.

- **Parameters**
  - None.
- **Returns**
  - The current terminal mode.

---

## `void plot(int x, int y, int colour)`

Sets the colour of a pixel at the specified terminal position. The origin (0, 0) is located at the top left corner.

- **Parameters**
  - `x` — The x coordinate of the pixel.
  - `y` — The y coordinate of the pixel.
  - `colour` — The colour value of the pixel.
- **Returns**
  - `void`

---

## `void __send_raw(int value, int location)`

Stores the value `value` at the address represented by `location`.
This method is mostly only used for debugging or for efficient manipulation of the terminal.

- **Parameters**
  - `value` — Value to store.
  - `location` — Location to store the value at.
- **Returns**
  - `void`

---

## `void __set_zero_char(int left_low, int left_high, int right_low, int right_high)`

Uses 4 16 bit values to construct an 8x8 bitmap for the 0th character.

- **Parameters**
  - `left_low` — The 2 even columns on the left side of the bitmap.
  - `left_high` — The 2 odd columns on the left side of the bitmap.
  - `right_low` — The 2 even columns on the right side of the bitmap.
  - `right_high` — The 2 odd columns on the right side of the bitmap.
- **Returns**
  - `void`

---

## `void __print_char_array(char *str)`

Displays a null-terminated array of characters.

- **Parameters**
  - `str` — Pointer to a character array.
- **Returns**
  - `void`

---

## `void set_hrange(int minimum_column, int maximum_column)`

Sets the horizontal range of the scrollprint. Note that the origin is at the top left corner.

- **Parameters**
  - `minimum_column` — The lowest inclusive column in the scrollprint
  - `maximum_column` — The highest inclusive column in the scrollprint
- **Returns**
  - `void`

---

## `void set_vrange(int minimum_row, int maximum_row)`

Sets the vertical range of the scrollprint. Note that the origin is at the top left corner.

- **Parameters**
  - `minimum_row` — The lowest inclusive row in the scrollprint
  - `maximum_row` — The highest inclusive row in the scrollprint
- **Returns**
  - `void`

---

# Inline Assembly
The Powder Toy Compiler provides basic support for inline assembly. The following EBNF-like pseudocode describes how to use this feature.

```
asm(
    { <string-literal> }*
    [: <inputs>]
    [: <outputs>]
    [: <clobbers>]
    );
```

The inline assembly statement consists of the `asm` keyword followed by a pair of parentheses containing the body of the statement.
The statement body begins with zero or more assembly instructions in the form of string literals. Note that the compiler does not parse these instructions and instead simply copies them into the output assembly file. Additionally, three distinct kinds of optional operands are available that help to integrate the assembly snippet within the encompassing program.

The inputs operand defines which variables (if any) should be copied into which registers. It takes the form of a list of register names assigned to variables accessible in the current scope. The compiler will ensure that the contents of the variables are copied to their corresponding registers.

The outputs operand defines which variables (if any) should receive the output of the inline assembly statement. Like the input operand, it consists of a list of registers assigned to variables accessible in the current scope.

The clobbers operand allows the compiler to preserve a set of registers. The registers are specified in a list.

### Example Usage
```c
    int double_word_low = 0xffff, double_word_high = 0x8001;
    int result;
    asm(
        "exhs r3, r0, r2"
        "adds r3, r1"
        :r1=double_word_low, r2=double_word_high
        :r3=result
        :r1, r2, r3
    );
  ```
