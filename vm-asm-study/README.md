# Haskell VM in R3 assembly

`runtime.asm` is a hand-written Blynn graph reducer: combinators, signed
16-bit arithmetic, references, putchar/getchar, and copying GC with explicit
out-of-memory checks. `build_asm.py` links Blynn's compiled graph to it; Python
does not compile Haskell. Unsupported foreign signatures are rejected.

## Build

Requires a C compiler and Python 3. From the repository root:

```sh
mkdir -p vm-asm-study/build
cc -O2 vm-asm-study/bootstrap/methodically.c -o vm-asm-study/build/methodically
vm-asm-study/build/methodically < vm-asm-study/bootstrap/party_tpt.hs \
  > vm-asm-study/build/party_tpt.c
cc -O2 vm-asm-study/build/party_tpt.c -o vm-asm-study/build/party_tpt
python3 vm-asm-study/build_asm.py examples/hello.hs --heap 768 \
  --output vm-asm-study/build/hello.asm
```

The driver writes `.asm`, the original generated `.c`, and graph metadata
`.json`. Use `--compiler PATH` to select another rebuilt `party_tpt` binary.

Assemble using TPTASM with Lua 5.1–5.4, then run with repaired R3emu:

```sh
lua -e 'unpack=unpack or table.unpack' /path/to/tptasm.lua \
  vm-asm-study/build/hello.asm vm-asm-study/build/hello.bin \
  vm-asm-study/build/hello.log R3A4810
r3emu vm-asm-study/build/hello.bin --profile showcase --memrows 48 \
  --headless --stdout --no-fpslimiter --max-cycles 100000
```

This configuration has **6144 physical RAM words**, not the stock 2048-word
showcase. `--heap` specifies **each** VM semispace; the image also reserves an
evacuation stack and code. Check that the assembled image fits your machine.

## Test

```sh
python3 vm-asm-study/test.py --lua lua \
  --assembler /path/to/tptasm.lua --emulator /path/to/r3emu
```

Ten cases run twice: five example programs, signed arithmetic, input,
references, repeated GC, and safe exhaustion. Tests check output, halt status,
GC activity, stack boundaries, and repeatable frame counts. Generated files
live in a temporary directory. `LUA`, `TPTASM`, and `R3EMU` can supply defaults.

## Measured results

Identical Blynn graphs, 768-word semispaces, 6144 physical RAM words,
`msfffmsfff` units, terminal on unit 9:

| Program | C VM frames | Assembly frames |
|---|---:|---:|
| hello | 2225 | 292 |
| print123 | 29615 | 4287 |
| branch | 2979 | 424 |
| arith_tests | 41675 | 5754 |
| factorial | 44861 | 6260 |

Assembly images occupy 3294 words versus 4036–4376 for C. Improvements include
preloading the graph and swapping semispace base pointers rather than arrays;
these are whole-runtime comparisons, not isolated C-codegen measurements.

Native headless TPT validation covered six reloaded-save runs, including
final assembly hello/factorial and C hello/print123. Every RAM word and display
pixel matched the emulator. This is bounded validation, not universal
correctness. Generated saves, dumps, traces and historical investigations are
not part of the source distribution.

Known differences: the existing Lua C backend lowers signed division/remainder
as unsigned; assembly passes all 13 directed signed checks. Assembly putchar
returns the character, whereas the C backend leaves its return register
unspecified. Exhaustion/error cases use explicit VM status codes.
