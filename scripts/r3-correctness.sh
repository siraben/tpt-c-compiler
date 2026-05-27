#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

lua="${TPTCC_LUA:-lua}"
r3emu_root="${R3EMU_ROOT:-$HOME/R3emu}"

run_hs_compiler() {
  if [[ -n "${TPTCC_HS:-}" ]]; then
    "$TPTCC_HS" "$@"
  else
    cabal run -v0 exe:tptcc-hs -- "$@"
  fi
}

assemble_with_r3() {
  local asm="$1"
  local bin="$2"

  if [[ ! -f "$r3emu_root/tests/tptasm.lua" ]]; then
    echo "R3emu assembler not found at $r3emu_root/tests/tptasm.lua" >&2
    exit 1
  fi

  (
    cd "$r3emu_root/tests"
    "$lua" - <<LUA
_G.unpack = _G.unpack or table.unpack
tptasm = loadfile("tptasm.lua")
local exit_code = tptasm("$asm", "$bin", nil, "R3A0564")
if exit_code and exit_code ~= 0 then os.exit(exit_code) end
LUA
  ) >/dev/null

  if [[ ! -s "$bin" ]]; then
    echo "R3 assembler did not produce $bin from $asm" >&2
    exit 1
  fi
}

find_r3emu() {
  if [[ -n "${R3EMU_BIN:-}" ]]; then
    printf '%s\n' "$R3EMU_BIN"
  elif command -v r3emu >/dev/null 2>&1; then
    command -v r3emu
  elif [[ -x "$r3emu_root/target/debug/r3emu" ]]; then
    printf '%s\n' "$r3emu_root/target/debug/r3emu"
  elif [[ -x "$r3emu_root/result/bin/r3emu" ]]; then
    printf '%s\n' "$r3emu_root/result/bin/r3emu"
  elif [[ -x "$r3emu_root/target/release/r3emu" ]]; then
    printf '%s\n' "$r3emu_root/target/release/r3emu"
  else
    nix build "$r3emu_root" --out-link "$tmpdir/r3emu-result" >/dev/null
    printf '%s\n' "$tmpdir/r3emu-result/bin/r3emu"
  fi
}

write_fixtures() {
  cat >"$tmpdir/arithmetic-divrem.c" <<'C'
int main(void) {
    int a = 37;
    int b = 5;
    putchar('0' + (a / 3));
    putchar('0' + (a % 3));
    putchar('0' + (a / b));
    putchar('0' + (a % b));
    return 0;
}
C

  cat >"$tmpdir/variable-divrem.c" <<'C'
int main(void) {
    int total = 0;
    for (int i = 1; i < 6; i = i + 1) {
        total = total + i;
    }
    putchar('0' + (total / 4));
    putchar('0' + (total % 4));
    return 0;
}
C

  cat >"$tmpdir/recursion-and-calls.c" <<'C'
int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int mix(int x, int y, int z) {
    return x * 2 + y - z;
}

int main(void) {
    int a = fib(6);
    int b = mix(4, 7, 3);
    putchar('0' + a);
    putchar('0' + (b % 10));
    return 0;
}
C

  cat >"$tmpdir/short-circuit.c" <<'C'
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
C

  cat >"$tmpdir/arrays-pointers.c" <<'C'
int main(void) {
    int values[5];
    int *p;
    values[0] = 2;
    values[1] = 3;
    values[2] = 4;
    p = &values[0];
    p[3] = p[0] + p[1] + p[2];
    *(p + 4) = p[3] - p[1];
    putchar('0' + values[3]);
    putchar('0' + values[4]);
    return 0;
}
C

  cat >"$tmpdir/global-matrix.c" <<'C'
int matrix[3][4];
int seed = 2;

int main(void) {
    register int row = 1;
    register int col = 2;
    matrix[row][col] = seed + row + col;
    matrix[2][3] = matrix[row][col] + 1;
    putchar('0' + matrix[row][col]);
    putchar('0' + matrix[2][3]);
    return 0;
}
C

  cat >"$tmpdir/switch-loop.c" <<'C'
int main(void) {
    int sum = 0;
    for (int i = 0; i < 8; i = i + 1) {
        switch (i) {
        case 0:
            sum = sum + 1;
            break;
        case 1:
        case 2:
            continue;
        case 5:
            break;
        default:
            sum = sum + i;
        }
        if (sum > 9) {
            break;
        }
    }
    putchar('0' + sum);
    return 0;
}
C

  cat >"$tmpdir/compound-ternary.c" <<'C'
int main(void) {
    int a = 3;
    int b = 4;
    int c = a++ + ++b;
    c += a > b ? a : b;
    c *= 2;
    c >>= 1;
    c ^= 3;
    putchar('0' + (a % 10));
    putchar('0' + (b % 10));
    putchar('0' + (c % 10));
    return 0;
}
C

  cat >"$tmpdir/input-flow.c" <<'C'
int main(void) {
    char a = getchar();
    char b = getchar_nb();
    char c = getchar_nb();
    if (a >= 'a' && a <= 'z') {
        putchar(a - 32);
    } else {
        putchar(a);
    }
    putchar(b);
    if (c == 0) {
        putchar('0');
    } else {
        putchar(c);
    }
    return 0;
}
C
}

run_case() {
  local name="$1"
  local expected="$2"
  local keyboard_input="${3:-}"
  local modes="${4:-optimized}"

  for mode in $modes; do
    local asm="$tmpdir/$name-$mode.asm"
    local bin="$tmpdir/$name-$mode.bin"
    local out="$tmpdir/$name-$mode.out"

    case "$mode" in
      optimized)
        run_hs_compiler "$tmpdir/$name.c" --output "$asm" >/dev/null
        ;;
      unoptimized)
        run_hs_compiler --dump-native-asm-unoptimized "$tmpdir/$name.c" >"$asm"
        ;;
    esac

    assemble_with_r3 "$asm" "$bin"
    if [[ -n "$keyboard_input" ]]; then
      timeout "${R3EMU_TIMEOUT:-10s}" "$r3emu" "$bin" --headless --stdout --keyboard-input "$keyboard_input" >"$out"
    else
      timeout "${R3EMU_TIMEOUT:-10s}" "$r3emu" "$bin" --headless --stdout >"$out"
    fi

    local actual
    actual="$(tr -d '\r\n' <"$out")"
    if [[ "$actual" != "$expected" ]]; then
      echo "R3 correctness failed for $name/$mode: expected '$expected', got '$actual'" >&2
      exit 1
    fi

    printf '%s/%s => %s\n' "$name" "$mode" "$actual"
  done
}

cd "$repo_root"
r3emu="$(find_r3emu)"
write_fixtures

run_case arithmetic-divrem "<172" "" "optimized unoptimized"
run_case variable-divrem "33" "" "optimized unoptimized"
run_case recursion-and-calls "82" "" "optimized unoptimized"
run_case short-circuit "42" "" "optimized unoptimized"
run_case arrays-pointers "96"
run_case global-matrix "56"
run_case switch-loop ">"
run_case compound-ternary "454"
run_case input-flow "Ab0" "ab" "optimized unoptimized"

echo "R3 C correctness suite passed"
