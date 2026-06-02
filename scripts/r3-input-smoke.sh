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
if exit_code ~= 0 then os.exit(exit_code) end
LUA
  ) >/dev/null
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

run_keyboard_case() {
  local asm="$tmpdir/input.asm"
  local bin="$tmpdir/input.bin"
  local out="$tmpdir/input.out"

  run_hs_compiler "$tmpdir/input.c" --output "$asm" >/dev/null

  assemble_with_r3 "$asm" "$bin"
  timeout "${R3EMU_TIMEOUT:-10s}" "$r3emu" "$bin" --headless --stdout --keyboard-input xy >"$out"

  local actual
  actual="$(tr -d '\r\n' <"$out")"
  if [[ "$actual" != "xy0" ]]; then
    echo "R3 keyboard smoke failed: expected xy0, got '$actual'" >&2
    exit 1
  fi
}

cat >"$tmpdir/input.c" <<'C'
int main(void) {
    char a = getchar();
    char b = getchar_nb();
    char c = getchar_nb();
    putchar(a);
    putchar(b);
    if (c == 0) {
        putchar('0');
    } else {
        putchar(c);
    }
    return 0;
}
C

cd "$repo_root"
r3emu="$(find_r3emu)"

run_keyboard_case

printf 'filez' >"$tmpdir/keys.txt"
run_hs_compiler "$tmpdir/input.c" --output "$tmpdir/input-file.asm" >/dev/null
assemble_with_r3 "$tmpdir/input-file.asm" "$tmpdir/input-file.bin"
timeout "${R3EMU_TIMEOUT:-10s}" "$r3emu" "$tmpdir/input-file.bin" --headless --stdout --keyboard-input-file "$tmpdir/keys.txt" >"$tmpdir/input-file.out"
file_actual="$(tr -d '\r\n' <"$tmpdir/input-file.out")"
if [[ "$file_actual" != "fil" ]]; then
  echo "R3 keyboard input-file smoke failed: expected fil, got '$file_actual'" >&2
  exit 1
fi

echo "R3 keyboard input smoke passed for direct and file-backed input"
