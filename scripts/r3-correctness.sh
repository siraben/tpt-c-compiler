#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

lua="${TPTCC_LUA:-lua}"
r3emu_root="${R3EMU_ROOT:-$HOME/R3emu}"
fixtures_dir="$repo_root/tests/r3-correctness"

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

run_case() {
  local name="$1"
  local expected="$2"
  local keyboard_input="${3:-}"
  local modes="${4:-optimized}"
  local source="$fixtures_dir/$name.c"

  if [[ ! -f "$source" ]]; then
    echo "R3 correctness fixture not found: $source" >&2
    exit 1
  fi

  for mode in $modes; do
    local asm="$tmpdir/$name-$mode.asm"
    local bin="$tmpdir/$name-$mode.bin"
    local out="$tmpdir/$name-$mode.out"

    case "$mode" in
      optimized)
        run_hs_compiler "$source" --output "$asm" >/dev/null
        ;;
      unoptimized)
        run_hs_compiler --dump-native-asm-unoptimized "$source" >"$asm"
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

run_case arithmetic-divrem "<172" "" "optimized unoptimized"
run_case variable-divrem "33" "" "optimized unoptimized"
run_case recursion-and-calls "82" "" "optimized unoptimized"
run_case short-circuit "42" "" "optimized unoptimized"
run_case arrays-pointers "96"
run_case global-matrix "56"
run_case switch-loop ">"
run_case do-while-empty "8" "" "optimized unoptimized"
run_case compound-ternary "454"
run_case input-flow "Ab0" "ab" "optimized unoptimized"
run_case function-pointers "53" "" "optimized unoptimized"
run_case struct-members "8" "" "optimized unoptimized"
run_case union-members "6" "" "optimized unoptimized"
run_case enum-switch "36" "" "optimized unoptimized"

echo "R3 C correctness suite passed"
