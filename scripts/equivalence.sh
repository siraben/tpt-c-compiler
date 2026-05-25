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

cd "$repo_root"

mkdir -p "$tmpdir/lua" "$tmpdir/hs"

for source in examples/*.c; do
  name="$(basename "$source" .c)"
  "$lua" cli.lua "$source" --output "$tmpdir/lua/$name.asm"
  run_hs_compiler "$source" --output "$tmpdir/hs/$name.asm"
  diff -u "$tmpdir/lua/$name.asm" "$tmpdir/hs/$name.asm"
done

cat > "$tmpdir/debug-options.c" <<'C'
int f(int x) {
    return x + 1;
}

int main(void) {
    int a = 1;
    a = f(a);
    return a;
}
C

"$lua" cli.lua "$tmpdir/debug-options.c" --output "$tmpdir/lua/debug-breakpoints.asm" --breakpoints "[6, 7]" > "$tmpdir/lua/debug-breakpoints.out"
run_hs_compiler "$tmpdir/debug-options.c" --output "$tmpdir/hs/debug-breakpoints.asm" --breakpoints "[6, 7]" > "$tmpdir/hs/debug-breakpoints.out"
diff -u "$tmpdir/lua/debug-breakpoints.asm" "$tmpdir/hs/debug-breakpoints.asm"
diff -u "$tmpdir/lua/debug-breakpoints.out" "$tmpdir/hs/debug-breakpoints.out"

"$lua" cli.lua "$tmpdir/debug-options.c" --output "$tmpdir/lua/symbols.asm" --symbols "$tmpdir/lua/symbols.json" > "$tmpdir/lua/symbols.out"
run_hs_compiler "$tmpdir/debug-options.c" --output "$tmpdir/hs/symbols.asm" --symbols "$tmpdir/hs/symbols.json" > "$tmpdir/hs/symbols.out"
diff -u "$tmpdir/lua/symbols.asm" "$tmpdir/hs/symbols.asm"
diff -u "$tmpdir/lua/symbols.out" "$tmpdir/hs/symbols.out"
if [[ -e "$tmpdir/lua/symbols.json" || -e "$tmpdir/hs/symbols.json" ]]; then
  echo "Unexpected symbols JSON output in no-dkjson environment" >&2
  exit 1
fi

cat > "$tmpdir/r3-smoke.c" <<'C'
int main() {
    return 0;
}
C

run_hs_compiler "$tmpdir/r3-smoke.c" --output "$tmpdir/r3-smoke.asm"

if [[ -f "$r3emu_root/tests/tptasm.lua" ]]; then
  (
    cd "$r3emu_root/tests"
    "$lua" - <<LUA
_G.unpack = _G.unpack or table.unpack
tptasm = loadfile("tptasm.lua")
local exit_code = tptasm("$tmpdir/r3-smoke.asm", "$tmpdir/r3-smoke.bin", nil, "R3A0564")
if exit_code ~= 0 then os.exit(exit_code) end
LUA
  )
else
  echo "R3emu assembler not found at $r3emu_root/tests/tptasm.lua" >&2
  exit 1
fi

if [[ ! -s "$tmpdir/r3-smoke.bin" ]]; then
  echo "R3emu assembler did not produce $tmpdir/r3-smoke.bin" >&2
  exit 1
fi

if [[ -n "${R3EMU_BIN:-}" ]]; then
  r3emu="$R3EMU_BIN"
elif command -v r3emu >/dev/null 2>&1; then
  r3emu="$(command -v r3emu)"
elif [[ -x "$r3emu_root/result/bin/r3emu" ]]; then
  r3emu="$r3emu_root/result/bin/r3emu"
elif [[ -x "$r3emu_root/target/release/r3emu" ]]; then
  r3emu="$r3emu_root/target/release/r3emu"
else
  nix build "$r3emu_root" --out-link "$tmpdir/r3emu-result"
  r3emu="$tmpdir/r3emu-result/bin/r3emu"
fi

timeout "${R3EMU_TIMEOUT:-10s}" "$r3emu" "$tmpdir/r3-smoke.bin" --headless --stdout

echo "Haskell/Lua assembly equivalence passed for examples/*.c and debug CLI options"
echo "R3emu smoke test passed for $tmpdir/r3-smoke.bin"
