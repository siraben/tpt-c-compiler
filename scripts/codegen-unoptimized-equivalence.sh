#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

lua="${TPTCC_LUA:-lua}"

run_hs_compiler() {
  if [[ -n "${TPTCC_HS:-}" ]]; then
    "$TPTCC_HS" "$@"
  else
    cabal run -v0 exe:tptcc-hs -- "$@"
  fi
}

cat > "$tmpdir/straight-line.c" <<'C'
int main(void) {
    int a = 1;
    int b = 2;
    return a + b;
}
C

cat > "$tmpdir/if-else.c" <<'C'
int main(void) {
    int a = 4;
    int b = 2;
    if (a > b) {
        a = a - b;
    } else {
        a = b - a;
    }
    return a;
}
C

cat > "$tmpdir/user-call.c" <<'C'
int add(int x, int y) {
    return x + y;
}

int main(void) {
    return add(2, 3);
}
C

cat > "$tmpdir/local-indexing.c" <<'C'
int main(void) {
    int values[3];
    values[0] = 1;
    values[1] = 2;
    return values[0] + values[1];
}
C

cat > "$tmpdir/global-literals.c" <<'C'
int values[] = {1, 2, 3};
char text[] = "ab";
char *names[] = {"cd", "ef"};

int main(void) {
    return values[1] + text[0] + names[0][0];
}
C

cat > "$tmpdir/global-expression.c" <<'C'
int value = 1 + 2;

int main(void) {
    return value;
}
C

cat > "$tmpdir/standard-call.c" <<'C'
int main(void) {
    putchar(65);
    return 0;
}
C

cat > "$tmpdir/all-stdlib.c" <<'C'
int slot;

int main(void) {
    __print_unsigned_int(3);
    __print_signed_int(-1);
    __print_char_array("x");
    putchar(65);
    getchar();
    getchar_nb();
    __scan_unsigned_int(&slot);
    set_colour(1, 2);
    set_text_colour(3);
    __send_raw(1, 2);
    __set_zero_char(1, 2, 3, 4);
    set_cursor(1, 2);
    vscroll();
    hscroll();
    set_terminal_mode(1);
    get_terminal_mode();
    plot(1, 2, 3);
    set_hrange(1, 2);
    set_vrange(1, 2);
    return 0;
}
C

dump_lua_unoptimized_asm() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local type_checker = require("type_checker")
local irv = require("ir")
local codegen = require("codegen")

codegen.optimized = false

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local ast = parser.parse(lexer.lex(code), symbol_table)
local checked, included_standard_functions = type_checker.type_check(ast, symbol_table)
local ir = irv.generate_ir_code(checked, {})
io.write(codegen:generate(ir, symbol_table, included_standard_functions))
LUA
  )
}

dump_lua_optimized_asm() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local type_checker = require("type_checker")
local irv = require("ir")
local codegen = require("codegen")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local ast = parser.parse(lexer.lex(code), symbol_table)
local checked, included_standard_functions = type_checker.type_check(ast, symbol_table)
local ir = irv.generate_ir_code(checked, {})
io.write(codegen:generate(ir, symbol_table, included_standard_functions))
LUA
  )
}

for fixture in straight-line if-else user-call local-indexing global-literals global-expression standard-call all-stdlib; do
  dump_lua_unoptimized_asm "$tmpdir/$fixture.c" > "$tmpdir/$fixture.lua.asm"
  run_hs_compiler --dump-native-asm-unoptimized "$tmpdir/$fixture.c" > "$tmpdir/$fixture.hs.asm"
  diff -u "$tmpdir/$fixture.lua.asm" "$tmpdir/$fixture.hs.asm"

  dump_lua_optimized_asm "$tmpdir/$fixture.c" > "$tmpdir/$fixture.lua.opt.asm"
  run_hs_compiler --dump-native-asm-optimized "$tmpdir/$fixture.c" > "$tmpdir/$fixture.hs.opt.asm"
  diff -u "$tmpdir/$fixture.lua.opt.asm" "$tmpdir/$fixture.hs.opt.asm"
done

for source in "$repo_root"/examples/*.c; do
  fixture="$(basename "$source" .c)"
  dump_lua_unoptimized_asm "$source" > "$tmpdir/example-$fixture.lua.asm"
  run_hs_compiler --dump-native-asm-unoptimized "$source" > "$tmpdir/example-$fixture.hs.asm"
  diff -u "$tmpdir/example-$fixture.lua.asm" "$tmpdir/example-$fixture.hs.asm"

  dump_lua_optimized_asm "$source" > "$tmpdir/example-$fixture.lua.opt.asm"
  run_hs_compiler --dump-native-asm-optimized "$source" > "$tmpdir/example-$fixture.hs.opt.asm"
  diff -u "$tmpdir/example-$fixture.lua.opt.asm" "$tmpdir/example-$fixture.hs.opt.asm"
done

echo "Native Haskell codegen matches Lua for focused fixtures and examples/*.c"
