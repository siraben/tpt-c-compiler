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
    int c;
    c = a + b * 3;
    return c;
}
C

cat > "$tmpdir/bitwise-shift.c" <<'C'
int main(void) {
    int a = 7;
    int b = 3;
    int c;
    c = (a & b) | (a ^ 2);
    c = c << 1;
    c = c >> 2;
    return c;
}
C

cat > "$tmpdir/bool-unary.c" <<'C'
int main(void) {
    int a = 7;
    int b = 3;
    int c;
    c = a < b;
    c = a == b;
    c = (a < b) || (b != 0);
    c = !~(-a);
    c = ++a;
    c = --a;
    return c;
}
C

cat > "$tmpdir/if-else.c" <<'C'
int main(void) {
    int a = 7;
    int b = 3;
    int c;
    if (a > b) {
        c = a - b;
    } else {
        c = b - a;
    }
    if (c && a != 0)
        c = c + 1;
    return c;
}
C

cat > "$tmpdir/while.c" <<'C'
int main(void) {
    int a = 0;
    int b = 3;
    while (a < b) {
        a = a + 1;
    }
    return a;
}
C

cat > "$tmpdir/for.c" <<'C'
int main(void) {
    int sum = 0;
    for (int i = 0; i < 4; i = i + 1) {
        sum = sum + i;
    }
    for (; sum < 10; sum = sum + 1)
        sum = sum + 2;
    return sum;
}
C

cat > "$tmpdir/break-continue.c" <<'C'
int main(void) {
    int sum = 0;
    for (int i = 0; i < 5; i = i + 1) {
        if (i == 1)
            continue;
        if (i == 4)
            break;
        sum = sum + i;
    }
    while (sum < 10) {
        sum = sum + 1;
        continue;
    }
    return sum;
}
C

cat > "$tmpdir/ternary-postfix.c" <<'C'
int main(void) {
    int a = 1;
    int b = 2;
    int c;
    c = a ? b++ : a--;
    c = (a < b) ? a + b : b--;
    return c;
}
C

cat > "$tmpdir/compound-assignment.c" <<'C'
int main(void) {
    int a = 3;
    int b = 5;
    a += b;
    a -= 2;
    a *= b;
    a &= 7;
    a |= 1;
    a ^= b;
    a <<= 1;
    a >>= 2;
    return a;
}
C

cat > "$tmpdir/division-remainder.c" <<'C'
int main(void) {
    int a = 37;
    int b = 5;
    int c;
    c = a / 3;
    c = a % 3;
    c = a / b;
    c = a % b;
    a /= 3;
    a %= b;
    return c + a;
}
C

cat > "$tmpdir/function-call.c" <<'C'
int add(int x, int y) {
    int z = x + y;
    return z;
}

int main(void) {
    int a = add(2, 3);
    return a;
}
C

cat > "$tmpdir/standard-call.c" <<'C'
int main(void) {
    putchar('A');
    set_cursor(1, 2);
    int mode = get_terminal_mode();
    return mode;
}
C

cat > "$tmpdir/pointer-deref.c" <<'C'
int main(void) {
    int a = 7;
    int b = 0;
    int *p;
    p = &a;
    b = *p;
    *p = b + 2;
    return a;
}
C

cat > "$tmpdir/indexing.c" <<'C'
int main(void) {
    int values[4];
    int *p;
    values[0] = 1;
    values[2] = 7;
    p = &values[0];
    p[1] = values[2] + 3;
    return values[1];
}
C

cat > "$tmpdir/multidim-indexing.c" <<'C'
int values[3][4];

int main(void) {
    register int row = 1;
    register int col = 2;
    values[row][col] = 7;
    return values[row][col];
}
C

cat > "$tmpdir/cast-pointer-indexing.c" <<'C'
int values[4];

int main(void) {
    register int base = (int)values;
    register int offset = 2;
    ((int *)(base + offset))[0] = 9;
    return ((int *)(base + offset))[0];
}
C

cat > "$tmpdir/register-pointer-array.c" <<'C'
int values[2][3];

int main(void) {
    register int (*p)[3] = values;
    register int row = 1;
    register int col = 2;
    p[row][col] = 5;
    return p[row][col];
}
C

cat > "$tmpdir/switch-case.c" <<'C'
int main(void) {
    int a = 3;
    int b = 0;
    switch (a) {
        case 1:
            b = 10;
            break;
        case 3:
            b = 30;
            break;
        default:
            b = 99;
            break;
    }
    return b;
}
C

cat > "$tmpdir/inline-asm.c" <<'C'
int main(void) {
    int a = 2;
    int b = 5;
    asm(
        "add r1, r2"
        :r1=a
        :r2=b
        :r3, r4
    );
    return b;
}
C

cat > "$tmpdir/global-objects.c" <<'C'
int g;
int values[4];
int init = 7;

int main(void) {
    g = init + 1;
    values[2] = g;
    return values[2];
}
C

cat > "$tmpdir/string-literal.c" <<'C'
void use(char *s) {
}

char *words[] = {"hi", "bye"};

int main(void) {
    use("ok");
    return words[1][0];
}
C

cat > "$tmpdir/function-string-before-global.c" <<'C'
void use(char *s) {
}

void banner(void) {
    use("abc");
}

int later[2];

int main(void) {
    later[1] = 7;
    banner();
    return later[1];
}
C

cat > "$tmpdir/enum-constant.c" <<'C'
enum Mode {ZERO, TWO = 2, ALSO_TWO};

int main(void) {
    return ZERO + TWO + ALSO_TWO;
}
C

cat > "$tmpdir/global-expression-initializer.c" <<'C'
enum Mode {ZERO, ONE};
enum Mode mode = ONE;

int main(void) {
    register int value = 3;
    return value;
}
C

cat > "$tmpdir/register-local.c" <<'C'
int main(void) {
    register int a;
    register int b = 3;
    register int c = 4;
    a = b + c;
    b++;
    return a + b;
}
C

dump_lua_simple_tac() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local type_checker = require("type_checker")
local irv = require("ir")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local ast = parser.parse(lexer.lex(code), symbol_table)
type_checker.type_check(ast, symbol_table)
local ir = irv.generate_ir_code(ast, {})

local function render_place(place)
    return place.type .. ":" .. tostring(place.value)
end

for _, method_id in ipairs(ir.tac) do
    print("METHOD\t" .. method_id)
    for index, instr in ipairs(ir.tac[method_id]) do
        local parts = {tostring(index), instr.type}
        for _, key in ipairs({"source", "dest", "target", "first", "second", "third", "offset"}) do
            if instr[key] then
                parts[#parts + 1] = key .. "=" .. render_place(instr[key])
            end
        end
        if instr.asm then
            parts[#parts + 1] = "asm=" .. instr.asm
        end
        print(table.concat(parts, "\t"))
    end
end

print("LOCAL_SIZE\t" .. tostring(symbol_table.current_scope.ordinary_symbols.main.local_size))
LUA
  )
}

for fixture in straight-line bitwise-shift bool-unary if-else while for break-continue ternary-postfix compound-assignment division-remainder function-call standard-call pointer-deref indexing multidim-indexing cast-pointer-indexing register-pointer-array switch-case inline-asm global-objects string-literal function-string-before-global enum-constant global-expression-initializer register-local; do
  dump_lua_simple_tac "$tmpdir/$fixture.c" > "$tmpdir/$fixture.lua"
  run_hs_compiler --dump-simple-tac "$tmpdir/$fixture.c" > "$tmpdir/$fixture.hs"
  diff -u "$tmpdir/$fixture.lua" "$tmpdir/$fixture.hs"
done

for source in "$repo_root"/examples/*.c; do
  fixture="$(basename "$source" .c)"
  dump_lua_simple_tac "$source" > "$tmpdir/example-$fixture.lua"
  run_hs_compiler --dump-simple-tac "$source" > "$tmpdir/example-$fixture.hs"
  diff -u "$tmpdir/example-$fixture.lua" "$tmpdir/example-$fixture.hs"
done

echo "Native Haskell simple TAC matches Lua for focused fixtures and examples/*.c"
