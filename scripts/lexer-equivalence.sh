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

dump_lua_tokens() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local Token = require("token")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local function hex_bytes(value)
    local out = {}
    for i = 1, #value do
        out[#out + 1] = string.format("%02x", string.byte(value, i))
    end
    return table.concat(out, "")
end

local function encode_value(value)
    if type(value) == "number" then
        return "N:" .. tostring(value)
    end
    return "S:" .. hex_bytes(tostring(value))
end

for _, token in ipairs(lexer.lex(code)) do
    print(table.concat({
        tostring(token.type),
        Token.INVERTED_TOKENS[token.type],
        encode_value(token.value),
        tostring(token.pos.row),
        tostring(token.pos.col),
    }, "\t"))
end
LUA
  )
}

cat > "$tmpdir/lexer-fixture.c" <<'C'
typedef unsigned long word_t;
auto_x register static typedef
int main(void) {
    char c = '\n';
    char slash = '\\';
    char *s = "a\n\\z";
    int x = 0x10u + 12u + 1.2;
    x <<= 1;
    x >>= 2;
    x += sizeof(word_t);
    if (x >= 10 && x != 12 || x <= 99) {
        asm("nop" "hlt" : : : r1, r2);
    }
    return x ? x : ~x;
}
// single-line comment
/* block
   comment */
C

cd "$repo_root"

for source in examples/*.c "$tmpdir/lexer-fixture.c"; do
  name="$(basename "$source" .c)"
  dump_lua_tokens "$source" > "$tmpdir/$name.lua.tokens"
  run_hs_compiler --dump-tokens "$source" > "$tmpdir/$name.hs.tokens"
  diff -u "$tmpdir/$name.lua.tokens" "$tmpdir/$name.hs.tokens"
done

echo "Native Haskell lexer matches Lua lexer for examples/*.c and focused fixture"
