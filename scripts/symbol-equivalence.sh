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

(
  cd "$repo_root"
  LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - <<'LUA' | sort > "$tmpdir/lua.symbols"
local symbol_table = require("symbol_table")
local Type = require("type")

local function bool_string(value)
    if value then return "true" end
    return "false"
end

local function value_string(value)
    if value == nil then return "" end
    return tostring(value)
end

for name, symbol in pairs(symbol_table.default_symbol_table.ordinary_symbols) do
    local place = symbol.place or {}
    print(table.concat({
        name,
        Type.to_string_pretty(symbol.type),
        value_string(place.type),
        value_string(place.value),
        bool_string(symbol.place and symbol.place.is_standard_function),
        place.is_variadic == nil and "" or bool_string(place.is_variadic),
    }, "\t"))
end
LUA

  run_hs_compiler --dump-default-symbols | sort > "$tmpdir/hs.symbols"
)

diff -u "$tmpdir/lua.symbols" "$tmpdir/hs.symbols"

echo "Native Haskell default symbol table matches Lua defaults"
