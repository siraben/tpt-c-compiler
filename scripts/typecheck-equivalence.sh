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

cat > "$tmpdir/typecheck-fixture.c" <<'C'
enum Mode { OFF = 0, ON = 1 };
struct Pair { int x; int y; };
int values[] = {1, 2, 3};
char text[] = "abc";
int add(int lhs, int rhs);

int add(int lhs, int rhs) {
    int total = lhs + rhs;
    {
        register int shadow = total;
        total = shadow;
    }
    for (register int i = 0; i < 3; ++i) {
        total += values[i];
    }
    return total;
}
C

dump_lua_type_events() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local type_checker = require("type_checker")
local Type = require("type")
local Node = require("node")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local function render_type(type_value)
    if type_value == nil then
        return "?"
    end
    if type_value.kind == nil and type_value.id ~= nil then
        return "ENUM"
    end
    return Type.to_string_pretty(type_value)
end

local function current_namespace(namespace)
    if namespace == symbol_table.tag then
        return symbol_table.current_scope.tag_symbols
    end
    return symbol_table.current_scope.ordinary_symbols
end

local ast = parser.parse(lexer.lex(code), symbol_table)

local original_add_symbol = symbol_table.add_symbol
local original_new_scope = symbol_table.new_scope
local original_exit_scope = symbol_table.exit_scope

function symbol_table.add_symbol(id, symbol, namespace)
    local before = current_namespace(namespace)[id]
    original_add_symbol(id, symbol, namespace)
    local after = current_namespace(namespace)[id]
    if before == nil and after ~= nil then
        print(table.concat({"ADD", symbol_table.current_scope.level, symbol_table.current_scope.name or "global", namespace, id, render_type(after.type)}, "\t"))
    end
end

function symbol_table.new_scope(id)
    original_new_scope(id)
    print(table.concat({"ENTER", symbol_table.current_scope.level, symbol_table.current_scope.name}, "\t"))
end

function symbol_table.exit_scope()
    print(table.concat({"EXIT", symbol_table.current_scope.level, symbol_table.current_scope.name}, "\t"))
    original_exit_scope()
end

type_checker.type_check(ast, symbol_table)

local function is_node(value)
    local mt = type(value) == "table" and getmetatable(value)
    return type(mt) == "table" and mt.is_node == true
end

local function is_postfix_op(value)
    return type(value) == "table" and type(value.type) == "string"
end

local function sorted_keys(node)
    local keys = {}
    for key, _ in pairs(node) do
        if type(key) == "string" and key ~= "type" and key ~= "pos" and key ~= "value_type" and key ~= "value_types" and key ~= "handle" then
            keys[#keys + 1] = key
        end
    end
    table.sort(keys)
    return keys
end

local dump_type_nodes

local function dump_type_list(values)
    for _, child in ipairs(values) do
        if is_node(child) then
            dump_type_nodes(child)
        elseif is_postfix_op(child) and is_node(child.value) then
            dump_type_nodes(child.value)
        end
    end
end

function dump_type_nodes(node)
    if node.value_type ~= nil then
        print(table.concat({"TYPE", node.pos.row, node.pos.col, Node.INVERTED_NODE_TYPES[node.type], render_type(node.value_type)}, "\t"))
    end
    dump_type_list(node)
    for _, key in ipairs(sorted_keys(node)) do
        local value = node[key]
        if is_node(value) then
            dump_type_nodes(value)
        elseif type(value) == "table" then
            dump_type_list(value)
        end
    end
end

dump_type_nodes(ast)
LUA
  )
}

compare_typecheck() {
  local source="$1"
  local name="$2"
  dump_lua_type_events "$source" > "$tmpdir/$name.lua.types"
  run_hs_compiler --dump-type-events "$source" > "$tmpdir/$name.hs.types"
  diff -u "$tmpdir/$name.lua.types" "$tmpdir/$name.hs.types"
}

compare_typecheck "$tmpdir/typecheck-fixture.c" "typecheck-fixture"

for source in "$repo_root"/examples/*.c; do
  compare_typecheck "$source" "$(basename "$source" .c)"
done

echo "Native Haskell type construction, symbol-scope events, and expression value types match Lua for focused fixture and examples/*.c"
