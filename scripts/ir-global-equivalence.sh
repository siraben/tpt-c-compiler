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

cat > "$tmpdir/ir-global-fixture.c" <<'C'
enum Mode { OFF = 0, ON = 1 };
int values[] = {1, 2, 3};
char text[] = "abc";
char *names[] = {"amy", "bob"};
int flag;

int main(void) {
    return values[0] + text[0] + flag + names[0][0];
}
C

dump_lua_ir_globals() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local type_checker = require("type_checker")
local irv = require("ir")
local Type = require("type")
local Node = require("node")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local ast = parser.parse(lexer.lex(code), symbol_table)
type_checker.type_check(ast, symbol_table)
local ir = irv.generate_ir_code(ast, {})

local function render_type(type_value)
    if type_value == nil then
        return "?"
    end
    if type_value.kind == nil and type_value.id ~= nil then
        return "ENUM"
    end
    return Type.to_string_pretty(type_value)
end

local function is_default_symbol(name)
    return symbol_table.default_symbol_table.ordinary_symbols[name] ~= nil
end

local function place_type(symbol)
    return symbol.place and symbol.place.type or ""
end

local function place_value(symbol)
    if symbol.place == nil then
        return ""
    end
    if symbol.place.type == "vr" then
        return "vr"
    end
    return tostring(symbol.place.value)
end

print("GLOBAL_SIZE\t" .. tostring(ir.global))

local data_keys = {}
for key, value in pairs(ir.global_data) do
    if type(key) == "number" then
        data_keys[#data_keys + 1] = key
    end
end
table.sort(data_keys)
for _, key in ipairs(data_keys) do
    print(table.concat({"DATA", key, tostring(ir.global_data[key])}, "\t"))
end

local symbol_keys = {}
for name, symbol in pairs(symbol_table.current_scope.ordinary_symbols) do
    if not is_default_symbol(name) and symbol.place ~= nil then
        symbol_keys[#symbol_keys + 1] = name
    end
end
table.sort(symbol_keys)
for _, name in ipairs(symbol_keys) do
    local symbol = symbol_table.current_scope.ordinary_symbols[name]
    print(table.concat({"SYMBOL", name, render_type(symbol.type), place_type(symbol), place_value(symbol)}, "\t"))
end

local method_keys = {}
for name, symbol in pairs(symbol_table.current_scope.ordinary_symbols) do
    if not is_default_symbol(name) and symbol.local_size ~= nil then
        method_keys[#method_keys + 1] = name
    end
end
table.sort(method_keys)
for _, name in ipairs(method_keys) do
    local symbol = symbol_table.current_scope.ordinary_symbols[name]
    print(table.concat({"METHOD", name, tostring(symbol.local_size)}, "\t"))
end

local function node_name(node)
    return Node.INVERTED_NODE_TYPES[node.type]
end

local function print_place(node, method_name, kind, name, type_value, handle)
    if handle ~= nil and handle.place ~= nil then
        print(table.concat({"PLACE", node.pos.row, node.pos.col, method_name, kind, name, render_type(type_value), place_type(handle), place_value(handle)}, "\t"))
    end
end

local walk_block
local walk_statement
local walk_declaration

local function walk_function(declaration)
    if not declaration.block then
        return
    end
    local declarator = declaration.declarators[1]
    local method_name = declarator.id.id
    if declarator.direct_declarator.parameter_list then
        for _, parameter in ipairs(declarator.direct_declarator.parameter_list) do
            print_place(parameter, method_name, "PARAM", parameter.declarator.id.id, parameter.value_type, parameter.handle)
        end
    end
    walk_block(method_name, declaration.block)
end

function walk_declaration(method_name, declaration)
    for _, declarator in ipairs(declaration.declarators or {}) do
        if declarator.handle ~= nil and declarator.value_type.kind ~= Type.KINDS["FUNCTION"] then
            print_place(declarator, method_name, "LOCAL", declarator.id.id, declarator.value_type, declarator.handle)
        end
    end
end

function walk_statement(method_name, statement)
    local child = statement.child
    local name = node_name(child)
    if name == "DECLARATION" then
        if child.specifier.storage_class.kind ~= "static" then
            walk_declaration(method_name, child)
        end
    elseif name == "IF" then
        walk_statement(method_name, child.true_case)
        if child.false_case then
            walk_statement(method_name, child.false_case)
        end
    elseif name == "BLOCK" then
        walk_block(method_name, child)
    elseif name == "FOR" then
        if child.initialization and node_name(child.initialization) == "DECLARATION" then
            walk_declaration(method_name, child.initialization)
        end
        walk_statement(method_name, child.statement)
    elseif name == "WHILE" then
        walk_statement(method_name, child.statement)
    elseif name == "SWITCH" then
        walk_block(method_name, child.block)
    elseif name == "CASE" then
        walk_statement(method_name, child.statement)
    elseif name == "DEFAULT" then
        walk_statement(method_name, child.statement)
    end
end

function walk_block(method_name, block)
    for _, statement in ipairs(block) do
        walk_statement(method_name, statement)
    end
end

for _, declaration in ipairs(ast) do
    walk_function(declaration)
end
LUA
  )
}

compare_ir_globals() {
  local source="$1"
  local name="$2"
  dump_lua_ir_globals "$source" > "$tmpdir/$name.lua.ir-globals"
  run_hs_compiler --dump-ir-globals "$source" > "$tmpdir/$name.hs.ir-globals"
  diff -u "$tmpdir/$name.lua.ir-globals" "$tmpdir/$name.hs.ir-globals"
}

compare_ir_globals "$tmpdir/ir-global-fixture.c" "ir-global-fixture"

for source in "$repo_root"/examples/*.c; do
  compare_ir_globals "$source" "$(basename "$source" .c)"
done

echo "Native Haskell IR global data, method local sizes, and parameter/local places match Lua for focused fixture and examples/*.c"
