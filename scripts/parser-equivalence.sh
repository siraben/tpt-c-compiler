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

cat > "$tmpdir/parser-fixture.c" <<'C'
enum Mode { OFF = 0, ON = 1 };
struct Pair { int x; int y; };
int global_value = 7;
char *message;

int main(void) {
    int local_value = 3 + 4 * 5;
    int array[3] = {1, 2, 3};
    register int (*pop_map)[30];
    int i = 0;
    local_value += (int)message;
    local_value += sizeof(int);
    local_value += sizeof(int[3]);
    while (i < 3) {
        local_value += array[i++];
        if (local_value > 20) {
            break;
        } else {
            continue;
        }
    }
    for (i = 0; i < 2; ++i) {
        putchar(message[i]);
    }
    switch (i) {
        case 0:
            local_value += 1;
        default:
            local_value += 2;
    }
    asm("nop" : r1=local_value : r2=i : r3);
    putchar(message[0]);
    local_value = local_value << 1;
    return local_value >= 10 ? local_value : 0;
}
C

dump_lua_ast() {
  local source="$1"
  (
    cd "$repo_root"
    LUA_PATH="$repo_root/?.lua;$repo_root/?/init.lua;${LUA_PATH:-}" "$lua" - "$source" <<'LUA'
local lexer = require("lexer")
local parser = require("parser")
local symbol_table = require("symbol_table")
local Node = require("node")
local Token = require("token")

local path = arg[1]
local file = assert(io.open(path, "r"))
local code = file:read("*all")
file:close()

local ast = parser.parse(lexer.lex(code), symbol_table)

local function pad(indent)
    return string.rep(" ", indent * 2)
end

local function quoted(value)
    local text = tostring(value)
    text = text:gsub("\\", "\\\\")
    text = text:gsub("\"", "\\\"")
    text = text:gsub("\n", "\\n")
    text = text:gsub("\r", "\\r")
    text = text:gsub("\t", "\\t")
    return "\"" .. text .. "\""
end

local function sorted_keys(node)
    local keys = {}
    for key, _ in pairs(node) do
        if type(key) == "string" and key ~= "type" and key ~= "pos" then
            keys[#keys + 1] = key
        end
    end
    table.sort(keys)
    return keys
end

local function is_node(value)
    local mt = type(value) == "table" and getmetatable(value)
    return type(mt) == "table" and mt.is_node == true
end

local function is_token(value)
    return type(value) == "table" and type(value.type) == "number" and Token.INVERTED_TOKENS[value.type] ~= nil
end

local function is_postfix_op(value)
    return type(value) == "table" and type(value.type) == "string"
end

local function render_token_value(value)
    if type(value) == "number" then
        return tostring(value)
    end
    return quoted(value)
end

local function render_string_list(values)
    local out = {}
    for _, value in ipairs(values) do
        out[#out + 1] = quoted(value)
    end
    return "[" .. table.concat(out, ",") .. "]"
end

local function render_int_list(values)
    local out = {}
    for _, value in ipairs(values) do
        out[#out + 1] = tostring(value)
    end
    return "[" .. table.concat(out, ",") .. "]"
end

local dump_node

local function dump_list(indent, values)
    for index, child in ipairs(values) do
        print(pad(indent + 1) .. "I " .. index)
        if is_node(child) then
            dump_node(child, indent + 2)
        elseif is_token(child) then
            print(pad(indent + 2) .. "T " .. Token.INVERTED_TOKENS[child.type] .. " " .. render_token_value(child.value))
        elseif is_postfix_op(child) then
            print(pad(indent + 2) .. "O " .. child.type)
            if child.value ~= nil then
                if is_node(child.value) then
                    print(pad(indent + 3) .. "V node")
                    dump_node(child.value, indent + 4)
                elseif type(child.value) == "string" then
                    print(pad(indent + 3) .. "V string " .. quoted(child.value))
                elseif type(child.value) == "number" then
                    print(pad(indent + 3) .. "V int " .. tostring(child.value))
                elseif type(child.value) == "boolean" then
                    print(pad(indent + 3) .. "V bool " .. tostring(child.value))
                end
            end
        end
    end
end

local function dump_field(node, key, indent)
    local value = node[key]
    if key == "kind" and is_node(value) then
        print(pad(indent + 1) .. "F " .. key .. " node")
        dump_node(value, indent + 2)
    elseif key == "kind" and type(value) == "table" then
        print(pad(indent + 1) .. "F " .. key .. " strings " .. render_string_list(value))
    elseif key == "dimensions" and type(value) == "table" then
        print(pad(indent + 1) .. "F " .. key .. " ints " .. render_int_list(value))
    elseif is_node(value) then
        print(pad(indent + 1) .. "F " .. key .. " node")
        dump_node(value, indent + 2)
    elseif type(value) == "table" then
        print(pad(indent + 1) .. "F " .. key .. " list")
        dump_list(indent + 1, value)
    elseif type(value) == "string" then
        print(pad(indent + 1) .. "F " .. key .. " string " .. quoted(value))
    elseif type(value) == "number" then
        print(pad(indent + 1) .. "F " .. key .. " int " .. tostring(value))
    elseif type(value) == "boolean" then
        print(pad(indent + 1) .. "F " .. key .. " bool " .. tostring(value))
    end
end

function dump_node(node, indent)
    print(pad(indent) .. "N " .. Node.INVERTED_NODE_TYPES[node.type])
    dump_list(indent, node)
    for _, key in ipairs(sorted_keys(node)) do
        dump_field(node, key, indent)
    end
end

dump_node(ast, 0)
LUA
  )
}

compare_parser() {
  local source="$1"
  local name="$2"
  dump_lua_ast "$source" > "$tmpdir/$name.lua.ast"
  run_hs_compiler --dump-ast "$source" > "$tmpdir/$name.hs.ast"
  diff -u "$tmpdir/$name.lua.ast" "$tmpdir/$name.hs.ast"
}

compare_parser "$tmpdir/parser-fixture.c" "parser-fixture"

for source in "$repo_root"/examples/*.c; do
  compare_parser "$source" "$(basename "$source" .c)"
done

echo "Native Haskell parser matches Lua parser for focused fixture and examples/*.c"
