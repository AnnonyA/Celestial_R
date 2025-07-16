# Celestial
A decompiler for vLuaU in luau, Roblox scripting language

WARNING: THIS CANNOT EXECUTE IN ANY OTHER LANGUAGES ONLY IN LUAU

This was made for decompiling vLua compiler bytecode, this is still in beta do not expect much.

# Features:

Upvalue decompiling
Function decompiling
Global decompiling
Numerical For loop decompiling (PARTIAL SUPPORT: still gives some errors)
etc.

# Known errors:
Cannot decompile while and repeat loops, May cause some errors since its in beta

# Usage:

local decompile = require("Decompile")
local bytecode = "bytecodestring" -- this is an example this will not work
local code = decompile(bytecode)

If you want to use this in exploits ( I AM NOT RESPONSIBLE FOR ANY DAMAGES ) you can by utilizing the getscriptbytecode in certain enviroments

# Enviromental usage:
local decompile = require(script.Decompile)
local bytecode = getscriptbytecode(scriptPath --[[Replace with script path smth like game.ReplicatedFirst or smth like that]]) -- this will generate real bytecode
local code = decompile(bytecode) -- returns the code from that bytecode


