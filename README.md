# Celestial
A decompiler for vLuaU in luau, Roblox scripting language

WARNING: THIS CANNOT EXECUTE IN ANY OTHER LANGUAGES ONLY IN LUAU

This was made for decompiling vLua compiler bytecode, this is still in beta do not expect much.

# Features:

Upvalue decompiling
Function decompiling
Global decompiling
etc.

# Known errors:
May some times do a random function call in the end of the script
Cannot decompile for, while and repeat loops -- if it can decompile repeat loops then i am sorry for putting this here
May still cause errors since its still in beta

# Usage:

local decompile = require("Decompile")
local bytecode = "bytecodestring" -- this is an example this will not work
local code = decompile(bytecode)

If you want to use this in exploits ( I AM NOT RESPONSIBLE FOR ANY DAMAGES ) you can by utilizing the getscriptbytecode in certain enviroments

# Enviromental usage:
local decompile = require(script.Decompile)
local bytecode = getscriptbytecode(scriptPath --[[Replace with script path smth like game.ReplicatedFirst or smth like that]]) -- this will generate real bytecode
local code = decompile(bytecode) -- returns the code from that bytecode


