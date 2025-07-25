
local string_format = string.format

local function cerror(chunk, reason)
	return string_format("--CELESTIALERROR[%s]: Failed to decompile reason: %s\nC\nC\nC\nC\nC\nC\nC\nC\nC", chunk, reason)
end

-- opcode list pls dont remove it you can add more but if you remove the code stops working
local opList = {
  NOP               = 0,
  BREAK             = 1,
  LOADNIL           = 2,
  LOADB             = 3,
  LOADN             = 4,
  LOADK             = 5,
  MOVE              = 6,
  GETGLOBAL         = 7,
  SETGLOBAL         = 8,
  GETUPVAL          = 9,
  SETUPVAL          = 10,
  CLOSEUPVALS       = 11,
  GETIMPORT         = 12,
  GETTABLE          = 13,
  SETTABLE          = 14,
  GETTABLEKS        = 15,
  SETTABLEKS        = 16,
  GETTABLEN         = 17,
  SETTABLEN         = 18,
  NEWCLOSURE        = 19,
  NAMECALL          = 20,
  CALL              = 21,
  RETURN            = 22,
  JUMP              = 23,
  JUMPBACK          = 24,
  JUMPIF            = 25,
  JUMPIFNOT         = 26,
  JUMPIFEQ          = 27,
  JUMPIFLE          = 28,
  JUMPIFLT          = 29,
  JUMPIFNOTEQ       = 30,
  JUMPIFNOTLE       = 31,
  JUMPIFNOTLT       = 32,
  ADD               = 33,
  SUB               = 34,
  MUL               = 35,
  DIV               = 36,
  MOD               = 37,
  POW               = 38,
  ADDK              = 39,
  SUBK              = 40,
  MULK              = 41,
  DIVK              = 42,
  MODK              = 43,
  POWK              = 44,
  AND               = 45,
  OR                = 46,
  ANDK              = 47,
  ORK               = 48,
  CONCAT            = 49,
  NOT               = 50,
  MINUS             = 51,
  LENGTH            = 52,
  NEWTABLE          = 53,
  DUPTABLE          = 54,
  SETLIST           = 55,
  FORNPREP          = 56,
  FORNLOOP          = 57,
  FORGLOOP          = 58,
  FORGPREP_INEXT    = 59,
  FASTCALL3         = 60,
  FORGPREP_NEXT     = 61,
  PREPVARARGS       = 62,
  GETVARARGS        = 63,
  DUPCLOSURE        = 64,
  LOADKX            = 66,
  JUMPX             = 67,
  COVERAGE          = 69,
  CAPTURE           = 70,
  SUBRK             = 71,
  DIVRK             = 72,
  FASTCALL2         = 74,
  FASTCALL2K        = 75,
  FORGPREP          = 76,
  JUMPXEQKNIL       = 77,
  JUMPXEQKB         = 78,
  JUMPXEQKN         = 79,
  JUMPXEQKS         = 80,
  IDIV              = 81,
  IDIVK             = 82,
}

-- Helper to get constant text (string/number)
local function constToString(k)
	if type(k) == "string" then
		return ("%q"):format(k)
	elseif type(k) == "number" then
		return tostring(k)
	elseif k == nil then
		return "nil"
	elseif type(k) == "boolean" then
		return k and "true" or "false"
	else
		return "<const>"
	end
end

local old_tostring = tostring

local tostring = function(x)
	if type(x) == "string" then
		return "'"..x.."'"
	else
		return old_tostring(x)
	end
end


-- Helper for var naming fallback
local function varName(proto, reg, pc)
	if proto.locvars then
		for _, v in ipairs(proto.locvars) do
			if v.startpc <= pc and v.endpc >= pc and v.reg == reg then
				return v.varname
			end
		end
	end
	return "upvalue_" .. tostring(reg)
end

-- Core decompile function for a proto (function or main chunk)
local function decompileBlock(proto, indent, startpc, endpc, mainproto)
	indent = indent or ""
	startpc = startpc or 1
	endpc = endpc or #proto.code

	local env = getfenv(2)
	local old_env_script = env.script
	env.script = nil
	
	local stack = {}

	local output = {}
	local pc = startpc
	local globals = 0
	
	
	local isinsideforloop = 0
	
	-- these are extreme important btw
	
	local firstparamforloop = nil
	local secondparamforloop = nil
	local thirdparamforloop = nil
	local forloopidk = {}
	local calltype = nil
	local callargs = nil -- i aint doing this yet
	
	
	
	
	
	while pc <= endpc do
		local instr = proto.code[pc]
		local op = instr.opcode
		local inst = instr
		
		if isinsideforloop > 0 then
			isinsideforloop = isinsideforloop - 1
		end

		if instr.opname == "auxvalue" then
			pc = pc + 1
			continue
		end

		if not op then
			table.insert(output, indent .. "--CELESTIALERROR: opcode nil at pc=" .. pc)
			pc = pc + 1

		elseif op == opList.LOADK then
			local k = instr.K
			table.insert(output, indent .. ("local %s = %s"):format(varName(proto, instr.A, pc), constToString(k)))
			pc = pc + 1
			stack[instr.A] = k

		elseif op == opList.LOADNIL then
			table.insert(output, indent .. ("local %s = nil"):format(varName(proto, instr.A, pc)))
			pc = pc + 1
			
			stack[instr.A] = nil

		elseif op == opList.LOADB then
			table.insert(output, indent .. ("local %s = %s"):format(varName(proto, instr.A, pc), (instr.B ~= 0 and "true" or "false")))
			pc = pc + 1
			stack[instr.A] = instr.B == 1

		elseif op == opList.LOADN then
			
			
			if proto.code[pc + 3].opcode == opList.FORNPREP then
				stack[instr.A] = instr.D
				forloopidk[instr.A] = "i"
				
				-- fix names variables since after a for loop they get interpreted with more than the original
				
				for i,v in proto.code do
					if v.A then
						v.A -= inst.A
					end
				end
				
				pc = pc + 1
				firstparamforloop = instr.D
				continue
			elseif proto.code[pc + 2].opcode == opList.FORNPREP then
				stack[instr.A] = instr.D
				pc = pc + 1
				secondparamforloop = instr.D
				continue
			elseif proto.code[pc + 1].opcode == opList.FORNPREP then
				stack[instr.A] = instr.D
				pc = pc + 1
				thirdparamforloop = instr.D
				continue
			end
			
			
			table.insert(output, indent .. ("local %s = %d"):format(varName(proto, instr.A, pc), instr.D))
			pc = pc + 1
			stack[instr.A] = instr.D

		elseif op == opList.MOVE then
			local pass = false
			for i,v in output do
				if v:find(varName(proto, instr.B, pc)) then
					pass = true
				end
			end
			if pass == false then
				pc = pc + 1
				continue
			end
			
			
			table.insert(output, indent .. ("%s = %s"):format(varName(proto, instr.A, pc), varName(proto, instr.B, pc)))
			pc = pc + 1
			stack[instr.A] = stack[instr.B]

		elseif op == opList.LOADKX then
			local kv = inst.K
			
			table.insert(output, indent..varName(proto, instr.A, pc).." = "..tostring(kv))
			
			stack[inst.A] = kv
			pc = pc + 1

		elseif op == opList.FORNPREP then
			isinsideforloop = isinsideforloop + inst.D
			
			if thirdparamforloop == 1 then
				if not firstparamforloop or not secondparamforloop then
					table.insert("--FOR LOOP STRUCTURE DETECTED BUT CAUSED ERROR WILL BE FIXED ON THE NEXT UPDATE")
				end
				pcall(function(...) -- error handling i guess
					
					table.insert(output, indent..("for i = %s, %s do"):format(firstparamforloop, secondparamforloop))

				end)
				
			else
				if not firstparamforloop or not secondparamforloop then
					table.insert("--FOR LOOP STRUCTURE DETECTED BUT CAUSED ERROR WILL BE FIXED ON THE NEXT UPDATE")
				end
				pcall(function(...) 
					table.insert(output, indent..("for i = %s, %s, %s do"):format(firstparamforloop, secondparamforloop, thirdparamforloop))
				end)
			end
			
			firstparamforloop = nil
			secondparamforloop = nil
			thirdparamforloop = nil
			
			indent = indent.."\t"
			
			pc = pc + 1
		elseif op == opList.FORNLOOP then
			table.insert(output, string.sub(indent, 3).."end")
			pc = pc + 1

		elseif op == opList.GETGLOBAL then
			local name = instr.K or "global_" .. globals
			globals += 1
			table.insert(output, indent .. ("local %s = %s"):format(varName(proto, instr.A, pc), name))
			pc = pc + 1
			stack[instr.A] = env[name]

		elseif op == opList.SETGLOBAL then
			local name = instr.K or "global_" .. globals
			globals += 1
			table.insert(output, indent .. (name .. " = " .. varName(proto, instr.A, pc)))
			pc = pc + 1
			env[name] = stack[instr.A]
		elseif op == opList.GETUPVAL then
			table.insert(output, indent .. ("local %s = upval%d"):format(varName(proto, instr.A, pc), instr.B))
			pc = pc + 1

		elseif op == opList.SETUPVAL then
			table.insert(output, indent .. ("upval%d = %s"):format(instr.B, varName(proto, instr.A, pc)))
			pc = pc + 1

		elseif op >= opList.ADD and op <= opList.POW then
			local ops = {
				[opList.ADD] = "+", [opList.SUB] = "-", [opList.MUL] = "*",
				[opList.DIV] = "/", [opList.MOD] = "%", [opList.POW] = "^"
			}
			table.insert(output, indent .. ("%s = %s %s %s"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				ops[op],
				varName(proto, instr.C, pc)
				))
			pc = pc + 1
			
			if op == opList.ADD then
				stack[instr.A] = stack[instr.B] + stack[instr.C]
			elseif op == opList.SUB then
				stack[inst.A] = stack[inst.B] - stack[inst.C]
			elseif op == opList.MUL then
				stack[inst.A] = stack[inst.B] * stack[inst.C]
			elseif op == opList.DIV then
				stack[inst.A] = stack[inst.B] / stack[inst.C]
			elseif op == opList.MOD then
				stack[inst.A] = stack[inst.B] % stack[inst.C]
			elseif op == opList.POW then
				stack[inst.A] = stack[inst.B] ^ stack[inst.C]
			end
		
		elseif op >= opList.ADDK and op <= opList.POWK then
			local ops = {
				[opList.ADDK] = "+", [opList.SUBK] = "-", [opList.MULK] = "*",
				[opList.DIVK] = "/", [opList.MODK] = "%", [opList.POWK] = "^"
			}
			table.insert(output, indent .. ("%s = %s %s %s"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				ops[op],
				tostring(instr.K)
				))
		
			if op == opList.ADDK then
				stack[inst.A] = stack[inst.B] + inst.K
			elseif op == opList.SUBK then
				stack[inst.A] = stack[inst.B] - inst.K
			elseif op == opList.MULK then
				stack[inst.A] = stack[inst.B] * inst.K
			elseif op == opList.DIVK then
				stack[inst.A] = stack[inst.B] / inst.K
			elseif op == opList.MODK then
				stack[inst.A] = stack[inst.B] % inst.K
			elseif op == opList.POWK then
				stack[inst.A] = stack[inst.B] ^ inst.K
			end
			pc = pc + 1
		elseif op == opList.AND then
			local value = stack[inst.B]
			
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..if value then varName(proto, inst.C, pc) or "false" else tostring(value))
			
			stack[instr.A] = if value then stack[inst.C] or false else value
			pc = pc + 1
		elseif op == opList.OR then
			local value = stack[inst.B]
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..if value then tostring(value) else varName(proto, inst.C, pc) or "false")
			
			stack[inst.A] = if value then value else stack[inst.C] or false
			pc = pc + 1
			
		elseif op == opList.ANDK then
			local value = stack[inst.B]
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..if value then tostring(instr.K) or "false" else tostring(value))
			
			stack[inst.A] = if value then inst.K or false else value
			pc = pc + 1
		elseif op == opList.ORK then
			local value = stack[inst.B]
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..if value then tostring(value) else tostring(inst.K) or "false")
			
			stack[inst.A] = if value then value else inst.K or false
			pc = pc + 1
			
		elseif op == opList.CONCAT then
			local s = ""
			for i = inst.B, inst.C do
				s ..= stack[i] -- idk how tf this line works just let it work tbh
			end
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = '"..s.."'")
			
			stack[inst.A] = s
			pc = pc + 1
			
		elseif op == opList.NOT then
			table.insert(output, indent..varName(proto, inst.A, pc).." = not "..varName(proto, inst.B, pc))
			stack[inst.A] = not stack[inst.B]
			pc = pc + 1
		elseif op == opList.MINUS then
			table.insert(output, indent..varName(proto, inst.A, pc).." = -"..varName(proto, inst.B, pc))
			stack[inst.A] = -stack[inst.B]
			pc = pc + 1
		elseif op == opList.LENGTH then
			table.insert(output, indent..varName(proto, inst.A, pc).." = #"..varName(proto, inst.B, pc))
			stack[inst.A] = #stack[inst.B]
			pc = pc + 1
			
		elseif op == opList.CALL then
			local args = {}
			for i = 0, instr.B - 2 do
				table.insert(args, if forloopidk[i+2] then forloopidk[i+2] else varName(proto, i+1, pc))
			end
			
			
			
			if instr.B - 1 == 0 then
				args = {}
			end
			
			
			
			local callStr = if calltype then calltype .. "(" .. table.concat(args, ", ") .. ")" else varName(proto, instr.A, pc) .. "(" .. table.concat(args, ", ") .. ")"
			if instr.B and instr.B - 1 > 1 then
				local rets = {}
				for i = instr.A, instr.A + instr.B - 2 do
					table.insert(rets, varName(proto, i, pc))
				end
				callStr = table.concat(rets, ", ") .. " = " .. callStr
			end
			table.insert(output, indent .. callStr)
			pc = pc + 1
		elseif op == opList.NAMECALL then
			-- Format: varA = varA:method(varA+1, varA+2, ...)
			-- old method
			--local methodName = instr.K or "unknownMethod"
			--local args = {"var" .. tostring(instr.A)}
			-- Arguments start from A+1 if C specifies arg count
			--local argCount = instr.B - 2 or 0
			--if argCount == -1 then
			--	local callStr = string.format("var%d = var%d:%s(%s)", instr.A, instr.A, tostring(methodName), "")
			--	table.insert(output, indent .. callStr)
			--	pc = pc + 1
			--	continue
			--end
			--for i = 1, argCount do
			--	table.insert(args, "var" .. tostring(instr.A + i))
			--end
			--local callStr = string.format("var%d = var%d:%s(%s)", instr.A, instr.A, tostring(methodName), table.concat(args, ", "))
			--table.insert(output, indent .. callStr)
			
			local A = instr.A
			local B = instr.B
			local kv = instr.K
			local sb = varName(proto, B, pc)
			
			table.insert(output, indent..varName(proto, A+1, pc).." = "..sb)
			
			
			pc = pc + 1
			
			
			table.insert(output, indent..varName(proto, A, pc).." = "..sb.."["..kv.."]")
			
		-- Jump opcodes
		elseif op == opList.JUMP then
			pc += instr.D
			pc = pc + 1
		
		elseif op == opList.JUMPBACK then
			pc += instr.D
			pc = pc + 1
		
		elseif op == opList.JUMPIF then
			if stack[instr.A] then
				pc += instr.D
			end
			pc = pc + 1
			
		elseif op == opList.JUMPIFNOT then
			if not stack[instr.A] then
				pc += instr.D
			end
			pc = pc + 1
		elseif op == opList.JUMPIFEQ then
			if stack[instr.A] == stack[instr.aux] then
				pc += instr.D
			else
				pc = pc + 1
			end
			pc = pc + 1
		elseif op == opList.JUMPIFLE then
			if stack[inst.A] <= stack[inst.aux] then
				pc += inst.D
			else
				pc += 1
			end
			pc = pc + 1
		elseif op == opList.JUMPIFLT then
			if stack[inst.A] < stack[inst.aux] then
				pc += inst.D
			else
				pc += 1
			end
			pc = pc + 1
		elseif op == opList.JUMPIFNOTEQ then
			if stack[inst.A] == stack[inst.aux] then
				pc += 1
			else
				pc += inst.D
			end
			pc += 1
		elseif op == opList.JUMPIFNOTLE then
			if stack[inst.A] <= stack[inst.aux] then
				pc += 1
			else
				pc += inst.D
			end
			pc += 1
		elseif op == opList.JUMPIFNOTLT then
			if stack[inst.A] < stack[inst.aux] then
				pc += 1
			else
				pc += inst.D
			end
			pc += 1
			
		elseif op == opList.JUMPXEQKN then
			local kv = inst.K
			local kn = inst.KN
			local ra = stack[inst.A]

			if (ra == kv) ~= kn then
				pc += inst.D
			else
				pc += 1
			end
			pc += 1
		elseif op == opList.JUMPXEQKNIL then
			local kn = instr.KN
			
			if (stack[inst.A] == nil) ~= kn then
				pc += inst.D
			else
				pc += 1
			end
			pc += 1
		elseif op == opList.JUMPXEQKB then
			local kv = inst.K
			local kn = inst.KN
			local ra = stack[inst.A]

			if (type(ra) == "boolean" and (ra == kv)) ~= kn then
				pc += inst.D
			else
				pc += 1
			end
			
			pc += 1
			
		elseif op == opList.JUMPXEQKS then
			local kv = inst.K
			local kn = inst.KN
			local ra = stack[inst.A]

			if (ra == kv) ~= kn then
				pc += inst.D
			else
				pc += 1
			end
			
			pc += 1
			
		elseif op == opList.RETURN then
			if not instr.B or instr.B <= 1 then
				-- no return value
			else
				local rets = {}
				for i = 0, instr.B - 2 do
					table.insert(rets, varName(proto, instr.A + i, pc))
				end
				if #rets > 0 then
					table.insert(output, indent .. "return " .. table.concat(rets, ", "))
				end
			end
			pc = pc + 1

		elseif op == opList.GETIMPORT then
			
			
			
			local count = instr.KC
			local k0 = instr.K0
			local import = env[k0]
			
			if proto.code[pc + 2].opcode == opList.CALL or proto.code[pc + 3].opcode == opList.CALL then
				calltype = k0
				pc = pc + 1
				continue
			end
			
			

			if count == 1 then
				table.insert(output, indent..varName(proto, instr.A, pc).." = getfenv()['"..k0.."']")
				stack[instr.A] = import
			elseif count == 2 then
				table.insert(output, indent..varName(proto, instr.A, pc).." = getfenv()['"..k0.."']['"..instr.K1.."']")
				stack[instr.A] = import[instr.K1]
			elseif count == 3 then
				table.insert(output, indent..varName(proto, instr.A, pc).." = getfenv()['"..k0.."']['"..instr.K1.."']['"..instr.K2.."']")
				stack[instr.A] = import[instr.K1][instr.K2]
			end
			pc = pc + 1

		elseif op == opList.PREPVARARGS then -- bro what was devs thinking when making this opcode
			-- This opcode prepares variable arguments, typically no direct source code line
			-- We can skip it or mark it minimally
			-- It should never appear repeatedly, so we just skip
			pc = pc + 1
			
		elseif op == opList.GETVARARGS then
			pc = pc + 1
			
		elseif op == opList.FASTCALL3 then
			pc = pc + 1
			pc = pc + 1

		elseif op == opList.NEWTABLE then
			table.insert(output, indent .. ("local %s = {%s}"):format(varName(proto, instr.A, pc), table.concat(table.create(instr.aux), ", ")))
			stack[instr.A] = table.create(inst.aux)
			pc = pc + 1

		elseif op == opList.DUPTABLE then -- this does the same shit bro only a little diff so this is not right if you want to be more precise you will fix it
			table.insert(output, indent .. ("local %s = {%s}"):format(varName(proto, instr.A, pc), table.concat(table.create(instr.aux), ", ")))
			stack[instr.A] = table.create(inst.aux)
			pc = pc + 1

		elseif op == opList.SETTABLE then
			table.insert(output, indent .. ("%s[%s] = %s"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				varName(proto, instr.C, pc)
				))
			pc = pc + 1
			stack[instr.B][stack[instr.C]] = stack[instr.A]
			
			
		
		
			
		elseif op == opList.COVERAGE then
			
			inst.E += 1
			pc = pc + 1 
			
		elseif op == opList.SUBRK then
			
			stack[inst.A] = inst.K - stack[inst.C]
			
			local name = instr.K
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..name.." - "..varName(proto, inst.C, pc))
			
			pc = pc + 1 
			
		elseif op == opList.DIVK then
			stack[inst.A] = inst.K / stack[inst.C]
			
			local name = instr.K

			table.insert(output, indent..varName(proto, inst.A, pc).." = "..name.." - "..varName(proto, inst.C, pc))

			pc = pc + 1 
			
			
		elseif op == opList.FASTCALL2 then
			pc = pc + 1 
			pc = pc + 1 
			
		elseif op == opList.FASTCALL2K then
			pc = pc + 1
			pc = pc + 1 
		
			
		elseif op == opList.GETTABLE then
			table.insert(output, indent .. ("local %s = %s[%s]"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				varName(proto, instr.C, pc)
				))
			pc = pc + 1
			stack[instr.A] = stack[instr.B][stack[instr.C]]
		elseif op == opList.GETTABLEKS then
			local key = instr.K or ("key" .. tostring(instr.C))
			table.insert(output, indent .. ("local %s = %s.%s"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				key
				))
			pc = pc + 1
			pcall(function(...) -- error handling i guess
				stack[instr.A] = stack[instr.B][instr.K]
			end)
			

		elseif op == opList.SETTABLEKS then
			local key = instr.K or ("key" .. tostring(instr.C))
			table.insert(output, indent .. ("%s.%s = %s"):format(
				varName(proto, instr.A, pc),
				key,
				varName(proto, instr.B, pc)
				))
			pc = pc + 1
			stack[instr.B][instr.K] = stack[instr.A]
			
		elseif op == opList.GETTABLEN then
			local key = instr.K or ("key" .. tostring(instr.C))
			table.insert(output, indent..("local %s = %s.%s"):format(
				varName(proto, instr.A, pc),
				varName(proto, instr.B, pc),
				key
				))
			stack[instr.A] = stack[instr.B][instr.C + 1]
			pc = pc + 1
		elseif op == opList.SETTABLEN then
			local key = instr.K or ("key" .. tostring(instr.C))
			table.insert(output, indent .. ("%s.%s = %s"):format(
				varName(proto, instr.A, pc),
				key,
				varName(proto, instr.B, pc)
				))
			pc = pc + 1
			stack[instr.B][instr.C + 1] = stack[instr.A]
		
		elseif op == opList.NEWCLOSURE or op == opList.DUPCLOSURE then
			if op == opList.DUPCLOSURE then
				local subProto = mainproto.protoList[proto.protos[instr.K + 1]]
				if subProto then
					local args = {}
					for i = 0, subProto.numparams - 1 do
						table.insert(args, varName(subProto, i, 0))
					end
					local name = subProto.debugname or varName(proto, instr.A, pc)
					table.insert(output, indent .. ("local %s = function(%s)"):format(
						name,
						table.concat(args, ", ")
						))
					local body = decompileBlock(subProto, indent .. "    ", 1, #subProto.code)
					for _, line in ipairs(body) do
						table.insert(output, line)
					end
					table.insert(output, indent .. "end")
				else
					table.insert(output, indent .. ("-- closure missing proto at pc=%d"):format(pc))
				end
				if proto.code[pc + 1] == opList.CAPTURE and proto.code[pc + 2] == opList.CAPTURE then
					pc = pc + 4
				else
					pc = pc + 2
				end
			else
				local subProto = mainproto.protoList[proto.protos[instr.D + 1]]
				if subProto then
					local args = {}
					for i = 0, subProto.numparams - 1 do
						table.insert(args, varName(subProto, i, 0))
					end
					local name = subProto.debugname or varName(proto, instr.A, pc)
					table.insert(output, indent .. ("local %s = function(%s)"):format(
						name,
						table.concat(args, ", ")
						))
					local body = decompileBlock(subProto, indent .. "    ", 1, #subProto.code)
					for _, line in ipairs(body) do
						table.insert(output, line)
					end
					table.insert(output, indent .. "end")
				else
					table.insert(output, indent .. ("-- closure missing proto at pc=%d"):format(pc))
				end
				if proto.code[pc + 1] == opList.CAPTURE and proto.code[pc + 2] == opList.CAPTURE then
					pc = pc + 4
				else
					pc = pc + 2
				end
			end


		elseif op == opList.CAPTURE then
			--handled by NEWCLOSURE and DUPCLOSURE
			pc += 1
		elseif op == 65 then
			-- this opcods do nothing idk why i even added them
			pc += 1
		elseif op == opList.IDIV then
			stack[instr.A] = stack[inst.B] // stack[inst.C]
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..varName(proto, inst.B, pc).." // "..varName(proto, inst.C, pc))
			
			
			pc += 1
		
		elseif op == opList.IDIVK then
			stack[inst.A] = stack[inst.B] // inst.K
			local name = inst.K
			
			table.insert(output, indent..varName(proto, inst.A, pc).." = "..varName(proto, inst.B, pc).." // "..name)
			
		else
			print("PC: "..pc.."\nOPCODE:"..op.."\n\n")
			pc += 1
		end
	end
	
	env.script = old_env_script -- re-add the script that is calling into his enviroment again

	


	return output
end

-- Entry decompiler function (example)
local function decompile(proto, mainproto)
	local success, lines = pcall(decompileBlock, proto, nil, nil, nil, mainproto)
	if not success then
		return nil
	end
	return table.concat(lines, "\n")
end




local function parseprotolist(protolist)
	local out = {}
	for i,v in protolist do
		if i == #protolist then
			break
		end
		out[i] = decompile(v)
	end
	out[#protolist] = decompile(protolist[#protolist])
	return table.concat(out)
end




local function decompileLuau(proto)
	local ast
	if proto.mainProto and proto.protoList then
		ast = decompile(proto.mainProto, proto)
		--ast = parseBlock(proto.mainProto, proto.mainProto.code, 1, #proto.mainProto.code, proto.protoList)
	else
		return cerror("MAIN","Invalid bytecode structure. Expected mainProto.")
	end
	
	if ast == nil then
		return cerror("MAIN", "Unknown error decompiling sorry please report this")
	end
	
	local lines = ast
	return "--DECOMPILED USING CELESTIAL V2.2 AS A FAST LUAU DECOMPILER\n--DECOMPILER MADE BY GLITCHED VOID\n\n"..lines
end





tostring = old_tostring

-- // Environment changes in the VM are not supposed to alter the behaviour of the VM so we localise globals beforehand
local type = type
local pcall = pcall
local error = error
local tonumber = tonumber
local assert = assert
local setmetatable = setmetatable

local string_format = string.format

local table_move = table.move
local table_pack = table.pack
local table_unpack = table.unpack
local table_create = table.create
local table_insert = table.insert
local table_remove = table.remove

local coroutine_create = coroutine.create
local coroutine_yield = coroutine.yield
local coroutine_resume = coroutine.resume
local coroutine_close = coroutine.close

local buffer_fromstring = buffer.fromstring
local buffer_len = buffer.len
local buffer_readu8 = buffer.readu8
local buffer_readu32 = buffer.readu32
local buffer_readstring = buffer.readstring
local buffer_readf32 = buffer.readf32
local buffer_readf64 = buffer.readf64

local bit32_bor = bit32.bor
local bit32_band = bit32.band
local bit32_btest = bit32.btest
local bit32_rshift = bit32.rshift
local bit32_lshift = bit32.lshift
local bit32_extract = bit32.extract

-- // opList contains information about the instruction, each instruction is defined in this format:
-- // {OP_NAME, OP_MODE, K_MODE, HAS_AUX}
-- // OP_MODE specifies what type of registers the instruction uses if any
--		0 = NONE
--		1 = A
--		2 = AB
--		3 = ABC
--		4 = AD
--		5 = AE
-- // K_MODE specifies if the instruction has a register that holds a constant table index, which will be directly converted to the constant in the 2nd pass
--		0 = NONE
--		1 = AUX
--		2 = C
--		3 = D
--		4 = AUX import
--		5 = AUX boolean low 1 bit
--		6 = AUX number low 24 bits
-- // HAS_AUX boolean specifies whether the instruction is followed up with an AUX word, which may be used to execute the instruction.

local opList = {
	{ "NOP", 0, 0, false },
	{ "BREAK", 0, 0, false },
	{ "LOADNIL", 1, 0, false },
	{ "LOADB", 3, 0, false },
	{ "LOADN", 4, 0, false },
	{ "LOADK", 4, 3, false },
	{ "MOVE", 2, 0, false },
	{ "GETGLOBAL", 1, 1, true },
	{ "SETGLOBAL", 1, 1, true },
	{ "GETUPVAL", 2, 0, false },
	{ "SETUPVAL", 2, 0, false },
	{ "CLOSEUPVALS", 1, 0, false },
	{ "GETIMPORT", 4, 4, true },
	{ "GETTABLE", 3, 0, false },
	{ "SETTABLE", 3, 0, false },
	{ "GETTABLEKS", 3, 1, true },
	{ "SETTABLEKS", 3, 1, true },
	{ "GETTABLEN", 3, 0, false },
	{ "SETTABLEN", 3, 0, false },
	{ "NEWCLOSURE", 4, 0, false },
	{ "NAMECALL", 3, 1, true },
	{ "CALL", 3, 0, false },
	{ "RETURN", 2, 0, false },
	{ "JUMP", 4, 0, false },
	{ "JUMPBACK", 4, 0, false },
	{ "JUMPIF", 4, 0, false },
	{ "JUMPIFNOT", 4, 0, false },
	{ "JUMPIFEQ", 4, 0, true },
	{ "JUMPIFLE", 4, 0, true },
	{ "JUMPIFLT", 4, 0, true },
	{ "JUMPIFNOTEQ", 4, 0, true },
	{ "JUMPIFNOTLE", 4, 0, true },
	{ "JUMPIFNOTLT", 4, 0, true },
	{ "ADD", 3, 0, false },
	{ "SUB", 3, 0, false },
	{ "MUL", 3, 0, false },
	{ "DIV", 3, 0, false },
	{ "MOD", 3, 0, false },
	{ "POW", 3, 0, false },
	{ "ADDK", 3, 2, false },
	{ "SUBK", 3, 2, false },
	{ "MULK", 3, 2, false },
	{ "DIVK", 3, 2, false },
	{ "MODK", 3, 2, false },
	{ "POWK", 3, 2, false },
	{ "AND", 3, 0, false },
	{ "OR", 3, 0, false },
	{ "ANDK", 3, 2, false },
	{ "ORK", 3, 2, false },
	{ "CONCAT", 3, 0, false },
	{ "NOT", 2, 0, false },
	{ "MINUS", 2, 0, false },
	{ "LENGTH", 2, 0, false },
	{ "NEWTABLE", 2, 0, true },
	{ "DUPTABLE", 4, 3, false },
	{ "SETLIST", 3, 0, true },
	{ "FORNPREP", 4, 0, false },
	{ "FORNLOOP", 4, 0, false },
	{ "FORGLOOP", 4, 8, true },
	{ "FORGPREP_INEXT", 4, 0, false },
	{ "FASTCALL3", 3, 1, true },
	{ "FORGPREP_NEXT", 4, 0, false },
	{ "DEP_FORGLOOP_NEXT", 0, 0, false },
	{ "GETVARARGS", 2, 0, false },
	{ "DUPCLOSURE", 4, 3, false },
	{ "PREPVARARGS", 1, 0, false },
	{ "LOADKX", 1, 1, true },
	{ "JUMPX", 5, 0, false },
	{ "FASTCALL", 3, 0, false },
	{ "COVERAGE", 5, 0, false },
	{ "CAPTURE", 2, 0, false },
	{ "SUBRK", 3, 7, false },
	{ "DIVRK", 3, 7, false },
	{ "FASTCALL1", 3, 0, false },
	{ "FASTCALL2", 3, 0, true },
	{ "FASTCALL2K", 3, 1, true },
	{ "FORGPREP", 4, 0, false },
	{ "JUMPXEQKNIL", 4, 5, true },
	{ "JUMPXEQKB", 4, 5, true },
	{ "JUMPXEQKN", 4, 6, true },
	{ "JUMPXEQKS", 4, 6, true },
	{ "IDIV", 3, 0, false },
	{ "IDIVK", 3, 2, false }
}

local function luau_newsettings()
	return {
		vectorCtor = function() error("vectorCtor was not provided") end,
		vectorSize = 4,
		useNativeNamecall = false,
		namecallHandler = function() error("Native __namecall handler was not provided") end,
		extensions = {},
		callHooks = {},
		errorHandling = false,
		generalizedIteration = true,
		allowProxyErrors = false,
		useImportConstants = false,
		staticEnvironment = {},
		decodeOp = function(op) return op end
	}
end

local function resolveImportConstant(static, count, k0, k1, k2)
	local res = static[k0]
	if count < 2 or res == nil then
		return res
	end
	res = res[k1]
	if count < 3 or res == nil then
		return res
	end
	res = res[k2]
	return res
end

local function luau_deserialize(bytecode, luau_settings)
	if luau_settings == nil then
		luau_settings = luau_newsettings()
	end

	local stream = if type(bytecode) == "string" then buffer_fromstring(bytecode) else bytecode
	local cursor = 0

	local function readByte()
		local byte = buffer_readu8(stream, cursor)
		cursor = cursor + 1
		return byte
	end

	local function readWord()
		local word = buffer_readu32(stream, cursor)
		cursor = cursor + 4
		return word
	end

	local function readFloat()
		local float = buffer_readf32(stream, cursor)
		cursor = cursor + 4
		return float
	end

	local function readDouble()
		local double = buffer_readf64(stream, cursor)
		cursor = cursor + 8
		return double
	end

	local function readVarInt()
		local result = 0

		for i = 0, 4 do
			local value = readByte()
			result = bit32_bor(result, bit32_lshift(bit32_band(value, 0x7F), i * 7))
			if not bit32_btest(value, 0x80) then
				break
			end
		end

		return result
	end

	local function readString()
		local size = readVarInt()

		if size == 0 then
			return ""
		else
			local str = buffer_readstring(stream, cursor, size)
			cursor = cursor + size

			return str
		end
	end

	local luauVersion = readByte()
	local typesVersion = 0
	if luauVersion == 0 then
		error("the provided bytecode is an error message",0)
	elseif luauVersion < 3 or luauVersion > 6 then
		error("the version of the provided bytecode is unsupported",0)
	elseif luauVersion >= 4 then
		typesVersion = readByte()
	end

	local stringCount = readVarInt()
	local stringList = table_create(stringCount)

	for i = 1, stringCount do
		stringList[i] = readString()
	end

	local function readInstruction(codeList)
		local value = readWord()
		local opcode = bit32_band(value, 0xFF)

		local opname, opmode, kmode, usesAux = unpack(opList[opcode + 1])

		local inst = {
			opcode = opcode;
			opname = opname;
			opmode = opmode;
			kmode = kmode;
			usesAux = usesAux;
		}

		table_insert(codeList, inst)

		if opmode == 1 then --[[ A ]]
			inst.A = bit32_band(bit32_rshift(value, 8), 0xFF)
		elseif opmode == 2 then --[[ AB ]]
			inst.A = bit32_band(bit32_rshift(value, 8), 0xFF)
			inst.B = bit32_band(bit32_rshift(value, 16), 0xFF)
		elseif opmode == 3 then --[[ ABC ]]
			inst.A = bit32_band(bit32_rshift(value, 8), 0xFF)
			inst.B = bit32_band(bit32_rshift(value, 16), 0xFF)
			inst.C = bit32_band(bit32_rshift(value, 24), 0xFF)
		elseif opmode == 4 then --[[ AD ]]
			inst.A = bit32_band(bit32_rshift(value, 8), 0xFF)
			local temp = bit32_band(bit32_rshift(value, 16), 0xFFFF)
			inst.D = if temp < 0x8000 then temp else temp - 0x10000
		elseif opmode == 5 then --[[ AE ]]
			local temp = bit32_band(bit32_rshift(value, 8), 0xFFFFFF)
			inst.E = if temp < 0x800000 then temp else temp - 0x1000000
		end

		if usesAux then 
			local aux = readWord()
			inst.aux = aux

			table_insert(codeList, {value = aux, opname = "auxvalue" })
		end

		return usesAux
	end

	local function checkkmode(inst, k)
		local kmode = inst.kmode

		if kmode == 1 then --// AUX
			inst.K = k[inst.aux +  1]
		elseif kmode == 2 then --// C
			inst.K = k[inst.C + 1]
		elseif kmode == 3 then--// D
			inst.K = k[inst.D + 1]
		elseif kmode == 4 then --// AUX import
			local extend = inst.aux
			local count = bit32_rshift(extend, 30)
			local id0 = bit32_band(bit32_rshift(extend, 20), 0x3FF)

			inst.K0 = k[id0 + 1]
			inst.KC = count
			if count == 2 then
				local id1 = bit32_band(bit32_rshift(extend, 10), 0x3FF)

				inst.K1 = k[id1 + 1]
			elseif count == 3 then
				local id1 = bit32_band(bit32_rshift(extend, 10), 0x3FF)
				local id2 = bit32_band(bit32_rshift(extend, 0), 0x3FF)

				inst.K1 = k[id1 + 1]
				inst.K2 = k[id2 + 1]
			end
			if luau_settings.useImportConstants then
				inst.K = resolveImportConstant(
					luau_settings.staticEnvironment,
					count, inst.K0, inst.K1, inst.K2
				)
			end
		elseif kmode == 5 then --// AUX boolean low 1 bit
			inst.K = bit32_extract(inst.aux, 0, 1) == 1
			inst.KN = bit32_extract(inst.aux, 31, 1) == 1
		elseif kmode == 6 then --// AUX number low 24 bits
			inst.K = k[bit32_extract(inst.aux, 0, 24) + 1]
			inst.KN = bit32_extract(inst.aux, 31, 1) == 1
		elseif kmode == 7 then --// B
			inst.K = k[inst.B + 1]
		elseif kmode == 8 then --// AUX number low 16 bits
			inst.K = bit32_band(inst.aux, 0xf)
		end
	end

	local function readProto(bytecodeid)
		local maxstacksize = readByte()
		local numparams = readByte()
		local nups = readByte()
		local isvararg = readByte() ~= 0

		if luauVersion >= 4 then
			readByte() --// flags 
			local typesize = readVarInt();
			cursor = cursor + typesize;
		end

		local sizecode = readVarInt()
		local codelist = table_create(sizecode)

		local skipnext = false 
		for i = 1, sizecode do
			if skipnext then 
				skipnext = false
				continue 
			end

			skipnext = readInstruction(codelist)
		end

		local debugcodelist = table_create(sizecode) 
		for i = 1, sizecode do 
			debugcodelist[i] = codelist[i].opcode
		end 

		local sizek = readVarInt()
		local klist = table_create(sizek)

		for i = 1, sizek do
			local kt = readByte()
			local k

			if kt == 1 then --// Bool
				k = readByte() ~= 0
			elseif kt == 2 then --// Number
				k = readDouble()
			elseif kt == 3 then --// String
				k = stringList[readVarInt()]
			elseif kt == 4 then --// Import
				k = readWord()
			elseif kt == 5 then --// Table
				local dataLength = readVarInt()
				k = table_create(dataLength)

				for i = 1, dataLength do
					k[i] = readVarInt()
				end
			elseif kt == 6 then --// Closure
				k = readVarInt()
			elseif kt == 7 then --// Vector
				local x,y,z,w = readFloat(), readFloat(), readFloat(), readFloat()

				if luau_settings.vectorSize == 4 then
					k = luau_settings.vectorCtor(x,y,z,w)
				else 
					k = luau_settings.vectorCtor(x,y,z)
				end
			end

			klist[i] = k
		end

		-- // 2nd pass to replace constant references in the instruction
		for i = 1, sizecode do
			checkkmode(codelist[i], klist)
		end

		local sizep = readVarInt()
		local protolist = table_create(sizep)

		for i = 1, sizep do
			protolist[i] = readVarInt() + 1
		end

		local linedefined = readVarInt()

		local debugnameindex = readVarInt()
		local debugname 

		if debugnameindex ~= 0 then
			debugname = stringList[debugnameindex]
		else 
			debugname = "(??)"
		end

		-- // lineinfo
		local lineinfoenabled = readByte() ~= 0
		local instructionlineinfo = nil 

		if lineinfoenabled then
			local linegaplog2 = readByte()

			local intervals = bit32_rshift((sizecode - 1), linegaplog2) + 1

			local lineinfo = table_create(sizecode)
			local abslineinfo = table_create(intervals)

			local lastoffset = 0
			for j = 1, sizecode do
				lastoffset += readByte()
				lineinfo[j] = lastoffset
			end

			local lastline = 0
			for j = 1, intervals do
				lastline += readWord()
				abslineinfo[j] = lastline % (2 ^ 32)
			end

			instructionlineinfo = table_create(sizecode)

			for i = 1, sizecode do 
				--// p->abslineinfo[pc >> p->linegaplog2] + p->lineinfo[pc];
				table_insert(instructionlineinfo, abslineinfo[bit32_rshift(i - 1, linegaplog2) + 1] + lineinfo[i])
			end
		end

		-- // debuginfo
		if readByte() ~= 0 then
			local sizel = readVarInt()
			for i = 1, sizel do
				readVarInt()
				readVarInt()
				readVarInt()
				readByte()
			end
			local sizeupvalues = readVarInt()
			for i = 1, sizeupvalues do
				readVarInt()
			end
		end

		return {
			maxstacksize = maxstacksize;
			numparams = numparams;
			nups = nups;
			isvararg = isvararg;
			linedefined = linedefined;
			debugname = debugname;

			sizecode = sizecode;
			code = codelist;
			debugcode = debugcodelist;

			sizek = sizek;
			k = klist;

			sizep = sizep;
			protos = protolist;

			lineinfoenabled = lineinfoenabled;
			instructionlineinfo = instructionlineinfo;

			bytecodeid = bytecodeid;
		}
	end

	-- userdataRemapping (not used in VM, left unused)
	if typesVersion == 3 then
		local index = readByte()

		while index ~= 0 do
			readVarInt()

			index = readByte()
		end
	end

	local protoCount = readVarInt()
	local protoList = table_create(protoCount)

	for i = 1, protoCount do
		protoList[i] = readProto(i - 1)
	end

	local mainProto = protoList[readVarInt() + 1]

	assert(cursor == buffer_len(stream), "deserializer cursor position mismatch")

	mainProto.debugname = "(main)"

	return {
		stringList = stringList;
		protoList = protoList;

		mainProto = mainProto;

		typesVersion = typesVersion;
	}
end


return setmetatable({
	decompile = function(bytecode)
		if type(bytecode) ~= "string" then
			return cerror("MAIN", "Bytecode given is a "..type(bytecode).." expected string.")
		end
		return decompileLuau(luau_deserialize(bytecode))
	end,
}, {
	__call = function(self, bytecode)
		return self.decompile(bytecode)
	end,
})
