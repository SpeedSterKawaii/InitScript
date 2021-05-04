--- Thunder Init Script by SpeedSterKawaii ---

------ GAME DETECTION ------

if game.PlaceId == 2377868063 then
  game.Players.LocalPlayer:Kick("You have a high change of getting banned.")
  else
  wait(0.30)
end

if game.PlaceId == 292439477 then
    info("Playing Phantom Forces")
    wait(1)
  else
  wait(0.30)
end

------ GAME TELEPORTER ------

game:GetService("Players").LocalPlayer.OnTeleport:Connect(function(state) 
    if state == Enum.TeleportState.InProgress then
        messagebox("Hello skid who is skidding thunder.", "Skid")
    end
end)

------ EXTRA FUNCTIONS ------

function hint(msg)
    local hint = Instance.new("Hint")
    hint.Parent = game.Workspace
    hint.Text = msg
end

------ CHECK FUNCTION ------

if getfenv then 
  wait(0.30)
  else
  warn("Error while doing getfenv")
end

------ FIONE PARSER ------

local bit = bit or bit32 or require('bit')
local unpack = table.unpack or unpack

local stm_lua_bytecode
local wrap_lua_func
local stm_lua_func

-- SETLIST config
local FIELDS_PER_FLUSH = 50

-- opcode types for getting values
local opcode_t = {
	[0] = 'ABC',
	'ABx',
	'ABC',
	'ABC',
	'ABC',
	'ABx',
	'ABC',
	'ABx',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'AsBx',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'ABC',
	'AsBx',
	'AsBx',
	'ABC',
	'ABC',
	'ABC',
	'ABx',
	'ABC',
}

local opcode_m = {
	[0] = {b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgK', c = 'OpArgN'},
	{b = 'OpArgU', c = 'OpArgU'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgU', c = 'OpArgN'},
	{b = 'OpArgK', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgN'},
	{b = 'OpArgU', c = 'OpArgN'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgU', c = 'OpArgU'},
	{b = 'OpArgR', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgR'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgK', c = 'OpArgK'},
	{b = 'OpArgR', c = 'OpArgU'},
	{b = 'OpArgR', c = 'OpArgU'},
	{b = 'OpArgU', c = 'OpArgU'},
	{b = 'OpArgU', c = 'OpArgU'},
	{b = 'OpArgU', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgR', c = 'OpArgN'},
	{b = 'OpArgN', c = 'OpArgU'},
	{b = 'OpArgU', c = 'OpArgU'},
	{b = 'OpArgN', c = 'OpArgN'},
	{b = 'OpArgU', c = 'OpArgN'},
	{b = 'OpArgU', c = 'OpArgN'},
}

-- int rd_int_basic(string src, int s, int e, int d)
-- @src - Source binary string
-- @s - Start index of a little endian integer
-- @e - End index of the integer
-- @d - Direction of the loop
local function rd_int_basic(src, s, e, d)
	local num = 0

	-- if bb[l] > 127 then -- signed negative
	-- 	num = num - 256 ^ l
	-- 	bb[l] = bb[l] - 128
	-- end

	for i = s, e, d do num = num + string.byte(src, i, i) * 256 ^ (i - s) end

	return num
end

-- float rd_flt_basic(byte f1..8)
-- @f1..4 - The 4 bytes composing a little endian float
local function rd_flt_basic(f1, f2, f3, f4)
	local sign = (-1) ^ bit.rshift(f4, 7)
	local exp = bit.rshift(f3, 7) + bit.lshift(bit.band(f4, 0x7F), 1)
	local frac = f1 + bit.lshift(f2, 8) + bit.lshift(bit.band(f3, 0x7F), 16)
	local normal = 1

	if exp == 0 then
		if frac == 0 then
			return sign * 0
		else
			normal = 0
			exp = 1
		end
	elseif exp == 0x7F then
		if frac == 0 then
			return sign * (1 / 0)
		else
			return sign * (0 / 0)
		end
	end

	return sign * 2 ^ (exp - 127) * (1 + normal / 2 ^ 23)
end

-- double rd_dbl_basic(byte f1..8)
-- @f1..8 - The 8 bytes composing a little endian double
local function rd_dbl_basic(f1, f2, f3, f4, f5, f6, f7, f8)
	local sign = (-1) ^ bit.rshift(f8, 7)
	local exp = bit.lshift(bit.band(f8, 0x7F), 4) + bit.rshift(f7, 4)
	local frac = bit.band(f7, 0x0F) * 2 ^ 48
	local normal = 1

	frac = frac + (f6 * 2 ^ 40) + (f5 * 2 ^ 32) + (f4 * 2 ^ 24) + (f3 * 2 ^ 16) + (f2 * 2 ^ 8) + f1 -- help

	if exp == 0 then
		if frac == 0 then
			return sign * 0
		else
			normal = 0
			exp = 1
		end
	elseif exp == 0x7FF then
		if frac == 0 then
			return sign * (1 / 0)
		else
			return sign * (0 / 0)
		end
	end

	return sign * 2 ^ (exp - 1023) * (normal + frac / 2 ^ 52)
end

-- int rd_int_le(string src, int s, int e)
-- @src - Source binary string
-- @s - Start index of a little endian integer
-- @e - End index of the integer
local function rd_int_le(src, s, e) return rd_int_basic(src, s, e - 1, 1) end

-- int rd_int_be(string src, int s, int e)
-- @src - Source binary string
-- @s - Start index of a big endian integer
-- @e - End index of the integer
local function rd_int_be(src, s, e) return rd_int_basic(src, e - 1, s, -1) end

-- float rd_flt_le(string src, int s)
-- @src - Source binary string
-- @s - Start index of little endian float
local function rd_flt_le(src, s) return rd_flt_basic(string.byte(src, s, s + 3)) end

-- float rd_flt_be(string src, int s)
-- @src - Source binary string
-- @s - Start index of big endian float
local function rd_flt_be(src, s)
	local f1, f2, f3, f4 = string.byte(src, s, s + 3)
	return rd_flt_basic(f4, f3, f2, f1)
end

-- double rd_dbl_le(string src, int s)
-- @src - Source binary string
-- @s - Start index of little endian double
local function rd_dbl_le(src, s) return rd_dbl_basic(string.byte(src, s, s + 7)) end

-- double rd_dbl_be(string src, int s)
-- @src - Source binary string
-- @s - Start index of big endian double
local function rd_dbl_be(src, s)
	local f1, f2, f3, f4, f5, f6, f7, f8 = string.byte(src, s, s + 7) -- same
	return rd_dbl_basic(f8, f7, f6, f5, f4, f3, f2, f1)
end

-- to avoid nested ifs in deserializing
local float_types = {
	[4] = {little = rd_flt_le, big = rd_flt_be},
	[8] = {little = rd_dbl_le, big = rd_dbl_be},
}

-- byte stm_byte(Stream S)
-- @S - Stream object to read from
local function stm_byte(S)
	local idx = S.index
	local bt = string.byte(S.source, idx, idx)

	S.index = idx + 1
	return bt
end

-- string stm_string(Stream S, int len)
-- @S - Stream object to read from
-- @len - Length of string being read
local function stm_string(S, len)
	local pos = S.index + len
	local str = string.sub(S.source, S.index, pos - 1)

	S.index = pos
	return str
end

-- string stm_lstring(Stream S)
-- @S - Stream object to read from
local function stm_lstring(S)
	local len = S:s_szt()
	local str

	if len ~= 0 then str = string.sub(stm_string(S, len), 1, -2) end

	return str
end

-- fn cst_int_rdr(string src, int len, fn func)
-- @len - Length of type for reader
-- @func - Reader callback
local function cst_int_rdr(len, func)
	return function(S)
		local pos = S.index + len
		local int = func(S.source, S.index, pos)
		S.index = pos

		return int
	end
end

-- fn cst_flt_rdr(string src, int len, fn func)
-- @len - Length of type for reader
-- @func - Reader callback
local function cst_flt_rdr(len, func)
	return function(S)
		local flt = func(S.source, S.index)
		S.index = S.index + len

		return flt
	end
end

local function stm_instructions(S)
	local size = S:s_int()
	local code = {}

	for i = 1, size do
		local ins = S:s_ins()
		local op = bit.band(ins, 0x3F)
		local args = opcode_t[op]
		local mode = opcode_m[op]
		local data = {value = ins, op = op, A = bit.band(bit.rshift(ins, 6), 0xFF)}

		if args == 'ABC' then
			data.B = bit.band(bit.rshift(ins, 23), 0x1FF)
			data.C = bit.band(bit.rshift(ins, 14), 0x1FF)
			data.is_KB = mode.b == 'OpArgK' and data.B > 0xFF -- post process optimization
			data.is_KC = mode.c == 'OpArgK' and data.C > 0xFF
		elseif args == 'ABx' then
			data.Bx = bit.band(bit.rshift(ins, 14), 0x3FFFF)
			data.is_K = mode.b == 'OpArgK'
		elseif args == 'AsBx' then
			data.sBx = bit.band(bit.rshift(ins, 14), 0x3FFFF) - 131071
		end

		code[i] = data
	end

	return code
end

local function stm_constants(S)
	local size = S:s_int()
	local consts = {}

	for i = 1, size do
		local tt = stm_byte(S)
		local k

		if tt == 1 then
			k = stm_byte(S) ~= 0
		elseif tt == 3 then
			k = S:s_num()
		elseif tt == 4 then
			k = stm_lstring(S)
		end

		consts[i] = k -- offset +1 during instruction decode
	end

	return consts
end

local function stm_subfuncs(S, src)
	local size = S:s_int()
	local sub = {}

	for i = 1, size do
		sub[i] = stm_lua_func(S, src) -- offset +1 in CLOSURE
	end

	return sub
end

local function stm_lineinfo(S)
	local size = S:s_int()
	local lines = {}

	for i = 1, size do lines[i] = S:s_int() end

	return lines
end

local function stm_locvars(S)
	local size = S:s_int()
	local locvars = {}

	for i = 1, size do locvars[i] = {varname = stm_lstring(S), startpc = S:s_int(), endpc = S:s_int()} end

	return locvars
end

local function stm_upvals(S)
	local size = S:s_int()
	local upvals = {}

	for i = 1, size do upvals[i] = stm_lstring(S) end

	return upvals
end

function stm_lua_func(S, psrc)
	local proto = {}
	local src = stm_lstring(S) or psrc -- source is propagated

	proto.source = src -- source name

	S:s_int() -- line defined
	S:s_int() -- last line defined

	proto.numupvals = stm_byte(S) -- num upvalues
	proto.numparams = stm_byte(S) -- num params

	stm_byte(S) -- vararg flag
	stm_byte(S) -- max stack size

	proto.code = stm_instructions(S)
	proto.const = stm_constants(S)
	proto.subs = stm_subfuncs(S, src)
	proto.lines = stm_lineinfo(S)

	stm_locvars(S)
	stm_upvals(S)

	-- post process optimization
	for _, v in ipairs(proto.code) do
		if v.is_K then
			v.const = proto.const[v.Bx + 1] -- offset for 1 based index
		else
			if v.is_KB then v.const_B = proto.const[v.B - 0xFF] end

			if v.is_KC then v.const_C = proto.const[v.C - 0xFF] end
		end
	end

	return proto
end

function stm_lua_bytecode(src)
	-- func reader
	local rdr_func

	-- header flags
	local little
	local size_int
	local size_szt
	local size_ins
	local size_num
	local flag_int

	-- stream object
	local stream = {
		-- data
		index = 1,
		source = src,
	}

	assert(stm_string(stream, 4) == '\27Lua', 'invalid Lua signature')
	assert(stm_byte(stream) == 0x51, 'invalid Lua version')
	assert(stm_byte(stream) == 0, 'invalid Lua format')

	little = stm_byte(stream) ~= 0
	size_int = stm_byte(stream)
	size_szt = stm_byte(stream)
	size_ins = stm_byte(stream)
	size_num = stm_byte(stream)
	flag_int = stm_byte(stream) ~= 0

	rdr_func = little and rd_int_le or rd_int_be
	stream.s_int = cst_int_rdr(size_int, rdr_func)
	stream.s_szt = cst_int_rdr(size_szt, rdr_func)
	stream.s_ins = cst_int_rdr(size_ins, rdr_func)

	if flag_int then
		stream.s_num = cst_int_rdr(size_num, rdr_func)
	elseif float_types[size_num] then
		stream.s_num = cst_flt_rdr(size_num, float_types[size_num][little and 'little' or 'big'])
	else
		error('unsupported float size')
	end

	return stm_lua_func(stream, '@virtual')
end

local function close_lua_upvalues(list, index)
	for i, uv in pairs(list) do
		if uv.index >= index then
			uv.value = uv.store[uv.index] -- store value
			uv.store = uv
			uv.index = 'value' -- self reference
			list[i] = nil
		end
	end
end

local function open_lua_upvalue(list, index, stack)
	local prev = list[index]

	if not prev then
		prev = {index = index, store = stack}
		list[index] = prev
	end

	return prev
end

local function wrap_lua_variadic(...) return select('#', ...), {...} end

local function on_lua_error(exst, err)
	local src = exst.source
	local line = exst.lines[exst.pc - 1]
	local psrc, pline, pmsg = string.match(err, '^(.-):(%d+):%s+(.+)')
	local fmt = '%s:%i: [%s:%i] %s'

	line = line or '0'
	psrc = psrc or '?'
	pline = pline or '0'
	pmsg = pmsg or err

	error(string.format(fmt, src, line, psrc, pline, pmsg), 0)
end

local function exec_lua_func(exst)
	-- localize for easy lookup
	local code = exst.code
	local subs = exst.subs
	local env = exst.env
	local upvs = exst.upvals
	local vargs = exst.varargs

	-- state variables
	local stktop = -1
	local openupvs = {}
	local stack = exst.stack
	local pc = exst.pc

	while true do
		local inst = code[pc]
		local op = inst.op
		pc = pc + 1

		if op < 19 then
			if op < 9 then
				if op < 4 then
					if op < 2 then
						if op < 1 then
							--[[0 MOVE]]
							stack[inst.A] = stack[inst.B]
						else
							--[[1 LOADK]]
							stack[inst.A] = inst.const
						end
					elseif op > 2 then
						--[[3 LOADNIL]]
						for i = inst.A, inst.B do stack[i] = nil end
					else
						--[[2 LOADBOOL]]
						stack[inst.A] = inst.B ~= 0

						if inst.C ~= 0 then pc = pc + 1 end
					end
				elseif op > 4 then
					if op < 7 then
						if op < 6 then
							--[[5 GETGLOBAL]]
							stack[inst.A] = env[inst.const]
						else
							--[[6 GETTABLE]]
							local index

							if inst.is_KC then
								index = inst.const_C
							else
								index = stack[inst.C]
							end

							stack[inst.A] = stack[inst.B][index]
						end
					elseif op > 7 then
						--[[8 SETUPVAL]]
						local uv = upvs[inst.B]

						uv.store[uv.index] = stack[inst.A]
					else
						--[[7 SETGLOBAL]]
						env[inst.const] = stack[inst.A]
					end
				else
					--[[4 GETUPVAL]]
					local uv = upvs[inst.B]

					stack[inst.A] = uv.store[uv.index]
				end
			elseif op > 9 then
				if op < 14 then
					if op < 12 then
						if op < 11 then
							--[[10 NEWTABLE]]
							stack[inst.A] = {}
						else
							--[[11 SELF]]
							local A = inst.A
							local B = inst.B
							local index

							if inst.is_KC then
								index = inst.const_C
							else
								index = stack[inst.C]
							end

							stack[A + 1] = stack[B]
							stack[A] = stack[B][index]
						end
					elseif op > 12 then
						--[[13 SUB]]
						local lhs, rhs

						if inst.is_KB then
							lhs = inst.const_B
						else
							lhs = stack[inst.B]
						end

						if inst.is_KC then
							rhs = inst.const_C
						else
							rhs = stack[inst.C]
						end

						stack[inst.A] = lhs - rhs
					else
						--[[12 ADD]]
						local lhs, rhs

						if inst.is_KB then
							lhs = inst.const_B
						else
							lhs = stack[inst.B]
						end

						if inst.is_KC then
							rhs = inst.const_C
						else
							rhs = stack[inst.C]
						end

						stack[inst.A] = lhs + rhs
					end
				elseif op > 14 then
					if op < 17 then
						if op < 16 then
							--[[15 DIV]]
							local lhs, rhs

							if inst.is_KB then
								lhs = inst.const_B
							else
								lhs = stack[inst.B]
							end

							if inst.is_KC then
								rhs = inst.const_C
							else
								rhs = stack[inst.C]
							end

							stack[inst.A] = lhs / rhs
						else
							--[[16 MOD]]
							local lhs, rhs

							if inst.is_KB then
								lhs = inst.const_B
							else
								lhs = stack[inst.B]
							end

							if inst.is_KC then
								rhs = inst.const_C
							else
								rhs = stack[inst.C]
							end

							stack[inst.A] = lhs % rhs
						end
					elseif op > 17 then
						--[[18 UNM]]
						stack[inst.A] = -stack[inst.B]
					else
						--[[17 POW]]
						local lhs, rhs

						if inst.is_KB then
							lhs = inst.const_B
						else
							lhs = stack[inst.B]
						end

						if inst.is_KC then
							rhs = inst.const_C
						else
							rhs = stack[inst.C]
						end

						stack[inst.A] = lhs ^ rhs
					end
				else
					--[[14 MUL]]
					local lhs, rhs

					if inst.is_KB then
						lhs = inst.const_B
					else
						lhs = stack[inst.B]
					end

					if inst.is_KC then
						rhs = inst.const_C
					else
						rhs = stack[inst.C]
					end

					stack[inst.A] = lhs * rhs
				end
			else
				--[[9 SETTABLE]]
				local index, value

				if inst.is_KB then
					index = inst.const_B
				else
					index = stack[inst.B]
				end

				if inst.is_KC then
					value = inst.const_C
				else
					value = stack[inst.C]
				end

				stack[inst.A][index] = value
			end
		elseif op > 19 then
			if op < 29 then
				if op < 24 then
					if op < 22 then
						if op < 21 then
							--[[20 LEN]]
							stack[inst.A] = #stack[inst.B]
						else
							--[[21 CONCAT]]
							local str = stack[inst.B]

							for i = inst.B + 1, inst.C do str = str .. stack[i] end

							stack[inst.A] = str
						end
					elseif op > 22 then
						--[[23 EQ]]
						local lhs, rhs

						if inst.is_KB then
							lhs = inst.const_B
						else
							lhs = stack[inst.B]
						end

						if inst.is_KC then
							rhs = inst.const_C
						else
							rhs = stack[inst.C]
						end

						if (lhs == rhs) ~= (inst.A ~= 0) then pc = pc + 1 end
					else
						--[[22 JMP]]
						pc = pc + inst.sBx
					end
				elseif op > 24 then
					if op < 27 then
						if op < 26 then
							--[[25 LE]]
							local lhs, rhs

							if inst.is_KB then
								lhs = inst.const_B
							else
								lhs = stack[inst.B]
							end

							if inst.is_KC then
								rhs = inst.const_C
							else
								rhs = stack[inst.C]
							end

							if (lhs <= rhs) ~= (inst.A ~= 0) then pc = pc + 1 end
						else
							--[[26 TEST]]
							if (not stack[inst.A]) == (inst.C ~= 0) then pc = pc + 1 end
						end
					elseif op > 27 then
						--[[28 CALL]]
						local A = inst.A
						local B = inst.B
						local C = inst.C
						local params
						local sz_vals, l_vals

						if B == 0 then
							params = stktop - A
						else
							params = B - 1
						end

						sz_vals, l_vals = wrap_lua_variadic(stack[A](unpack(stack, A + 1, A + params)))

						if C == 0 then
							stktop = A + sz_vals - 1
						else
							sz_vals = C - 1
						end

						for i = 1, sz_vals do stack[A + i - 1] = l_vals[i] end
					else
						--[[27 TESTSET]]
						local A = inst.A
						local B = inst.B

						if (not stack[B]) == (inst.C ~= 0) then
							pc = pc + 1
						else
							stack[A] = stack[B]
						end
					end
				else
					--[[24 LT]]
					local lhs, rhs

					if inst.is_KB then
						lhs = inst.const_B
					else
						lhs = stack[inst.B]
					end

					if inst.is_KC then
						rhs = inst.const_C
					else
						rhs = stack[inst.C]
					end

					if (lhs < rhs) ~= (inst.A ~= 0) then pc = pc + 1 end
				end
			elseif op > 29 then
				if op < 34 then
					if op < 32 then
						if op < 31 then
							--[[30 RETURN]]
							local A = inst.A
							local B = inst.B
							local vals = {}
							local size

							if B == 0 then
								size = stktop - A + 1
							else
								size = B - 1
							end

							for i = 1, size do vals[i] = stack[A + i - 1] end

							close_lua_upvalues(openupvs, 0)
							return size, vals
						else
							--[[31 FORLOOP]]
							local A = inst.A
							local step = stack[A + 2]
							local index = stack[A] + step
							local limit = stack[A + 1]
							local loops

							if step == math.abs(step) then
								loops = index <= limit
							else
								loops = index >= limit
							end

							if loops then
								stack[inst.A] = index
								stack[inst.A + 3] = index
								pc = pc + inst.sBx
							end
						end
					elseif op > 32 then
						--[[33 TFORLOOP]]
						local A = inst.A
						local func = stack[A]
						local state = stack[A + 1]
						local index = stack[A + 2]
						local base = A + 3
						local vals

						stack[base + 2] = index
						stack[base + 1] = state
						stack[base] = func

						vals = {func(state, index)}

						for i = 1, inst.C do stack[base + i - 1] = vals[i] end

						if stack[base] ~= nil then
							stack[A + 2] = stack[base]
						else
							pc = pc + 1
						end
					else
						--[[32 FORPREP]]
						local A = inst.A
						local init, limit, step

						init = assert(tonumber(stack[A]), '`for` initial value must be a number')
						limit = assert(tonumber(stack[A + 1]), '`for` limit must be a number')
						step = assert(tonumber(stack[A + 2]), '`for` step must be a number')

						stack[A] = init - step
						stack[A + 1] = limit
						stack[A + 2] = step

						pc = pc + inst.sBx
					end
				elseif op > 34 then
					if op < 36 then
						--[[35 CLOSE]]
						close_lua_upvalues(openupvs, inst.A)
					elseif op > 36 then
						--[[37 VARARG]]
						local A = inst.A
						local size = inst.B

						if size == 0 then
							size = vargs.size
							stktop = A + size - 1
						end

						for i = 1, size do stack[A + i - 1] = vargs.list[i] end
					else
						--[[36 CLOSURE]]
						local sub = subs[inst.Bx + 1] -- offset for 1 based index
						local nups = sub.numupvals
						local uvlist

						if nups ~= 0 then
							uvlist = {}

							for i = 1, nups do
								local pseudo = code[pc + i - 1]

								if pseudo.op == 0 then -- @MOVE
									uvlist[i - 1] = open_lua_upvalue(openupvs, pseudo.B, stack)
								elseif pseudo.op == 4 then -- @GETUPVAL
									uvlist[i - 1] = upvs[pseudo.B]
								end
							end

							pc = pc + nups
						end

						stack[inst.A] = wrap_lua_func(sub, env, uvlist)
					end
				else
					--[[34 SETLIST]]
					local A = inst.A
					local C = inst.C
					local size = inst.B
					local tab = stack[A]
					local offset

					if size == 0 then size = stktop - A end

					if C == 0 then
						C = inst[pc].value
						pc = pc + 1
					end

					offset = (C - 1) * FIELDS_PER_FLUSH

					for i = 1, size do tab[i + offset] = stack[A + i] end
				end
			else
				--[[29 TAILCALL]]
				local A = inst.A
				local B = inst.B
				local params

				if B == 0 then
					params = stktop - A
				else
					params = B - 1
				end

				close_lua_upvalues(openupvs, 0)
				return wrap_lua_variadic(stack[A](unpack(stack, A + 1, A + params)))
			end
		else
			--[[19 NOT]]
			stack[inst.A] = not stack[inst.B]
		end

		exst.pc = pc
	end
end

function wrap_lua_func(state, env, upvals)
	local st_code = state.code
	local st_subs = state.subs
	local st_lines = state.lines
	local st_source = state.source
	local st_numparams = state.numparams

	local function exec_wrap(...)
		local stack = {}
		local varargs = {}
		local sizevarg = 0
		local sz_args, l_args = wrap_lua_variadic(...)

		local exst
		local ok, err, vals

		for i = 1, st_numparams do stack[i - 1] = l_args[i] end

		if st_numparams < sz_args then
			sizevarg = sz_args - st_numparams
			for i = 1, sizevarg do varargs[i] = l_args[st_numparams + i] end
		end

		exst = {
			varargs = {list = varargs, size = sizevarg},
			code = st_code,
			subs = st_subs,
			lines = st_lines,
			source = st_source,
			env = env,
			upvals = upvals,
			stack = stack,
			pc = 1,
		}

		ok, err, vals = pcall(exec_lua_func, exst, ...)

		if ok then
			return unpack(vals, 1, err)
		else
			on_lua_error(exst, err)
		end

		return
	end

	return exec_wrap
end

return {stm_lua = stm_lua_bytecode, wrap_lua = wrap_lua_func}

------ JSON PARSER ------

local t = {}
 
local string = string
local math = math
local table = table
local error = error
local tonumber = tonumber
local tostring = tostring
local type = type
local setmetatable = setmetatable
local pairs = pairs
local ipairs = ipairs
local assert = assert


local StringBuilder = {
    buffer = {}
}

function StringBuilder:New()
    local o = {}
    setmetatable(o, self)
    self.__index = self
    o.buffer = {}
    return o
end

function StringBuilder:Append(s)
    self.buffer[#self.buffer+1] = s
end

function StringBuilder:ToString()
    return table.concat(self.buffer)
end

local JsonWriter = {
    backslashes = {
        ['\b'] = "\\b",
        ['\t'] = "\\t", 
        ['\n'] = "\\n", 
        ['\f'] = "\\f",
        ['\r'] = "\\r", 
        ['"']  = "\\\"", 
        ['\\'] = "\\\\", 
        ['/']  = "\\/"
    }
}

function JsonWriter:New()
    local o = {}
    o.writer = StringBuilder:New()
    setmetatable(o, self)
    self.__index = self
    return o
end

function JsonWriter:Append(s)
    self.writer:Append(s)
end

function JsonWriter:ToString()
    return self.writer:ToString()
end

function JsonWriter:Write(o)
    local t = type(o)
    if t == "nil" then
        self:WriteNil()
    elseif t == "boolean" then
        self:WriteString(o)
    elseif t == "number" then
        self:WriteString(o)
    elseif t == "string" then
        self:ParseString(o)
    elseif t == "table" then
        self:WriteTable(o)
    elseif t == "function" then
        self:WriteFunction(o)
    elseif t == "thread" then
        self:WriteError(o)
    elseif t == "userdata" then
        self:WriteError(o)
    end
end

function JsonWriter:WriteNil()
    self:Append("null")
end

function JsonWriter:WriteString(o)
    self:Append(tostring(o))
end

function JsonWriter:ParseString(s)
    self:Append('"')
    self:Append(string.gsub(s, "[%z%c\\\"/]", function(n)
        local c = self.backslashes[n]
        if c then return c end
        return string.format("\\u%.4X", string.byte(n))
    end))
    self:Append('"')
end

function JsonWriter:IsArray(t)
    local count = 0
    local isindex = function(k) 
        if type(k) == "number" and k > 0 then
            if math.floor(k) == k then
                return true
            end
        end
        return false
    end
    for k,v in pairs(t) do
        if not isindex(k) then
            return false, '{', '}'
        else
            count = math.max(count, k)
        end
    end
    return true, '[', ']', count
end

function JsonWriter:WriteTable(t)
    local ba, st, et, n = self:IsArray(t)
    self:Append(st) 
    if ba then      
        for i = 1, n do
            self:Write(t[i])
            if i < n then
                self:Append(',')
            end
        end
    else
        local first = true;
        for k, v in pairs(t) do
            if not first then
                self:Append(',')
            end
            first = false;          
            self:ParseString(k)
            self:Append(':')
            self:Write(v)           
        end
    end
    self:Append(et)
end

function JsonWriter:WriteError(o)
    error(string.format(
        "Encoding of %s unsupported", 
        tostring(o)))
end

function JsonWriter:WriteFunction(o)
    if o == Null then 
        self:WriteNil()
    else
        self:WriteError(o)
    end
end

local StringReader = {
    s = "",
    i = 0
}

function StringReader:New(s)
    local o = {}
    setmetatable(o, self)
    self.__index = self
    o.s = s or o.s
    return o    
end

function StringReader:Peek()
    local i = self.i + 1
    if i <= #self.s then
        return string.sub(self.s, i, i)
    end
    return nil
end

function StringReader:Next()
    self.i = self.i+1
    if self.i <= #self.s then
        return string.sub(self.s, self.i, self.i)
    end
    return nil
end

function StringReader:All()
    return self.s
end

local JsonReader = {
    escapes = {
        ['t'] = '\t',
        ['n'] = '\n',
        ['f'] = '\f',
        ['r'] = '\r',
        ['b'] = '\b',
    }
}

function JsonReader:New(s)
    local o = {}
    o.reader = StringReader:New(s)
    setmetatable(o, self)
    self.__index = self
    return o;
end

function JsonReader:Read()
    self:SkipWhiteSpace()
    local peek = self:Peek()
    if peek == nil then
        error(string.format(
            "Nil string: '%s'", 
            self:All()))
    elseif peek == '{' then
        return self:ReadObject()
    elseif peek == '[' then
        return self:ReadArray()
    elseif peek == '"' then
        return self:ReadString()
    elseif string.find(peek, "[%+%-%d]") then
        return self:ReadNumber()
    elseif peek == 't' then
        return self:ReadTrue()
    elseif peek == 'f' then
        return self:ReadFalse()
    elseif peek == 'n' then
        return self:ReadNull()
    elseif peek == '/' then
        self:ReadComment()
        return self:Read()
    else
        return nil
    end
end
        
function JsonReader:ReadTrue()
    self:TestReservedWord{'t','r','u','e'}
    return true
end

function JsonReader:ReadFalse()
    self:TestReservedWord{'f','a','l','s','e'}
    return false
end

function JsonReader:ReadNull()
    self:TestReservedWord{'n','u','l','l'}
    return nil
end

function JsonReader:TestReservedWord(t)
    for i, v in ipairs(t) do
        if self:Next() ~= v then
             error(string.format(
                "Error reading '%s': %s", 
                table.concat(t), 
                self:All()))
        end
    end
end

function JsonReader:ReadNumber()
        local result = self:Next()
        local peek = self:Peek()
        while peek ~= nil and string.find(
        peek, 
        "[%+%-%d%.eE]") do
            result = result .. self:Next()
            peek = self:Peek()
    end
    result = tonumber(result)
    if result == nil then
            error(string.format(
            "Invalid number: '%s'", 
            result))
    else
        return result
    end
end

function JsonReader:ReadString()
    local result = ""
    assert(self:Next() == '"')
        while self:Peek() ~= '"' do
        local ch = self:Next()
        if ch == '\\' then
            ch = self:Next()
            if self.escapes[ch] then
                ch = self.escapes[ch]
            end
        end
                result = result .. ch
    end
        assert(self:Next() == '"')
    local fromunicode = function(m)
        return string.char(tonumber(m, 16))
    end
    return string.gsub(
        result, 
        "u%x%x(%x%x)", 
        fromunicode)
end

function JsonReader:ReadComment()
        assert(self:Next() == '/')
        local second = self:Next()
        if second == '/' then
            self:ReadSingleLineComment()
        elseif second == '*' then
            self:ReadBlockComment()
        else
            error(string.format(
        "Invalid comment: %s", 
        self:All()))
    end
end

function JsonReader:ReadBlockComment()
    local done = false
    while not done do
        local ch = self:Next()      
        if ch == '*' and self:Peek() == '/' then
            done = true
                end
        if not done and 
            ch == '/' and 
            self:Peek() == "*" then
                    error(string.format(
            "Invalid comment: %s, '/*' illegal.",  
            self:All()))
        end
    end
    self:Next()
end

function JsonReader:ReadSingleLineComment()
    local ch = self:Next()
    while ch ~= '\r' and ch ~= '\n' do
        ch = self:Next()
    end
end

function JsonReader:ReadArray()
    local result = {}
    assert(self:Next() == '[')
    local done = false
    if self:Peek() == ']' then
        done = true;
    end
    while not done do
        local item = self:Read()
        result[#result+1] = item
        self:SkipWhiteSpace()
        if self:Peek() == ']' then
            done = true
        end
        if not done then
            local ch = self:Next()
            if ch ~= ',' then
                error(string.format(
                    "Invalid array: '%s' due to: '%s'", 
                    self:All(), ch))
            end
        end
    end
    assert(']' == self:Next())
    return result
end

function JsonReader:ReadObject()
    local result = {}
    assert(self:Next() == '{')
    local done = false
    if self:Peek() == '}' then
        done = true
    end
    while not done do
        local key = self:Read()
        if type(key) ~= "string" then
            error(string.format(
                "Invalid non-string object key: %s", 
                key))
        end
        self:SkipWhiteSpace()
        local ch = self:Next()
        if ch ~= ':' then
            error(string.format(
                "Invalid object: '%s' due to: '%s'", 
                self:All(), 
                ch))
        end
        self:SkipWhiteSpace()
        local val = self:Read()
        result[key] = val
        self:SkipWhiteSpace()
        if self:Peek() == '}' then
            done = true
        end
        if not done then
            ch = self:Next()
                    if ch ~= ',' then
                error(string.format(
                    "Invalid array: '%s' near: '%s'", 
                    self:All(), 
                    ch))
            end
        end
    end
    assert(self:Next() == "}")
    return result
end

function JsonReader:SkipWhiteSpace()
    local p = self:Peek()
    while p ~= nil and string.find(p, "[%s/]") do
        if p == '/' then
            self:ReadComment()
        else
            self:Next()
        end
        p = self:Peek()
    end
end

function JsonReader:Peek()
    return self.reader:Peek()
end

function JsonReader:Next()
    return self.reader:Next()
end

function JsonReader:All()
    return self.reader:All()
end

function Encode(o)
    local writer = JsonWriter:New()
    writer:Write(o)
    return writer:ToString()
end

function Decode(s)
    local reader = JsonReader:New(s)
    return reader:Read()
end

function Null()
    return Null
end
-------------------- End JSON Parser ------------------------

t.DecodeJSON = function(jsonString)
    pcall(function() warn("RbxUtility.DecodeJSON is deprecated, please use Game:GetService('HttpService'):JSONDecode() instead.") end)

    if type(jsonString) == "string" then
        return Decode(jsonString)
    end
    print("RbxUtil.DecodeJSON expects string argument!")
    return nil
end

t.EncodeJSON = function(jsonTable)
    pcall(function() warn("RbxUtility.EncodeJSON is deprecated, please use Game:GetService('HttpService'):JSONEncode() instead.") end)
    return Encode(jsonTable)
end

t.MakeWedge = function(x, y, z, defaultmaterial)
    return game:GetService("Terrain"):AutoWedgeCell(x,y,z)
end

t.SelectTerrainRegion = function(regionToSelect, color, selectEmptyCells, selectionParent)
    local terrain = game:GetService("Workspace"):FindFirstChild("Terrain")
    if not terrain then return end

    assert(regionToSelect)
    assert(color)

    if not type(regionToSelect) == "Region3" then
        error("regionToSelect (first arg), should be of type Region3, but is type",type(regionToSelect))
    end
    if not type(color) == "BrickColor" then
        error("color (second arg), should be of type BrickColor, but is type",type(color))
    end

    local GetCell = terrain.GetCell
    local WorldToCellPreferSolid = terrain.WorldToCellPreferSolid
    local CellCenterToWorld = terrain.CellCenterToWorld
    local emptyMaterial = Enum.CellMaterial.Empty

    local selectionContainer = Instance.new("Model")
    selectionContainer.Name = "SelectionContainer"
    selectionContainer.Archivable = false
    if selectionParent then
        selectionContainer.Parent = selectionParent
    else
        selectionContainer.Parent = game:GetService("Workspace")
    end

    local updateSelection = nil -- function we return to allow user to update selection
    local currentKeepAliveTag = nil -- a tag that determines whether adorns should be destroyed
    local aliveCounter = 0 -- helper for currentKeepAliveTag
    local lastRegion = nil -- used to stop updates that do nothing
    local adornments = {} -- contains all adornments
    local reusableAdorns = {}

    local selectionPart = Instance.new("Part")
    selectionPart.Name = "SelectionPart"
    selectionPart.Transparency = 1
    selectionPart.Anchored = true
    selectionPart.Locked = true
    selectionPart.CanCollide = false
    selectionPart.Size = Vector3.new(4.2,4.2,4.2)

    local selectionBox = Instance.new("SelectionBox")

    -- srs translation from region3 to region3int16
    local function Region3ToRegion3int16(region3)
        local theLowVec = region3.CFrame.p - (region3.Size/2) + Vector3.new(2,2,2)
        local lowCell = WorldToCellPreferSolid(terrain,theLowVec)

        local theHighVec = region3.CFrame.p + (region3.Size/2) - Vector3.new(2,2,2)
        local highCell = WorldToCellPreferSolid(terrain, theHighVec)

        local highIntVec = Vector3int16.new(highCell.x,highCell.y,highCell.z)
        local lowIntVec = Vector3int16.new(lowCell.x,lowCell.y,lowCell.z)

        return Region3int16.new(lowIntVec,highIntVec)
    end

    function createAdornment(theColor)
        local selectionPartClone = nil
        local selectionBoxClone = nil

        if #reusableAdorns > 0 then
            selectionPartClone = reusableAdorns[1]["part"]
            selectionBoxClone = reusableAdorns[1]["box"]
            table.remove(reusableAdorns,1)

            selectionBoxClone.Visible = true
        else
            selectionPartClone = selectionPart:Clone()
            selectionPartClone.Archivable = false

            selectionBoxClone = selectionBox:Clone()
            selectionBoxClone.Archivable = false

            selectionBoxClone.Adornee = selectionPartClone
            selectionBoxClone.Parent = selectionContainer

            selectionBoxClone.Adornee = selectionPartClone

            selectionBoxClone.Parent = selectionContainer
        end
            
        if theColor then
            selectionBoxClone.Color = theColor
        end

        return selectionPartClone, selectionBoxClone
    end

    function cleanUpAdornments()
        for cellPos, adornTable in pairs(adornments) do

            if adornTable.KeepAlive ~= currentKeepAliveTag then -- old news, we should get rid of this
                adornTable.SelectionBox.Visible = false
                table.insert(reusableAdorns,{part = adornTable.SelectionPart, box = adornTable.SelectionBox})
                adornments[cellPos] = nil
            end
        end
    end

    function incrementAliveCounter()
        aliveCounter = aliveCounter + 1
        if aliveCounter > 1000000 then
            aliveCounter = 0
        end
        return aliveCounter
    end

    function adornFullCellsInRegion(region, color)
        local regionBegin = region.CFrame.p - (region.Size/2) + Vector3.new(2,2,2)
        local regionEnd = region.CFrame.p + (region.Size/2) - Vector3.new(2,2,2)

        local cellPosBegin = WorldToCellPreferSolid(terrain, regionBegin)
        local cellPosEnd = WorldToCellPreferSolid(terrain, regionEnd)

        currentKeepAliveTag = incrementAliveCounter()
        for y = cellPosBegin.y, cellPosEnd.y do
            for z = cellPosBegin.z, cellPosEnd.z do
                for x = cellPosBegin.x, cellPosEnd.x do
                    local cellMaterial = GetCell(terrain, x, y, z)
                    
                    if cellMaterial ~= emptyMaterial then
                        local cframePos = CellCenterToWorld(terrain, x, y, z)
                        local cellPos = Vector3int16.new(x,y,z)

                        local updated = false
                        for cellPosAdorn, adornTable in pairs(adornments) do
                            if cellPosAdorn == cellPos then
                                adornTable.KeepAlive = currentKeepAliveTag
                                if color then
                                    adornTable.SelectionBox.Color = color
                                end
                                updated = true
                                break
                            end 
                        end

                        if not updated then
                            local selectionPart, selectionBox = createAdornment(color)
                            selectionPart.Size = Vector3.new(4,4,4)
                            selectionPart.CFrame = CFrame.new(cframePos)
                            local adornTable = {SelectionPart = selectionPart, SelectionBox = selectionBox, KeepAlive = currentKeepAliveTag}
                            adornments[cellPos] = adornTable
                        end
                    end
                end
            end
        end
        cleanUpAdornments()
    end


    lastRegion = regionToSelect

    if selectEmptyCells then -- use one big selection to represent the area selected
        local selectionPart, selectionBox = createAdornment(color)

        selectionPart.Size = regionToSelect.Size
        selectionPart.CFrame = regionToSelect.CFrame

        adornments.SelectionPart = selectionPart
        adornments.SelectionBox = selectionBox

        updateSelection = 
            function (newRegion, color)
                if newRegion and newRegion ~= lastRegion then
                    lastRegion = newRegion
                    selectionPart.Size = newRegion.Size
                    selectionPart.CFrame = newRegion.CFrame
                end
                if color then
                    selectionBox.Color = color
                end
            end
    else -- use individual cell adorns to represent the area selected
        adornFullCellsInRegion(regionToSelect, color)
        updateSelection = 
            function (newRegion, color)
                if newRegion and newRegion ~= lastRegion then
                    lastRegion = newRegion
                    adornFullCellsInRegion(newRegion, color)
                end
            end

    end

    local destroyFunc = function()
        updateSelection = nil
        if selectionContainer then selectionContainer:Destroy() end
        adornments = nil
    end

    return updateSelection, destroyFunc
end

function t.CreateSignal()
    local this = {}

    local mBindableEvent = Instance.new('BindableEvent')
    local mAllCns = {} --all connection objects returned by mBindableEvent::connect

    --main functions
    function this:connect(func)
        if self ~= this then error("connect must be called with `:`, not `.`", 2) end
        if type(func) ~= 'function' then
            error("Argument #1 of connect must be a function, got a "..type(func), 2)
        end
        local cn = mBindableEvent.Event:Connect(func)
        mAllCns[cn] = true
        local pubCn = {}
        function pubCn:disconnect()
            cn:Disconnect()
            mAllCns[cn] = nil
        end
        pubCn.Disconnect = pubCn.disconnect
        
        return pubCn
    end
    
    function this:disconnect()
        if self ~= this then error("disconnect must be called with `:`, not `.`", 2) end
        for cn, _ in pairs(mAllCns) do
            cn:Disconnect()
            mAllCns[cn] = nil
        end
    end
    
    function this:wait()
        if self ~= this then error("wait must be called with `:`, not `.`", 2) end
        return mBindableEvent.Event:Wait()
    end
    
    function this:fire(...)
        if self ~= this then error("fire must be called with `:`, not `.`", 2) end
        mBindableEvent:Fire(...)
    end
    
    this.Connect = this.connect
    this.Disconnect = this.disconnect
    this.Wait = this.wait
    this.Fire = this.fire

    return this
end

local function Create_PrivImpl(objectType)
    if type(objectType) ~= 'string' then
        error("Argument of Create must be a string", 2)
    end
    return function(dat)
        dat = dat or {}

        local obj = Instance.new(objectType)
        local parent = nil

        local ctor = nil

        for k, v in pairs(dat) do
            --add property
            if type(k) == 'string' then
                if k == 'Parent' then
                    parent = v
                else
                    obj[k] = v
                end

            elseif type(k) == 'number' then
                if type(v) ~= 'userdata' then
                    error("Bad entry in Create body: Numeric keys must be paired with children, got a: "..type(v), 2)
                end
                v.Parent = obj


            elseif type(k) == 'table' and k.__eventname then
                if type(v) ~= 'function' then
                    error("Bad entry in Create body: Key `[Create.E\'"..k.__eventname.."\']` must have a function value\
                           got: "..tostring(v), 2)
                end
                obj[k.__eventname]:connect(v)


            elseif k == t.Create then
                if type(v) ~= 'function' then
                    error("Bad entry in Create body: Key `[Create]` should be paired with a constructor function, \
                           got: "..tostring(v), 2)
                elseif ctor then
                    --ctor already exists, only one allowed
                    error("Bad entry in Create body: Only one constructor function is allowed", 2)
                end
                ctor = v


            else
                error("Bad entry ("..tostring(k).." => "..tostring(v)..") in Create body", 2)
            end
        end

        if ctor then
            ctor(obj)
        end
        
        if parent then
            obj.Parent = parent
        end

        return obj
    end
end

t.Create = setmetatable({}, {__call = function(tb, ...) return Create_PrivImpl(...) end})

t.Create.E = function(eventName)
    return {__eventname = eventName}
end

t.Help = 
    function(funcNameOrFunc) 
        if funcNameOrFunc == "DecodeJSON" or funcNameOrFunc == t.DecodeJSON then
            return "Function DecodeJSON.  " ..
                   "Arguments: (string).  " .. 
                   "Side effect: returns a table with all parsed JSON values" 
        end
        if funcNameOrFunc == "EncodeJSON" or funcNameOrFunc == t.EncodeJSON then
            return "Function EncodeJSON.  " ..
                   "Arguments: (table).  " .. 
                   "Side effect: returns a string composed of argument table in JSON data format" 
        end  
        if funcNameOrFunc == "MakeWedge" or funcNameOrFunc == t.MakeWedge then
            return "Function MakeWedge. " ..
                   "Arguments: (x, y, z, [default material]). " ..
                   "Description: Makes a wedge at location x, y, z. Sets cell x, y, z to default material if "..
                   "parameter is provided, if not sets cell x, y, z to be whatever material it previously was. "..
                   "Returns true if made a wedge, false if the cell remains a block "
        end
        if funcNameOrFunc == "SelectTerrainRegion" or funcNameOrFunc == t.SelectTerrainRegion then
            return "Function SelectTerrainRegion. " ..
                   "Arguments: (regionToSelect, color, selectEmptyCells, selectionParent). " ..
                   "Description: Selects all terrain via a series of selection boxes within the regionToSelect " ..
                   "(this should be a region3 value). The selection box color is detemined by the color argument " ..
                   "(should be a brickcolor value). SelectionParent is the parent that the selection model gets placed to (optional)." ..
                   "SelectEmptyCells is bool, when true will select all cells in the " ..
                   "region, otherwise we only select non-empty cells. Returns a function that can update the selection," ..
                   "arguments to said function are a new region3 to select, and the adornment color (color arg is optional). " ..
                   "Also returns a second function that takes no arguments and destroys the selection"
        end
        if funcNameOrFunc == "CreateSignal" or funcNameOrFunc == t.CreateSignal then
            return "Function CreateSignal. "..
                   "Arguments: None. "..
                   "Returns: The newly created Signal object. This object is identical to the RBXScriptSignal class "..
                   "used for events in Objects, but is a Lua-side object so it can be used to create custom events in"..
                   "Lua code. "..
                   "Methods of the Signal object: :connect, :wait, :fire, :disconnect. "..
                   "For more info you can pass the method name to the Help function, or view the wiki page "..
                   "for this library. EG: Help('Signal:connect')."
        end
        if funcNameOrFunc == "Signal:connect" then
            return "Method Signal:connect. "..
                   "Arguments: (function handler). "..
                   "Return: A connection object which can be used to disconnect the connection to this handler. "..
                   "Description: Connectes a handler function to this Signal, so that when |fire| is called the "..
                   "handler function will be called with the arguments passed to |fire|."
        end
        if funcNameOrFunc == "Signal:wait" then
            return "Method Signal:wait. "..
                   "Arguments: None. "..
                   "Returns: The arguments passed to the next call to |fire|. "..
                   "Description: This call does not return until the next call to |fire| is made, at which point it "..
                   "will return the values which were passed as arguments to that |fire| call."
        end
        if funcNameOrFunc == "Signal:fire" then
            return "Method Signal:fire. "..
                   "Arguments: Any number of arguments of any type. "..
                   "Returns: None. "..
                   "Description: This call will invoke any connected handler functions, and notify any waiting code "..
                   "attached to this Signal to continue, with the arguments passed to this function. Note: The calls "..
                   "to handlers are made asynchronously, so this call will return immediately regardless of how long "..
                   "it takes the connected handler functions to complete."
        end
        if funcNameOrFunc == "Signal:disconnect" then
            return "Method Signal:disconnect. "..
                   "Arguments: None. "..
                   "Returns: None. "..
                   "Description: This call disconnects all handlers attacched to this function, note however, it "..
                   "does NOT make waiting code continue, as is the behavior of normal Roblox events. This method "..
                   "can also be called on the connection object which is returned from Signal:connect to only "..
                   "disconnect a single handler, as opposed to this method, which will disconnect all handlers."
        end
        if funcNameOrFunc == "Create" then
            return "Function Create. "..
                   "Arguments: A table containing information about how to construct a collection of objects. "..
                   "Returns: The constructed objects. "..
                   "Descrition: Create is a very powerfull function, whose description is too long to fit here, and "..
                   "is best described via example, please see the wiki page for a description of how to use it."
        end
    end
    
return t
