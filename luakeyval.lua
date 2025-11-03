  --[[
  luakeyval version   = 0.1, 2025-11-25
  ]]--

local put_next = token.put_next
local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_keyword = token.scan_keyword

local relax = token.new(token.biggest_char() + 1)
local texerror = tex.error
local function check_delimiter(str1, str2)
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror(str1,{str2})
        put_next(tok)
    end
end

local unpack = table.unpack 
local function process_keys(tbl)
    local toks = scan_toks()
    put_next(relax)
    put_next(toks)
    local matched, vals = true, { }
    while matched do
	    matched = false
        for key, param in pairs(tbl) do
		    if scan_keyword(key) then
			    matched = true
				local args = param.args or {}
				local scanner = param.scanner
				local val = scan_keyword('=') and 
				  (scanner and scanner(unpack(args)) or true) or
				  (param.default or true)
			    local func = param.func
				if func then func(key,val) end
				vals[key] = val
				break
			end
		end
    end
	check_delimiter()
	return vals
end

local function scan_choice(...)
    local args = {...}
    for _, keyword in ipairs(args) do
        if scan_keyword(keyword) then
            return keyword
        end
    end
end

local function scan_bool()
    if scan_keyword('true') then 
	    return true
    elseif scan_keyword('flase') then
        return false
    end		
end

return {
    process = process_keys,
	choices = scan_choice,
	bool = scan_bool,
}