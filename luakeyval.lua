  --[[
  luakeyval version   = 0.1, 2025-11-28
  ]]--

local put_next = token.unchecked_put_next
local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_keyword = token.scan_keyword

local relax = token.new(token.biggest_char() + 1)
local texerror, uni_char = tex.error, utf8.char
local format = string.format
local function check_delimiter(str1, str2, key)
    local tok = get_next()
    if tok.tok ~= relax.tok then
        local tok_name = tok.csname or uni_char(tok.mode)
        texerror(format(str1, key, tok_name),{format(str2, key, tok_name)})
        put_next({tok})
    end
end

local unpack, insert = table.unpack, table.insert 
local function process_keys(tbl, str1, str2)
    local toks = scan_toks()
	insert(toks, relax)
    put_next(toks)
    local matched, vals = true, { }
    local curr_key
    while matched do
	    matched = false
        for key, param in pairs(tbl) do
		    if scan_keyword(key) then
			    matched = true
				curr_key = key
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
	check_delimiter(str1, str2, curr_key)
	return vals
end

local function scan_choice(...)
    local choices = {...}
    for _, choice in ipairs(choices) do
        if scan_keyword(choice) then
            return choice
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
