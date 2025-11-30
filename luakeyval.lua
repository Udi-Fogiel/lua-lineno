  --[[
  luakeyval version   = 0.1, 2025-11-28
  ]]--

local put_next = token.unchecked_put_next
local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_keyword = token.scan_keyword_cs

local relax = token.new(token.biggest_char() + 1)
local texerror, utfchar = tex.error, utf8.char
local format = string.format

local function check_delimiter(error1, error2, key)
    local tok = get_next()
    if tok.tok ~= relax.tok then
        local tok_name = tok.csname or utfchar(tok.mode)
        texerror(format(error1, key, tok_name),{format(error2, key, tok_name)})
        put_next({tok})
    end
end

local unpack, insert = table.unpack, table.insert 
local function process_keys(keys, messages)
    local matched, vals, curr_key = true, { }
    local value_forbidden = messages.value_forbidden
        or "luakeyval: the %s key does not accept a value"
    local value_required = messages.value_required
        or "luakeyval: the %s key require a value"
    local error1 = messages.error1
        or "wrong syntax when processing keys"
    local error2 = messages.error2
        or 'the last scanned key was "%s".\nthere is a "%s" in the way.'
    local toks = scan_toks()
    insert(toks, relax)
    put_next(toks)
    while matched do
        matched = false
        for key, param in pairs(keys) do
            if scan_keyword(key) then
                matched = true
                curr_key = key
                local args = param.args or { }
                local scanner = param.scanner
                local val = scan_keyword('=') and
                  (scanner and scanner(unpack(args)) or texerror(format(value_forbidden, key)))
                  or (param.default or texerror(format(value_required, key)))
                local func = param.func
                if func then func(key,val) end
                vals[key] = val
                break
            end
        end
    end
    check_delimiter(error1, error2, curr_key)
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
