  --[[
  lualineno version   = 0.1, 2025-01-19
  ]]--
-- \secc Initialization^^M
-- Currently the module works only with 
-- Lua\LaTeX/ and \OpTeX.

local format = tex.formatname
local texerror = tex.error

local optex, latex, plain

if format:find("optex") then -- OpTeX
    optex = true
elseif format:find("latex") then -- lualatex, lualatex-dev, ...
    latex = true
elseif format == "luatex" or
       format == "luahbtex" or
       format:find("plain")
then -- Plain
    plain = true
end


if not (optex or latex or plain) then
    error("lualineno: The format " .. format .. " is not supported\n\n" ..
             "Use OpTeX, LuaLaTeX or Plain.")
end

-- To make sure the tokens used has the correct meaning
-- we define them as new primitives. We use a prefix
-- unlikely used by others, but just in case if a macro
-- was overwritten we restore it at the end. 

local runtoks = tex.runtoks
local put_next = token.put_next
local create = token.create
local new_tok = token.new

local lbrace, rbrace = new_tok(string.byte('{'), 1), new_tok(string.byte('}'), 2)
local relax, hbox
do
-- initialization of the new primitives.
  local prefix = '@lua^line&no_' -- unlikely prefix...
  while token.is_defined(prefix .. 'let') or token.is_defined(prefix .. 'relax')
    or token.is_defined(prefix .. 'hbox') do
    prefix = prefix .. '@lua^line&no_'
  end
  local undef = create(prefix .. 'relax')
  tex.enableprimitives(prefix,{'relax', 'hbox', 'let'})
-- Now we create new tokens with the meaning of 
-- the primitives. It is very hard to change their
-- meaning from the user end (or even impossible?).
-- Credit to 
  local function frozentok(name)
      local tok = create(prefix .. name)
      return new_tok(tok.mode, tok.command)
  end
  relax = frozentok'relax'
  hbox  = frozentok'hbox'
  let = frozentok'let'
-- Finally, we clean after ourselvs. If we changed an existing macro, 
-- we restore its meaning, otherwise we undefine the new primitive.
  -- local undef = new_tok(0, table.swapped(token.commands())['undefined_cs'])
  -- OpTeX does not have table.swapped()
  runtoks(function()
      for _,csname in ipairs({'relax', 'hbox', 'let'}) do
          put_next(let, create(prefix .. csname), undef)
      end
  end)
end

-- \secc User Interface^^M
-- This section describe the definition of
-- the one macro exposed to the end user.
-- The code is probably longer than it
-- should be, but with this method we can
-- create a format agnostic key-value interface.
-- The idea is based on an article by Hans Hagen. 
local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_string = token.scan_string
local scan_list = token.scan_list
local scan_int = token.scan_int
local scan_keyword = token.scan_keyword
local unpack = table.unpack
-- A helper function for choice keys.
local function scan_choice(...)
    local args = {...}
    for _, keyword in ipairs(args) do
        if scan_keyword(keyword) then
            return keyword
        end
    end
end


local lineno_types = { }
local lineno_attr = { }
local lineno_vals = {
    toks = {scanner = scan_toks, default = { }},
    start = {scanner = scan_toks, default = { }},
    ['end'] = {scanner = scan_toks, default = { }},
    box = {scanner = scan_choice, args = {'true', 'false', 'inline'}, default = 'inline'},
    alignment = {scanner = scan_choice, args = {'true', 'false', 'once'}, default = 'true'},
    equation = {scanner = scan_choice, args = {'true', 'false', 'once'}, default = 'true'},
    line = {scanner = scan_choice, args = {'true', 'false'}, default = 'true'},
    offset = {scanner = scan_choice, args = {'true', 'false'}, default = 'true'}
}

local uni_char = utf8.char

local function check_delimiter()
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror("lualineno: wrong syntax in \\lualineno",
                {"There's a '" .. (tok.csname or uni_char(tok.mode)) .. "' out of place." })
        put_next(tok)
    end
end

local function process_keys(toks, tbl)
    put_next(relax)
    put_next(toks)
    local matched, vals = true, { }
    while matched do
	    matched = false
        for key, param in pairs(tbl) do
		    if scan_keyword(key) then
			    matched = true
			    scan_keyword('=')
				local args = param.args or {}
				vals[key] = param.scanner(unpack(args))
				break
			end
		end
    end
	check_delimiter()
	return vals
end

local function set_defaults()
    local vals = process_keys(scan_toks(), lineno_vals)
	for k,v in pairs(lineno_vals) do
	    v.default = vals[k]
	end
end

local function define_lineno()
-- This function is used in the define key.
-- It is very similar to the set_defaults() function,
-- but it accepts a `column` and `name` keys as well.
    local define_vals = {}
    for k,v in pairs(lineno_vals) do
        define_vals[k] = v
    end
	define_vals.column = {scanner = scan_int}
    define_vals.name = {scanner = scan_string}
    local vals = process_keys(scan_toks(), define_vals)
-- A newly defined lualineno type must have a name
    local name = vals['name']
    if not name then 
        texerror("lualineno: missing name in \\lualineno")
    end
-- If the `column` key is not specified we assume 
-- the definition is for the first (or only) column.
    local col = vals['column']
    col = col or 1
-- We keep a map between the lineno type names and attributes.
    lineno_attr[name] = lineno_attr[name] or #lineno_types + 1
	local i = lineno_attr[name]
    lineno_types[i] = lineno_types[i] or { }
-- Make sure the table for the column of this lineno type exists
    lineno_types[i][col] = lineno_types[i][col] or { }
-- Populate the column's table.
-- If a key was not specified we use the defualt value.
    local c = lineno_types[i][col]
	for k,v in pairs(lineno_vals) do 
        c[k] = vals[k] or v.default
    end
end

local setattribute = tex.setattribute
local type_attr = luatexbase and luatexbase.new_attribute('lualineno_type') or 0
local col_attr = luatexbase and luatexbase.new_attribute('lualineno_col') or 1
local unset_attr = -0x7FFFFFFF

local alg_bool = true
local hlist_id = node.id('hlist')
local vlist_id = node.id('vlist')
local glyph_id = node.id('glyph')
local hlist_subs = node.subtypes("hlist")
local tail = node.tail


local function mark_last_vlist(n)
    local current = n
    while current do
        if current.id == vlist_id then
            node.set_attribute(current, type_attr, -1)
            return true
        elseif current.id == hlist_id then
            if mark_last_vlist(tail(current.list)) then return true end
        end
        current = current.prev
    end
    return false
end

local get_props = node.getproperty
local set_props = node.setproperty

local make_labels
local function label_last_glyph(m, tokens)
    if optex then
        luatexbase.add_to_callback('lualineno.pre_add_numbers_filter', make_labels, 'lualineno.labels')
    elseif latex then
        luatexbase.add_to_callback('lualineno.post_add_numbers_filter', make_labels, 'lualineno.labels')
    end
    label_last_glyph = function(n, toks)
        local current = n
        while current do
            if current.id == glyph_id then
                local props = get_props(current)
                if not props then
                    props = { }
                    set_props(current,props)
                end
                props.lualineno = toks
                return true
            elseif current.list then
                if label_last_glyph(tail(current.list), toks) then 
                    return true 
                end
            end
            current = current.prev
        end
        return false
    end
    label_last_glyph(m, tokens)
end

local texnest = tex.nest
local number_lines_human
local function lualineno()
    local saved_endlinechar = tex.endlinechar
    tex.endlinechar = 32
    local toks = scan_toks()
    tex.endlinechar = saved_endlinechar
    put_next(relax)
    put_next(toks)

    while true do
        if scan_keyword('set') then
            scan_keyword('=')
            local attr = lineno_attr[scan_string()]
            setattribute(type_attr, attr and attr or unset_attr)
        elseif scan_keyword('define') then
            scan_keyword('=')
            define_lineno()
        elseif scan_keyword('defaults') then
            scan_keyword('=')
            set_defaults()
        elseif scan_keyword('algorithm') then
            scan_keyword('=')
            if scan_keyword('human') then
                alg_bool = true
            elseif scan_keyword('tex') then
                alg_bool = false
            end
        elseif scan_keyword('anchor') then
            for i=texnest.ptr,0,-1 do 
                if mark_last_vlist(texnest[i].tail) then return end
            end
        elseif scan_keyword('label') then
            scan_keyword('=')
            local toks = scan_toks(false, true)
            for i=texnest.ptr,0,-1 do 
                if label_last_glyph(texnest[i].tail, toks) then return end
            end
        elseif scan_keyword('line_attr') then
            scan_keyword('=')
            type_attr = scan_int()
        elseif scan_keyword('col_attr') then
            scan_keyword('=')
            col_attr = scan_int()
        elseif scan_keyword('processbox') then
            scan_keyword('=')
            local box = tex.box[scan_int()]
            number_lines_human(box.head, box, 1, 0, false)
            node.set_attribute(box, col_attr, -2)
        else
            break
        end
    end
    check_delimiter()    
end

do
  if token.is_defined('lualineno') then
      texio.write_nl('log', "lualineno: redefining \\lualineno")
  end
  local function_table = lua.get_functions_table()
  local luafnalloc = luatexbase and luatexbase.new_luafunction and luatexbase.new_luafunction('lualineno') or #function_table + 1
  token.set_lua('lualineno', luafnalloc)
  function_table[luafnalloc] = lualineno
end

-- \secc a^^M

local get_attribute = node.get_attribute
local insert_before = node.insert_before
local insert_after = node.insert_after
local traverse = node.traverse

local function add_boxes_to_line(n, parent, line_type, offset)
-- In case \LaTeX/ is used without the luacolor package,
-- we add an additional group to make the boxes color safe.
    put_next(rbrace, rbrace)
    put_next(line_type['start'])
    put_next(hbox, lbrace, lbrace)
    local start_box = scan_list()
    
    put_next(rbrace, rbrace)
    put_next(line_type['end'])
    put_next(hbox, lbrace, lbrace)
    local end_box = scan_list()
    
    local start_kern = node.new('kern')
    local shift_kern = node.new('kern')
    local end_kern = node.new('kern')
         
    start_kern.kern = -start_box.width - n.shift - offset
    shift_kern.kern = n.shift + offset
    end_kern.kern = parent.width - n.shift - n.width
		    
    n.head = insert_before(n.list,n.head,shift_kern)
    n.head = insert_before(n.list,n.head,start_box)
    n.head = insert_before(n.list,n.head,start_kern)
    if n.subtype ~= 1 then
        n.head = insert_after(n.list,tail(n.head),end_kern)
    end
    n.head = insert_after(n.list,tail(n.head),end_box)
    return n.head
end

local call_lineno_callbacks
if luatexbase then
    luatexbase.create_callback('lualineno.pre_add_numbers_filter', 'list', false)
    luatexbase.create_callback('lualineno.add_numbers', 'exclusive', add_boxes_to_line)
    luatexbase.create_callback('lualineno.post_add_numbers_filter', 'reverselist', false)
    luatexbase.add_to_callback('lualineno.pre_add_numbers_filter', function(n, parent, line_type, offset)
        runtoks(function() put_next(line_type['toks']) end) 
    return true end, 'lualineno.runtoks')
    local call_callback = luatexbase.call_callback
    call_lineno_callbacks = function(head, parent, line_type, offset)
        local current = call_callback('lualineno.pre_add_numbers_filter', head, parent, line_type, offset)
        head = current == true and head or current
        current = call_callback('lualineno.add_numbers', head , parent, line_type, offset)
        head = current == true and head or current
        call_callback('lualineno.post_add_numbers_filter', head , parent, line_type, offset)
    end
else
    call_lineno_callbacks = function(head, parent, line_type, offset)
        runtoks(function() put_next(line_type['toks']) end)
        add_boxes_to_line(head, parent, line_type, offset)
    end
end

local function number_lines_tex(parent, list, column)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        local line_attr = n.head and get_attribute(tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr][column]
        if n.id == hlist_id and line_type and
               (line_type[hlist_subs[n.subtype]] == 'true' or n.subtype == 0) then
            call_lineno_callbacks(n, parent, line_type, 0)
        elseif n.list then
            number_lines_tex(n, n.list, column)
        end
    end
end

-- Not every object that would be considered a line from \LuaTeX's point of view
-- would be considered a line from a human perspective. For example, a line
-- containing only an indent box, or an alignment containing only rules,
-- so we search for a glyph node recursively.

local function real_box(list)
    for n in traverse(list) do
        if n.id == hlist_id and n.subtype ~= 7 and real_box(n.list) then
            return true
        elseif n.id == vlist_id and n.subtype ~= 11 and real_box(n.list) then
            return true
        elseif n.id == glyph_id then
            return true
        end
    end
    return false
end

local function real_line(list, parent, offset)
    for n in traverse(list) do
        if n.id == glyph_id then
            return true
        elseif n.id == vlist_id and n.subtype ~= 11 and real_box(n.list) then
            return n, offset + node.rangedimensions(parent, list, n)
        elseif n.id == hlist_id and n.subtype ~= 7 and real_box(n.list) then
           local new_offset = offset + node.rangedimensions(parent, list, n)
           return real_line(n.list, n, new_offset)
        end
    end
    return false
end

number_lines_human = function(parent, list, column, offset, inline)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        local line_attr = n.head and get_attribute(tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr][column]
        if n.id == hlist_id and line_type and
               (line_type[hlist_subs[n.subtype]] == 'true' or
               (inline and line_type[hlist_subs[n.subtype]] == 'inline') or
               n.subtype == 0) then
            local m, new_offset = real_line(n.head, n, offset)
            if new_offset then
                new_offset = new_offset  + n.shift
                number_lines_human(m, m.head, column, get_attribute(m, type_attr) == -1 and 0 or new_offset, true)
            elseif m then
                call_lineno_callbacks(n, parent, line_type, line_type['offset'] == 'true' and offset or 0)
            end
        elseif n.id == hlist_id and line_type and line_type[hlist_subs[n.subtype]] == 'once' and real_box(n.list) then
            call_lineno_callbacks(n, parent, line_type, line_type['offset']  == 'true' and offset or 0)
        elseif n.list then
            number_lines_human(n, n.list, column, get_attribute(n, type_attr) == -1 and 0 or offset, inline)
        end
    end
end

if not plain then
    luatexbase.add_to_callback('pre_shipout_filter', function(head)
        if alg_bool then
            number_lines_human(head, head.list, 1, 0, false)
        else
            number_lines_tex(head, head.list, 1, 0)
        end
        return true
    end, 'lualineno.shipout')
end

-- \secc Format Specific Code^^M

if format == 'optex' then
-- To be able to use \OpTeX/'s color mechnism in line numbers the colorizing
-- needs to happen after the line numbers are added, so we remove and insert
-- back again the colorizing function from the `pre_shipout_filter` callback.
    local colorize = callback.remove_from_callback('pre_shipout_filter', '_colors')
    callback.add_to_callback('pre_shipout_filter', colorize, '_colors')
-- This is the patch of `\beginmulti` in order mark the columns boxes.
-- For each box we assign an attribute with a value 
-- according to the column number.
    local replace = [[
\_directlua{
    local column = tex.splitbox(6, tex.dimen[1], 'exactly')
    local num = tex.count['_tmpnum']
    local attr = luatexbase.attributes['lualineno_col']
    node.set_attribute(column, attr, num)
    node.write(column)
}]]
    local find = [[\_vsplit 6 to\_dimen 1 ]]
    local patched_multi, success = token.get_macro("_createcolumns"):gsub(find, replace)
-- Log the success or failure of the patch
    if success then
        texio.write_nl('log', "lualineno: patching \\_createcolumns")
        token.set_macro("_createcolumns", patched_multi)
    else
        texio.write_nl('log', "lualineno: failed to patch \\_createcolumns")
    end

    local lbracket, rbracket = new_tok(string.byte('['), 12), new_tok(string.byte(']'), 12)
    local label_tok = create('_label')
    make_labels = function(list)
        for n in traverse(list) do
            if n.id == glyph_id then
                local props = get_props(n)
                if props then
                    local label = props.lualineno
                    if label then 
                        runtoks(function()
                            put_next(rbracket)
                            put_next(label)
                            put_next(label_tok,lbracket)
                        end)
                    end 
                end
            elseif n.list then 
                make_labels(n.list)
            end
        end
        return true
    end
elseif latex then
-- Here we mark the columns accroding to `\if@firstcolumn`
    local true_tok = create('iftrue')
    luatexbase.add_to_callback('pre_output_filter', function()
        if create('if@firstcolumn').mode == true_tok.mode then
           setattribute(col_attr, 1)
        else
           setattribute(col_attr, 2)
        end
        return true
    end, 'lualineno.mark_columns')
    luatexbase.add_to_callback('buildpage_filter', function(info)
        if info == 'after_output' then
            setattribute(col_attr, unset_attr)
        end 
    end, 'lualineno.mark_columns')
-- If the luacolor package is loaded,
-- colorizing must happen after line numbers
-- are added to be able to color them.
    luatexbase.declare_callback_rule('pre_shipout_filter', 
         'lualineno.shipout', 'before', 'luacolor.process')
    
    local label_tok = create('label')
    local node_copy, node_flush = node.copy, node.flush_node
    make_labels = function(list)
        for n in traverse(list) do
            if n.id == glyph_id then
                local props = get_props(n)
                if props then
                    local label = props.lualineno
                    if label then 
                        runtoks(function()
                            put_next(rbrace,rbrace)
                            put_next(label)
                            put_next(hbox, lbrace, label_tok,lbrace)
                            local label_node = scan_list()
                            list = insert_after(list,n,node_copy(label_node.head))
                            node_flush(label_node)
                        end)
                    end 
                end
            elseif n.list then 
                make_labels(n.list)
            end
        end
        return true
    end
end
