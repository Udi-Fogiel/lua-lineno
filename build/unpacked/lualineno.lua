local format_name = tex.formatname
local texerror = tex.error

-- TODO: more robust format testing - lualatex-dev etc...
if format_name ~= 'optex' and format_name ~= 'lualatex' then
    texerror("lualineno: The format " .. format_name .. " is not supported", 
             {"Use optex or lualatex."})
end

local runtoks = tex.runtoks
local put_next = token.put_next
local create = token.create
local new_tok = token.new

local lbrace, rbrace = new_tok(0x7B, 1), new_tok(0x7D, 2)

local lineno_primitives = {'relax', 'endlocalcontrol', 'hbox', 'let'}
local saved_toks = { }
for _, csname in ipairs(lineno_primitives) do
    if token.is_defined('lualineno_' .. csname) then
        local tok = create('lualineno_' .. csname)
        saved_toks[csname] = new_tok(tok.mod, tok.command)
    end
end 
tex.enableprimitives('lualineno_',lineno_primitives)
local function frozentok(name)
    local tok = create('lualineno_' .. name)
    return new_tok(tok.mode, tok.command)
end

local relax = frozentok'relax'
local endlocalcontrol = frozentok'endlocalcontrol'
local hbox  = frozentok'hbox'
local let = frozentok'let'
local undef = new_tok(0, 133)

runtoks(function()
    for _,csname in ipairs(lineno_primitives) do
        put_next(let, create('lualineno_' .. csname),
            saved_toks[csname] or undef)
    end
end)
saved_toks = nil

local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_string = token.scan_string
local scan_list = token.scan_list
local scan_int = token.scan_int
local scan_keyword = token.scan_keyword

local lineno_types = { }
local lineno_attr = { }

local function define_lineno()
    local saved_endlinechar = tex.endlinechar
    tex.endlinechar = 32
    local toks = scan_toks()
    tex.endlinechar = saved_endlinechar
    put_next(relax)
    put_next(toks)
    
    local column, name, preamble, left_box, right_box
    while true do
        if scan_keyword('column') then
            scan_keyword('=')
            column = scan_int()
            scan_keyword(',')
        elseif scan_keyword('name') then
            scan_keyword('=')
            name = scan_string()
            scan_keyword(',')
        elseif scan_keyword('preamble') then
            scan_keyword('=')
            preamble = scan_toks()
            scan_keyword(',')
        elseif scan_keyword('left') then
            scan_keyword('=')
            left_box = scan_toks()
            scan_keyword(',')
        elseif scan_keyword('right') then
            scan_keyword('=')
            right_box = scan_toks()
            scan_keyword(',')
        else
            break
        end
    end
    
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror("lualineno: wrong syntax in \\deflineno",
                {"There's a '" .. (tok.csname or utf8.char(tok.mode)) .. "' out of place." })
        put_next(tok)
        
    end
    if not name then 
        texerror("lualineno: missing name in \\deflineno")
    end
    
    column = column or 1
    preamble = preamble or { }
    left_box = left_box or { }
    right_box = right_box or { }
        
    local t = { }
    t[#t+1] = hbox t[#t+1] = lbrace t[#t+1] = lbrace
    for i = 1, #left_box do
        t[#t+1] = left_box[i]
    end
    t[#t+1] = rbrace t[#t+1] = rbrace
    t[#t+1] = hbox t[#t+1] = lbrace t[#t+1] = lbrace
    for i = 1, #right_box do
        t[#t+1] = right_box[i]
    end
    t[#t+1] = rbrace t[#t+1] = rbrace
    preamble[#preamble+1] = endlocalcontrol
    lineno_attr[name] = lineno_attr[name] or #lineno_types + 1
    lineno_types[lineno_attr[name]] = lineno_types[lineno_attr[name]] or { }
    lineno_types[lineno_attr[name]][column] = 
      { ['boxes'] =  t, 
        ['preamble'] = preamble }
end


local setattribute = tex.setattribute
local type_attr = luatexbase.new_attribute('lualineno_type')
local col_attr = luatexbase.new_attribute('lualineno_col')

local function set_lineno()
    local attr = lineno_attr[scan_string()]
    if attr then
        setattribute(type_attr, attr)
    else
        setattribute(type_attr,-0x7FFFFFFF)
    end
end

local function_table = lua.get_functions_table()
local define_lua_call = function(csname, fn, ...)
    if token.is_defined(csname) then
        texio.write_nl('log', "lualineno: redefining \\" .. csname)
    end
    local luafnalloc
    if format_name == 'lualatex' then
        luafnalloc = luatexbase.new_luafunction('lualineno.' .. csname)
    else
        luafnalloc = #function_table + 1
    end
    token.set_lua(csname, luafnalloc, ...)
    function_table[luafnalloc] = fn
end

define_lua_call('deflineno', define_lineno)
define_lua_call('setlineno', set_lineno)

local get_attribute = node.get_attribute
local copy_list = node.copy_list
local insert_before = node.insert_before
local insert_after = node.insert_after
local traverse = node.traverse

local function add_line_boxes(n, parent, line_type, offset)
    put_next(line_type['preamble'])
    runtoks(get_next)
    put_next(line_type['boxes'])
    local left_box = scan_list()
    local right_box = scan_list()
    
    local left_kern = node.new('kern')
    local shift_kern = node.new('kern')
    local right_kern = node.new('kern')
         
    left_kern.kern = -left_box.width - n.shift - offset
    shift_kern.kern = n.shift + offset
    right_kern.kern = parent.width - n.shift - n.width
		    
    n.head = insert_before(n.list,n.head,shift_kern)
    n.head = insert_before(n.list,n.head,left_box)
    n.head = insert_before(n.list,n.head,left_kern)
		    
    if n.subtype ~= 1 then
        n.head = insert_after(n.list,node.tail(n.head),right_kern)
    end
    n.head = insert_after(n.list,node.tail(n.head),right_box)
end

local function inner_expand_write(n)
    n.data = n.data
end

local function expand_write(list)
    for n in traverse(list) do
        if n.id == 8 and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            expand_write(n.list)
        end
    end
end


local function recurse(parent, list, column)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        if n.id == 0 and (n.subtype == 1 or n.subtype == 6 or n.subtype == 4) then
            local line_attr = get_attribute(node.tail(n.head), type_attr)
            local line_type = line_attr and lineno_types[line_attr][column] or nil
            if line_type then
                add_line_boxes(n, parent, line_type, 0)
            end
            expand_write(n)
        elseif n.id == 8 and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            recurse(n, n.list, column)
        end
    end
end

local function real_box(list)
    for n in traverse(list) do
        if n.id == 0 and real_box(n.list) then
            return true
        elseif (n.id == 1 and real_box(n.list)) then
            return true
        elseif n.id == 29 then
            return true
        end
    end
    return false
end

local function real_line(list, offset)
    for n in traverse(list) do
        if n.id == 29 then
            return true
        elseif n.id == 1 and real_box(n.list) then
            return n, offset + node.dimensions(list, n)
        elseif n.id == 0 and real_box(n.list) then
           local new_offset = offset + node.dimensions(list, n)
           return real_line(n.list, new_offset)
        end
    end
    return false
end

local function recurse_smart(parent, list, column, offset, inline)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        if n.id == 0 and (n.subtype == 1 or n.subtype == 6 or n.subtype == 4 or (n.subtype == 2 and inline)) then
            local line_attr = get_attribute(node.tail(n.head), type_attr)
            local line_type = line_attr and lineno_types[line_attr][column] or nil 
            if line_type then
                
                local m, new_offset = real_line(n.head, offset)
                if new_offset then
                    new_offset = new_offset  + n.shift
                    recurse_smart(m, m.head, column, new_offset, true)
                elseif m then
                    add_line_boxes(n, parent, line_type, offset)
                end
            end
            expand_write(n)
        elseif n.id == 8 and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            recurse_smart(n, n.list, column, offset, inline)
        end
    end
end

local function lineno_pre_shipout(head)
    recurse_smart(head, head.list, 1, 0, false)
    return true
end

luatexbase.add_to_callback('pre_shipout_filter', lineno_pre_shipout, 'lualineno.add_boxes')

if format_name == 'lualatex' then
    function inner_expand_write(n)
        runtoks(function()
            put_next(let,create('protect'),create('noexpand'))
        end)
        n.data = n.data
        runtoks(function()
            put_next(let,create('protect'),create('relax'))
        end)
    end
    luatexbase.add_to_callback('pre_output_filter', function()
        if create('if@firstcolumn').mode == create('iftrue').mode then
           setattribute(col_attr, 1)
        else
           setattribute(col_attr, 2)
        end
        return true
    end, 'lualineno.mark_columns')
    luatexbase.add_to_callback('buildpage_filter', function(info)
        if info == 'after_output' then
            setattribute(col_attr, -0x7FFFFFFF)
        end 
    end, 'lualineno.mark_columns')
    if luatexbase.in_callback('pre_shipout_filter', 'luacolor.process') then
        luatexbase.declare_callback_rule('pre_shipout_filter', 
                 'lualineno.add_boxes', 'before', 'luacolor.process')
    end
end

if format_name == 'optex' then
    local colorize = callback.remove_from_callback('pre_shipout_filter', '_colors')
    callback.add_to_callback('pre_shipout_filter', colorize, '_colors')
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
    token.set_macro("_createcolumns", patched_multi)
end
