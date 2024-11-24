local format = tex.formatname
local texerror = tex.error

if not (format == 'optex' or format:find('lualatex')) then
    error("lualineno: The format " .. format .. " is not supported\n\n" ..
             "Use OpTeX or LuaLaTeX.")
end

------------------------------------------
------------------------------------------

local runtoks = tex.runtoks
local put_next = token.put_next
local create = token.create
local new_tok = token.new

local lbrace, rbrace = new_tok(string.byte('{'), 1), new_tok(string.byte('}'), 2)
local relax, endlocalcontrol, hbox, let, undef
do
  local lineno_primitives = {'relax', 'endlocalcontrol', 'hbox', 'let'}
  local prefix = 'lualineno_'
  local saved_toks = { }
  for _, csname in ipairs(lineno_primitives) do
      if token.is_defined(prefix .. csname) then
          local tok = create(prefix .. csname)
          saved_toks[csname] = new_tok(tok.mod, tok.command)
      end
  end 
  tex.enableprimitives(prefix,lineno_primitives)
  local function frozentok(name)
      local tok = create(prefix .. name)
      return new_tok(tok.mode, tok.command)
  end

  relax = frozentok'relax'
  endlocalcontrol = frozentok'endlocalcontrol'
  hbox  = frozentok'hbox'
  let = frozentok'let'
  undef = new_tok(0, 133)

  runtoks(function()
      for _,csname in ipairs(lineno_primitives) do
          put_next(let, create(prefix .. csname),
              saved_toks[csname] or undef)
      end
  end)
end

------------------------------------------
------------------------------------------

local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_string = token.scan_string
local scan_list = token.scan_list
local scan_int = token.scan_int
local scan_keyword = token.scan_keyword

local lineno_types = { }
local lineno_attr = { }
local lineno_defaults = {
    ['preamble'] = { },
    ['left'] = { },
    ['right'] = { },
    ['box'] = 'inline',
    ['alignment'] = 'true',
    ['equation'] = 'true',
    ['line'] = 'true',
    ['offset'] = 'true'
}

local function scan_bool(name)
    scan_keyword('=')
    if scan_keyword('true') then
        return 'true'
    elseif scan_keyword('false') then
        return 'false'
    end
end

local function set_defaults()  
    local toks = scan_toks()
    put_next(relax)
    put_next(toks)
    
    while true do
        if scan_keyword('preamble') then
            scan_keyword('=')
            lineno_defaults['preamble'] = scan_toks()
        elseif scan_keyword('left') then
            scan_keyword('=')
            lineno_defaults['left'] = scan_toks()
        elseif scan_keyword('right') then
            scan_keyword('=')
            lineno_defaults['right'] = scan_toks()
        elseif scan_keyword('box') then
            scan_keyword('=')
            if scan_keyword('true') then
                lineno_defaults['box'] = 'true'
            elseif scan_keyword('false') then
                lineno_defaults['box'] = 'false'
            elseif scan_keyword('inline') then
                lineno_defaults['box'] = 'inline'
            end
        elseif scan_keyword('alignment') then
            scan_keyword('=')
            if scan_keyword('true') then
                lineno_defaults['alignment'] = 'true'
            elseif scan_keyword('false') then
                lineno_defaults['alignment'] = 'false'
            elseif scan_keyword('once') then
                lineno_defaults['alignment'] = 'once'
            end
        elseif scan_keyword('equation') then
            lineno_defaults['equation'] = scan_bool('equation')
        elseif scan_keyword('line') then
            lineno_defaults['line'] = scan_bool('line')
        elseif scan_keyword('offset') then
            lineno_defaults['offset'] = scan_bool('offset')
        else
            break
        end
    end
    
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror("lualineno: wrong syntax in \\lualineno",
                {"There's a '" .. (tok.csname or utf8.char(tok.mode)) .. "' out of place." })
        put_next(tok)
    end
    
end

local function define_lineno()
    local toks = scan_toks()
    put_next(relax)
    put_next(toks)
    
    local column, name, preamble, left_box, right_box
    local box, align, equation, line, offset
    while true do
        if scan_keyword('column') then
            scan_keyword('=')
            column = scan_int()
        elseif scan_keyword('name') then
            scan_keyword('=')
            name = scan_string()
        elseif scan_keyword('preamble') then
            scan_keyword('=')
            preamble = scan_toks()
        elseif scan_keyword('left') then
            scan_keyword('=')
            left_box = scan_toks()
        elseif scan_keyword('right') then
            scan_keyword('=')
            right_box = scan_toks()
        elseif scan_keyword('box') then
               scan_keyword('=')
            if scan_keyword('true') then
                box = 'true'
            elseif scan_keyword('false') then
                box = 'false'
            elseif scan_keyword('inline') then
                box = 'inline'
            end
        elseif scan_keyword('alignment') then
            scan_keyword('=')
            if scan_keyword('true') then
                align = 'true'
            elseif scan_keyword('false') then
                align = 'false'
            elseif scan_keyword('once') then
                align = 'once'
            end
        elseif scan_keyword('equation') then
            equation = scan_bool('equation')
        elseif scan_keyword('line') then
            line = scan_bool('line')
        elseif scan_keyword('offset') then
            offset = scan_bool('offset')
        else
            break
        end
    end
    
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror("lualineno: wrong syntax in \\lualineno",
                {"There's a '" .. (tok.csname or utf8.char(tok.mode)) .. "' out of place." })
        put_next(tok)
    end
    if not name then 
        texerror("lualineno: missing name in \\lualineno")
    end
    
    column = column or 1
        
    lineno_attr[name] = lineno_attr[name] or #lineno_types + 1
    lineno_types[lineno_attr[name]] = lineno_types[lineno_attr[name]] or { }
    local lineno_type = lineno_types[lineno_attr[name]]

    if not lineno_type[column] then
        lineno_type[column] = { }
        for k,v in pairs(lineno_defaults) do
            lineno_type[column][k] = v
        end
    end
    
    local col = lineno_type[column]
    col['preamble'] = preamble or col['preamble']
    col['left'] = left_box or col['left']
    col['right'] = right_box or col['right']
    col['box'] = box or col['box']
    col['alignment'] = align or col['alignment']
    col['equation'] = equation or col['equation']
    col['line'] = line or col['line']
    col['offset'] = offset or col['offset']
end

local setattribute = tex.setattribute
local type_attr = luatexbase.new_attribute('lualineno_type')
local col_attr = luatexbase.new_attribute('lualineno_col')
local unset_attr = -0x7FFFFFFF

local function set_lineno()
    local attr = lineno_attr[scan_string()]
    if attr then
        setattribute(type_attr, attr)
    else
        setattribute(type_attr,unset_attr)
    end
end

local alg_bool = true

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
            set_lineno()
        elseif scan_keyword('define') then
            scan_keyword('=')
            define_lineno()
        elseif scan_keyword('defaults') then
            scan_keyword('=')
            set_default()
        elseif scan_keyword('algorithm') then
            scan_keyword('=')
            if scan_keyword('human') then
                alg_bool = true
            elseif scan_keyword('tex') then
                alg_bool = false
            end
        else
            break
        end
    end
    
    local tok = get_next()
    if tok.tok ~= relax.tok then
        texerror("lualineno: wrong syntax in \\lualineno",
                {"There's a '" .. (tok.csname or utf8.char(tok.mode)) .. "' out of place." })
        put_next(tok)
    end
    
end

do
  if token.is_defined('lualineno') then
      texio.write_nl('log', "lualineno: redefining \\lualineno")
  end
  local function_table = lua.get_functions_table()
  local luafnalloc
  if format == 'lualatex' then
      luafnalloc = luatexbase.new_luafunction('lualineno')
  else
      luafnalloc = #function_table + 1
  end
  token.set_lua('lualineno', luafnalloc)
  function_table[luafnalloc] = lualineno
end

------------------------------------------
------------------------------------------

local get_attribute = node.get_attribute
local copy_list = node.copy_list
local insert_before = node.insert_before
local insert_after = node.insert_after
local traverse = node.traverse

local function add_boxes_to_line(n, parent, line_type, offset)
    put_next(endlocalcontrol)
    put_next(line_type['preamble'])
    runtoks(get_next)
    
    put_next(rbrace, rbrace)
    put_next(line_type['left'])
    put_next(hbox, lbrace, lbrace)
    local left_box = scan_list()
    
    put_next(rbrace, rbrace)
    put_next(line_type['right'])
    put_next(hbox, lbrace, lbrace)
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

local hlist_id = node.id('hlist')
local vlist_id = node.id('vlist')
local glyph_id = node.id('glyph')
local whatsit_id = node.id('whatsit')

local function expand_write(list)
    for n in traverse(list) do
        if n.id == whatsit_id and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            expand_write(n.list)
        end
    end
end

local hlist_subs = node.subtypes("hlist")

local function number_lines_tex(parent, list, column)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        local line_attr = n.head and get_attribute(node.tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr][column]
        if n.id == hlist_id and line_type and
               (line_type[hlist_subs[n.subtype]] == 'true' or n.subtype == 0) then
            add_boxes_to_line(n, parent, line_type, 0)
            expand_write(n)
        elseif n.id == whatsit_id and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            number_lines_tex(n, n.list, column)
        end
    end
end

local function real_box(list)
    for n in traverse(list) do
        if n.id == hlist_id and real_box(n.list) then
            return true
        elseif (n.id == vlist_id and real_box(n.list)) then
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
        elseif n.id == vlist_id and real_box(n.list) then
            return n, offset + node.rangedimensions(parent, list, n)
        elseif n.id == hlist_id and n.subtype ~= 7 and real_box(n.list) then
           local new_offset = offset + node.rangedimensions(parent, list, n)
           return real_line(n.list, n, new_offset)
        end
    end
    return false
end

local function number_lines_human(parent, list, column, offset, inline)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        local line_attr = n.head and get_attribute(node.tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr][column]
        if n.id == hlist_id and line_type and
               (line_type[hlist_subs[n.subtype]] == 'true' or
               (line_type[hlist_subs[n.subtype]] == 'inline' and inline) or
               n.subtype == 0) then
            local m, new_offset = real_line(n.head, n, offset)
            if new_offset then
                new_offset = new_offset  + n.shift
                number_lines_human(m, m.head, column, new_offset, true)
            elseif m then
                add_boxes_to_line(n, parent, line_type, line_type['offset'] and offset or 0)
            end
            expand_write(n)
        elseif n.id == hlist_id and line_type and line_type[hlist_subs[n.subtype]] == 'once' and real_box(n.list) then
            add_boxes_to_line(n, parent, line_type, line_type['offset'] and offset or 0)
        elseif n.id == whatsit_id and n.subtype == 1 then
            inner_expand_write(n)
        elseif n.list then
            number_lines_human(n, n.list, column, offset, inline)
        end
    end
end

luatexbase.add_to_callback('pre_shipout_filter', function(head)
    if alg_bool then
        number_lines_human(head, head.list, 1, 0, false)
    else
        number_lines_tex(head, head.list, 1, 0)
    end
    return true
end, 'lualineno')

------------------------------------------
------------------------------------------

if format == 'optex' then
    local colorize = callback.remove_from_callback('pre_shipout_filter', '_colors')
    if colorize then
        callback.add_to_callback('pre_shipout_filter', colorize, '_colors')
    end
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
    if success then
        texio.write_nl('log', "lualineno: patching \\_createcolumns")
        token.set_macro("_createcolumns", patched_multi)
    else
        texio.write_nl('log', "lualineno: failed to patch \\_createcolumns")
    end
else
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
            setattribute(col_attr, unset_attr)
        end 
    end, 'lualineno.mark_columns')
    if luatexbase.in_callback('pre_shipout_filter', 'luacolor.process') then
        luatexbase.declare_callback_rule('pre_shipout_filter', 
                 'lualineno.add_boxes', 'before', 'luacolor.process')
    end
end
