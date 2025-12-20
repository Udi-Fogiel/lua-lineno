-- lualineno version = 0.1, 2025-11-28

-- \secc Initialization^^M
-- Currently the module works only with 
-- Lua\LaTeX/ and \OpTeX.

local format = tex.formatname
local texerror = tex.error

local runtoks = tex.runtoks
local put_next = token.unchecked_put_next
local create = token.create
local new_tok = token.new
local lbrace = new_tok(string.byte('{'), token.command_id'left_brace')
local rbrace = new_tok(string.byte('}'), token.command_id'right_brace')

local get_next = token.get_next
local scan_toks = token.scan_toks
local scan_string = token.scan_string
local scan_list = token.scan_list
local scan_int = token.scan_int
local scan_keyword = token.scan_keyword

local keyval = require('luakeyval')
local scan_choice = keyval.choices
local scan_bool = keyval.bool
local process_keys = keyval.process
local messages = {
    error1 = "lualineno: Wrong syntax in \\lualineno",
    value_forbidden = 'lualineno: The key "%s" does not accept a value',
    value_required = 'lualineno: The key "%s" requires a value',
}

local setattribute = tex.setattribute
local type_attr = luatexbase and luatexbase.new_attribute('lualineno_type') or 0
local col_attr = luatexbase and luatexbase.new_attribute('lualineno_col') or 1
local mark_attr = luatexbase and luatexbase.new_attribute('lualineno_mark') or 2

local unset_attr = -0x7FFFFFFF

local hlist_id = node.id('hlist')
local vlist_id = node.id('vlist')
local glyph_id = node.id('glyph')
local tail = node.tail

local get_props = node.getproperty
local set_props = node.setproperty
local get_attribute = node.get_attribute
local set_attribute = node.set_attribute
local node_flush = node.flush_node
local insert_before = node.insert_before
local insert_after = node.insert_after
local traverse = node.traverse
local rangedimensions = node.rangedimensions
local node_copy = node.copy
local texnest = tex.nest

local LINENO_NUMBER = 0x1
local LINENO_RECURSE = 0x2

local line_sb, eq_sb, align_sb, bo_sb
local mathcahr_sb, eqno_sb
for k,v in pairs(node.subtypes("hlist")) do
    if v == "line" then line_sb = k end
    if v == "alignment" then align_sb = k end
    if v == "box" then box_sb = k end
    if v == "equation" then eq_sb = k end
    if v == "equationnumber" then eqno_sb = k end
    if v == "mathchar" then mathcahr_sb = k end
end
local vex_sb, vdel_sb
for k,v in pairs(node.subtypes("vlist")) do
    if v == "vextensible" then vex_sb = k end
    if v == "vdelimiter" then vdel_sb = k end
end

local base_kern = node.new('kern', 'user')

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

  -- local hbox = new_tok(141, 21)
local hbox do
-- initialization of the new primitives.
  local prefix = '@lua^line&no_' -- unlikely prefix...
  while token.is_defined(prefix .. 'hbox') do
    prefix = prefix .. '@lua^line&no_'
  end
  tex.enableprimitives(prefix,{'hbox'})
-- Now we create new tokens with the meaning of
-- the primitives.
  local tok = create(prefix .. 'hbox')
  hbox  = new_tok(tok.mode, tok.command)
end

-- \secc User Interface^^M
-- This section describe the definition of
-- the one macro exposed to the end user.
-- The code is probably longer than it
-- should be, but with this method we can
-- create a format agnostic key-value interface.
-- The idea is based on an article by Hans Hagen. 
local lineno_types = { }
local lineno_attr = { }
local defaults = {
    toks = { },
    left = { },
    right = { },
    box = {number = true, recurse = true},
    alignment = {number = true, recurse = true},
    equation = {number = true, recurse = true},
    line = {number = true, recurse = true},
    offset = {number = true, recurse = true},
}

local inner_keys = {
    number = {scanner = scan_bool, default = true},
    recurse = {scanner = scan_bool, default = true}
}

local defaults_keys = {
    toks = {scanner = scan_toks},
    left = {scanner = scan_toks},
    right = {scanner = scan_toks},
    box = {scanner = process_keys, args = {inner_keys, messages}},
    alignment = {scanner = process_keys, args = {inner_keys, messages}},
    equation = {scanner = process_keys, args = {inner_keys, messages}},
    line = {scanner = process_keys, args = {inner_keys, messages}},
    offset = {scanner = scan_bool}
}

local function set_defaults()
    local vals = process_keys(defaults_keys,messages)
    for k,v in pairs(vals) do
        defaults[k] = v
    end
end

local define_keys = { }
for k,v in pairs(defaults_keys) do
    define_keys[k] = v
end
define_keys.column = {scanner = scan_int}
define_keys.name = {scanner = scan_string}

local function define_lineno()
    local vals = process_keys(define_keys, messages)
    local name = vals['name']
    if not name then 
        texerror("lualineno: Missing name when defining a lineno")
        return
    end
    
    local col = vals['column'] or 1
    lineno_attr[name] = lineno_attr[name] or #lineno_types + 1
    local i = lineno_attr[name]
    lineno_types[i] = lineno_types[i] or {}
    lineno_types[i][col] = lineno_types[i][col] or {}
    
    local c = lineno_types[i][col]
    
    -- Convert string keys to numeric keys when storing
    local function store_type(key, subtype_id)
        local setting = vals[key] or defaults[key]
        local flags = 0
        if setting.number then flags = flags | LINENO_NUMBER end
        if setting.recurse then flags = flags | LINENO_RECURSE end
        c[subtype_id] = flags
    end
    
    store_type('box', box_sb)
    store_type('alignment', align_sb)
    store_type('equation', eq_sb)
    store_type('line', line_sb)
    
    -- Other keys remain with string keys
    c.toks = vals.toks or defaults.toks
    c.left = vals.left or defaults.left
    c.right = vals.right or defaults.right
    c.offset = vals.offset ~= nil and vals.offset or defaults.offset
end

local function mark_last_vlist(n)
    local current = n
    while current do
        if current.id == vlist_id then
            set_attribute(current, mark_attr, -1)
            return true
        elseif current.id == hlist_id then
            if mark_last_vlist(tail(current.list)) then return true end
        end
        current = current.prev
    end
    return false
end

local make_label, find_label
find_label = function(list)
    for n in traverse(list) do
        if n.id == glyph_id then
            local props = get_props(n)
            if props then
                local label = props.lualineno
                if label then
                    make_label(label, list, n)
                end 
            end
        elseif n.list then
            find_label(n.list)
        end
    end
    return true
end

local function label_last_glyph(m, tokens)
    if optex then
        luatexbase.add_to_callback('lualineno.pre_numbering', find_label, 'lualineno.labels')
    elseif latex then
        luatexbase.add_to_callback('lualineno.post_numbering', find_label, 'lualineno.labels')
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
    return label_last_glyph(m, tokens)
end

local lualineno_keys = {
    set = {scanner = scan_string},
    unset = { default = true },
    define = {scanner = function() return true end, func = define_lineno},
    defaults = {scanner = function() return true end, func = set_defaults},
    anchor = { default = true },
    label = {scanner = scan_toks, args = {false, true}},
    line_attr = {scanner = scan_int},
    col_attr = {scanner = scan_int},
    mark_attr = {scanner = scan_int},
    processbox = {scanner = scan_int},
}

local find_line
local function lualineno()
    local saved_endlinechar = tex.endlinechar
    tex.endlinechar = 32
    local vals = process_keys(lualineno_keys,messages)
    tex.endlinechar = saved_endlinechar
    if vals.set then
        local attr = lineno_attr[vals.set]
        if attr then
          setattribute(type_attr, attr)
        else
          texerror("lualineno: type '" .. vals.set .. "' undefined")
        end
    end
    if vals.unset then
        setattribute(type_attr, unset_attr)
    end
    if vals.anchor then
        for i=texnest.ptr,0,-1 do 
            if mark_last_vlist(texnest[i].tail) then return end
        end
    end
    if vals.label then
        for i=texnest.ptr,0,-1 do 
            if label_last_glyph(texnest[i].tail, vals.label) then return end
        end
    end
    type_attr = vals.line_attr or type_attr
    col_attr = vals.col_attr or col_attr
    mark_attr = vals.mark_attr or mark_attr
    if vals.processbox then 
        local box = tex.box[vals.processbox]
        local col = get_attribute(box, col_attr)
        find_line(box, box.head, col or 1, 0, box.width)
    end
end

do
  if token.is_defined('lualineno') then
      texio.write_nl('log', "lualineno: redefining \\lualineno")
  end
  local function_table = lua.get_functions_table()
  local luafnalloc = luatexbase and luatexbase.new_luafunction 
    and luatexbase.new_luafunction('lualineno') or #function_table + 1
  token.set_lua('lualineno', luafnalloc, 'protected')
  function_table[luafnalloc] = lualineno
end

-- \secc a^^M

local function number_line(head, line, line_type, offset, width)
   local is_offset = line_type['offset']
   offset = is_offset and offset or 0
-- In case \LaTeX/ is used without the luacolor package,
-- we add an additional group to make the boxes color safe.
    put_next({rbrace, rbrace})
    put_next(line_type.left)
    put_next({hbox, lbrace, lbrace})
    put_next({rbrace, rbrace})
    put_next(line_type.right)
    put_next({hbox, lbrace, lbrace})
    local end_box, start_box
    if line.dir == "TLT" then
        end_box = scan_list()
        start_box = scan_list()
    else
        start_box = scan_list()
        end_box = scan_list()
    end
    if start_box.head then
        if offset ~= 0 then
            local offset_kern = node_copy(base_kern)
            offset_kern.kern = offset
            head = insert_before(head,head,offset_kern)
        end
        head = insert_before(head,head,start_box)
        local start_kern_width = -start_box.width - offset
        if start_kern_width ~= 0 then
            local start_kern = node_copy(base_kern)
            start_kern.kern = start_kern_width
            head = insert_before(head,head,start_kern)
        end
    else
        node_flush(start_box)
    end
    if end_box.head then
        if is_offset then
            local end_kern_width = width - line.width - offset
            if end_kern_width ~= 0 then
                local end_kern = node_copy(base_kern)
                end_kern.kern = end_kern_width
                head = insert_after(head,tail(head),end_kern)
            end
        end
        head = insert_after(head,tail(head),end_box)
    else
        node_flush(end_box)
    end
    return head
end

local lineno_callbacks
if luatexbase then
    luatexbase.create_callback('lualineno.pre_numbering', 'list', false)
    luatexbase.create_callback('lualineno.numbering', 'exclusive', number_line)
    luatexbase.create_callback('lualineno.post_numbering', 'reverselist', false)
    luatexbase.add_to_callback('lualineno.pre_numbering', function(_, _, lt)
        runtoks(function() put_next(lt['toks']) end) 
    return true end, 'lualineno.runtoks')
    local call_callback = luatexbase.call_callback
    lineno_callbacks = function(head, line, line_type, offset, width)
        head = call_callback('lualineno.pre_numbering', head, line, line_type, offset, width)
        if not head then return end
        line.head = head
        head = call_callback('lualineno.numbering', head, line, line_type, offset, width)
        if not head then return end
        line.head = head
        head = call_callback('lualineno.post_numbering', head, line, line_type, offset, width)
        if not head then return end
        line.head = head
    end
else
    lineno_callbacks = function(head, line, line_type, offset, width)
        runtoks(function() put_next(line_type['toks']) end)
        line.head = number_line(head, line, line_type, offset, width)
    end
end

-- Not every object that would be considered a line from \LuaTeX's point of view
-- would be considered a line from a human perspective. For example, a line
-- containing only an indent box, or an alignment containing only rules,
-- so we search for a glyph node recursively.

local function real_box(list)
    for n, id, sb in traverse(list) do
        if id == glyph_id then
            return true
        elseif (id == hlist_id and sb ~= eqno_sb and sb ~= mathcahr_sb) 
          or (id == vlist_id and sb ~= vex_sb and sb ~= vdel_sb) then
            if real_box(n.list) then
                return true
            end
        end
    end
    return false
end

local function real_line(list, parent, offset)
    for n, id, sb in traverse(list) do
        if id == glyph_id then
            return true
        elseif id == vlist_id and sb ~= vex_sb and sb ~= vdel_sb and real_box(n.list) then
            return n, offset + rangedimensions(parent, list, n)
        elseif id == hlist_id and sb ~= eqno_sb and sb ~= mathcahr_sb and real_box(n.list) then
           offset = offset + rangedimensions(parent, list, n)
           return real_line(n.list, n, offset)
        end
    end
    return false
end

find_line = function(parent, list, column, offset, width)
    if get_attribute(parent, mark_attr) == -2 then return end
    set_attribute(parent, mark_attr, -2)
    local parent_is_vlist = parent.id == vlist_id
    for n, id, sb in traverse(list) do
        if id ~= hlist_id then
            if not n.list then goto continue end
            local new_offset, new_width = offset, width
            local new_col = get_attribute(n, col_attr)
            if get_attribute(n, mark_attr) == -1 or new_col then
                new_offset, new_width = 0, n.width
            elseif parent_is_vlist then
                new_offset = new_offset + n.shift
            end
            find_line(n, n.list, new_col or column, new_offset, new_width)
            goto continue
        end
        
        local line_attr = n.head and get_attribute(tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr] and lineno_types[line_attr][column]
        local ltype = line_type and line_type[sb]
        
        local should_number = ltype and (ltype & LINENO_NUMBER) ~= 0 or false
        local should_recurse = ltype and (ltype & LINENO_RECURSE) ~= 0 or true 
        if not (should_number or should_recurse) then
            goto continue
        end
        
        if should_number and not should_recurse then
            if real_box(n.list) then
                local new_offset = parent_is_vlist and (offset + n.shift) or offset
                lineno_callbacks(n.head, n, line_type, new_offset, width)
            end
            goto continue
        end
        
        local m, new_offset = real_line(n.head, n, offset)
        local new_width = width
        if new_offset then
            local new_col = get_attribute(m, col_attr)
            if get_attribute(m, mark_attr) == -1 or new_col then
                new_offset, new_width = 0, m.width
            elseif parent_is_vlist then
                new_offset = new_offset + n.shift
            end
            find_line(m, m.head, new_col or column, new_offset, new_width)
            if new_col then
                find_line(n, n.head, new_col, new_offset, width)
            end
            goto continue
        end
        
        if not (m and should_number) then goto continue end
        
        local new_offset = parent_is_vlist and (offset + n.shift) or offset        
        lineno_callbacks(n.head, n, line_type, new_offset, width)
        
        ::continue::
    end
end

if not plain then
    luatexbase.add_to_callback('pre_shipout_filter', function(box)
        find_line(box, box.list, 1, 0, box.width)
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
    local replace = table.concat({
      "\\_directlua{ ",
      "local column = tex.splitbox(6, tex.dimen[1], 'exactly') ",
      "local num = tex.count['_tmpnum'] ",
      "local attr = luatexbase.attributes['lualineno_col'] ",
      "node.set_attribute(column, attr, num) ",
      "node.write(column) ",
      "}",
    })
    local find = [[\_vsplit 6 to\_dimen 1 ]]
    local patch, success = token.get_macro("_createcolumns"):gsub(find, replace)
-- Log the success or failure of the patch
    if success > 0 then
        token.set_macro("_createcolumns", patch)
    else
        texio.write_nl('log', "lualineno: failed to patch \\_createcolumns")
    end

    local lbracket = new_tok(string.byte('['), token.command_id'other_char')
    local rbracket = new_tok(string.byte(']'), token.command_id'other_char')
    local label_tok = create('_label')
    make_label = function(label)
        runtoks(function()
            put_next({rbracket})
            put_next(label)
            put_next({label_tok,lbracket})
        end)
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
    local attr_num = luatexbase.attributes['lualineno_mark']
    local replace = string.format([[\moveright \@themargin \vbox attr %d = -1]], attr_num)
    local find = [[\moveright \@themargin \vbox]]
    local patch, num_subs = token.get_macro("@outputpage"):gsub(find, replace)
    find = [[\catcode `\ 10\relax \catcode `\	10\relax]]
    replace = [[\catcode 32=10\relax \catcode 9=10\relax]]
    patch, num_subs = patch:gsub(find, replace)
    if num_subs > 0 then
        token.set_macro("@outputpage", patch)
    else
        texio.write_nl('log', "lualineno: failed to patch \\@outputpage")
    end
    local label_tok = create('label')
    make_label = function(label, list, n)
        runtoks(function()
            put_next({rbrace,rbrace})
            put_next(label)
            put_next({hbox, lbrace, label_tok,lbrace})
            local label_node = scan_list()
            list = insert_after(list,n,node_copy(label_node.head))
            node_flush(label_node)
        end)
    end
end
