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
local put_next = token.unchecked_put_next
local create = token.create
local new_tok = token.new

local lbrace, rbrace = new_tok(string.byte('{'), 1), new_tok(string.byte('}'), 2)
local hbox = new_tok(141, 21)

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

local lineno_types = { }
local lineno_attr = { }
local defaults = {
    toks = { },
    start = { },
    ['end'] = { },
    box = 'true',
    alignment = 'true',
    equation = 'true',
    line = 'true',
    offset = 'true'
}

local keyval = require('luakeyval')
local scan_choice = keyval.choices
local process_keys = keyval.process
local defaults_keys = {
    toks = {scanner = scan_toks},
    start = {scanner = scan_toks},
    ['end'] = {scanner = scan_toks},
    box = {scanner = scan_choice, args = {'true', 'false', 'inline'}},
    alignment = {scanner = scan_choice, args = {'true', 'false', 'once'}},
    equation = {scanner = scan_choice, args = {'true', 'false', 'once'}},
    line = {scanner = scan_choice, args = {'true', 'false'}},
    offset = {scanner = scan_choice, args = {'true', 'false'}}
}

local help_message = [[
the last scanned key was "%s".
there is a "%s" in the way.
]]
local function set_defaults()
    local vals = process_keys(defaults_keys,
      "lualineno: wrong syntax when setting defaults",
      help_message)
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
-- This function is used in the define key.
-- It is very similar to the set_defaults() function,
-- but it accepts a `column` and `name` keys as well.
    local vals = process_keys(define_keys,
      "lualineno: wrong syntax when defining a lineno",
      help_message)
-- A newly defined lualineno type must have a name
    local name = vals['name']
    if not name then 
        texerror("lualineno: missing name in when defining a lineno")
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
    for k,v in pairs(defaults) do 
        c[k] = vals[k] or defaults[k]
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
        luatexbase.add_to_callback('lualineno.pre_numbering', make_labels, 'lualineno.labels')
    elseif latex then
        luatexbase.add_to_callback('lualineno.post_numbering', make_labels, 'lualineno.labels')
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
    define = {func = define_lineno},
    defaults = {func = set_defaults},
    algorithm = {scanner = scan_choice, args = {'human', 'tex'}},
    anchor,
    label = {scanner = scan_toks, args = {false, true}},
    line_attr = {scanner = scan_int},
    col_attr = {scanner = scan_int},
    processbox = {scanner = scan_int},
}
local texnest = tex.nest
local number_lines_human

local function lualineno()
    local saved_endlinechar = tex.endlinechar
    tex.endlinechar = 32
    local vals = process_keys(lualineno_keys,
      "lualineno: wrong syntax in \\lualineno",
      help_message)
    tex.endlinechar = saved_endlinechar
    if vals.set then
        local attr = lineno_attr[vals.set]
        setattribute(type_attr, attr and attr or unset_attr) 
    end
    local alg = vals.algorithm
    if alg == "human" then alg_bool = true end
    if alg == "tex" then alg_bool = false end
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
    if vals.processbox then 
        local box = tex.box[vals.processbox]
        number_lines_human(box, box.head, 1, 0)
        node.set_attribute(box, type_attr, -2)
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
-- \docfile
local get_attribute = node.get_attribute
local insert_before = node.insert_before
local insert_after = node.insert_after
local traverse = node.traverse

local function add_boxes_to_line(n, parent, line_type, offset)
-- In case \LaTeX/ is used without the luacolor package,
-- we add an additional group to make the boxes color safe.
    put_next({rbrace, rbrace})
    put_next(line_type['start'])
    put_next({hbox, lbrace, lbrace})
    local start_box = scan_list()
    if start_box.head then
        local start_kern = node.new('kern', 1)
        local shift_kern = node.new('kern', 1)
        start_kern.kern = -start_box.width - n.shift - offset
        shift_kern.kern = n.shift + offset
        if shift_kern.kern ~= 0 then
            n.head = insert_before(n.list,n.head,shift_kern)
        end
        n.head = insert_before(n.list,n.head,start_box)
        if start_kern.kern ~= 0 then
            n.head = insert_before(n.list,n.head,start_kern)
        end
    end
    put_next({rbrace, rbrace})
    put_next(line_type['end'])
    put_next({hbox, lbrace, lbrace})
    local end_box = scan_list()
    if end_box.head then
        local end_kern = node.new('kern', 1)
        end_kern.kern = parent.width - n.shift - n.width + offset
        if end_kern.kern ~= 0 then
            n.head = insert_after(n.list,tail(n.head),end_kern)
        end
        n.head = insert_after(n.list,tail(n.head),end_box)
    end
    return n.head
end

local call_lineno_callbacks
if luatexbase then
    luatexbase.create_callback('lualineno.pre_numbering', 'list', false)
    luatexbase.create_callback('lualineno.numbering', 'exclusive', add_boxes_to_line)
    luatexbase.create_callback('lualineno.post_numbering', 'reverselist', false)
    luatexbase.add_to_callback('lualineno.pre_numbering', function(n, parent, line_type, offset)
        runtoks(function() put_next(line_type['toks']) end) 
    return true end, 'lualineno.runtoks')
    local call_callback = luatexbase.call_callback
    call_lineno_callbacks = function(head, parent, line_type, offset)
        local current = call_callback('lualineno.pre_numbering', head, parent, line_type, offset)
        head = current == true and head or current
        current = call_callback('lualineno.numbering', head , parent, line_type, offset)
        head = current == true and head or current
        call_callback('lualineno.post_numbering', head , parent, line_type, offset)
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

number_lines_human = function(parent, list, column, offset)
    if get_attribute(parent, type_attr) == -2 then return end
    node.set_attribute(parent, type_attr, -2)
    column = get_attribute(parent, col_attr) or column
    for n in traverse(list) do
        local line_attr = n.head and get_attribute(tail(n.head), type_attr)
        local line_type = line_attr and lineno_types[line_attr][column]
        if n.id == hlist_id and line_type then
            local ltype = line_type[hlist_subs[n.subtype]]
            if ltype == 'true' or n.subtype == 0 then
                local m, new_offset = real_line(n.head, n, offset)
                if new_offset then
                    if get_attribute(m, type_attr) == -1 then
                        new_offset = 0
                    else
                        if parent.id == vlist_id then
                            new_offset = new_offset + n.shift
                        end
                    end
                    number_lines_human(m, m.head, column, new_offset)
                    if get_attribute(m, col_attr) then
                        number_lines_human(n, n.head, column, new_offset)
                    end
                elseif m then
                    local final_offset = line_type['offset'] == 'true' and offset or 0
                    call_lineno_callbacks(n, parent, line_type, final_offset)
                end
            elseif ltype == 'once' and real_box(n.list) then
                local final_offset = line_type['offset'] == 'true' and offset or 0
                call_lineno_callbacks(n, parent, line_type, final_offset)
            else
                local is_offset = get_attribute(n, type_attr) == -1 and 0 or offset
                number_lines_human(n, n.list, column, is_offset)
            end
        elseif n.list then
            local is_offset = get_attribute(n, type_attr) == -1 and 0 or offset
            number_lines_human(n, n.list, column, is_offset)
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
    local patch, success = token.get_macro("_createcolumns"):gsub(find, replace)
-- Log the success or failure of the patch
    if success then
        token.set_macro("_createcolumns", patch)
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
                            put_next({rbracket})
                            put_next(label)
                            put_next({label_tok,lbracket})
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
                            put_next({rbrace,rbrace})
                            put_next(label)
                            put_next({hbox, lbrace, label_tok,lbrace})
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