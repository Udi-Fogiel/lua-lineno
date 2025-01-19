#!/usr/bin/env texlua

-- Identify the bundle and module
bundle = ""
module = "lualineno"

stdengine    = "luatex"
checkengines = {"luatex"}
checkruns = 1
sourcefiles = {"*.opm", "*.sty", "*.lua", "*.tex"}
installfiles = sourcefiles
packtdszip = true
typesetexe = "optex"
typesetfiles = {"lualineno.opm"}
ctanzip = module

checkconfigs = {"configfiles/config-optex", "configfiles/config-latex", "configfiles/config-plain"}
specialformats = specialformats or { }
specialformats.optex  = {luatex = {binary = "optex", format = ""}}
specialformats.plain  = {luatex = {binary = "luatex", format = ""}}

tdslocations =
  {
    "tex/optex/lualineno/*.opm",
    "tex/lualatex/lualineno/*.sty",
    "tex/luatex/lualineno/*.tex",
    "tex/luatex/lualineno/*.lua",
  }

specialtypesetting = specialtypesetting or {}
function optex_doc()
    run('.', "optex -jobname lualineno-doc '\\docgen lualineno'")
end
specialtypesetting["lualineno.opm"] = {func = optex_doc}

tagfiles = sourcefiles
function update_tag(file,content,tagname,tagdate)
  if string.match(file, "%.opm$") then
    return string.gsub(content,
      "lualineno_version {%d+.%d+, %d%d%d%d-%d%d-%d%d",
      "lualineno_version {" .. tagname .. ", " .. tagdate)
  elseif string.match(file, "%.lua$") then
    return content
  elseif file == "lualineno.sty" then
    return string.gsub(content,
      "{lualineno} [%d%d%d%d-%d%d-%d%d v%d+.%d+.%d+",
      "{lualineno} [" .. tagdate .. " v" .. tagname)
  elseif file == "README.md" then
    return content
  end
end

function pre_release()
    call({"."}, "tag")
    call({"."}, "ctan", {config = options['config']})
    rm(".", "*.tds.zip")
end

target_list["prerelease"] = { func = pre_release, 
			desc = "update tags, generate pdfs, build zip for ctan"}

