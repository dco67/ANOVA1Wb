-- encadres.lua — encadrés pédagogiques du manuel, dans les trois formats.
--   .app-step : « Dans l'application » (marche à suivre à l'écran)
--   .case     : « Cas fil rouge »
--   HTML / EPUB : classes conservées, titres et couleurs dans styles.css
--   PDF         : environnements tcolorbox définis dans preamble.tex

local titres = { ["app-step"] = "Dans l'application", ["case"] = "Cas fil rouge" }
local envs   = { ["app-step"] = "encadreapp",        ["case"] = "encadrecas" }

function Div(el)
  if not FORMAT:match("latex") then return nil end
  for cls, env in pairs(envs) do
    if el.classes:includes(cls) then
      local out = { pandoc.RawBlock("latex", "\\begin{" .. env .. "}" ..
                    "\\textbf{" .. titres[cls] .. "}\\par\\smallskip") }
      for _, b in ipairs(el.content) do table.insert(out, b) end
      table.insert(out, pandoc.RawBlock("latex", "\\end{" .. env .. "}"))
      return out
    end
  end
  return nil
end
