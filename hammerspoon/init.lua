config = {}

ime = require('ime')

local function pressn(mods, key)
    if key == nil then
        key = mods
        mods = {}
    end

    return function() hs.eventtap.keyStroke(mods, key, 1000) end
end

-- hyper key binding
hyper = require('hyper')

-- PaperWM = hs.loadSpoon('PaperWM')
-- PaperWM:bindHotkeys(PaperWM.default_hotkeys)

-- hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "h", PaperWM.actions.focus_left)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "j", PaperWM.actions.focus_down)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "k", PaperWM.actions.focus_up)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd" }, "l", PaperWM.actions.focus_right)

-- hs.hotkey.bind({ "ctrl", "alt", "cmd", "shift" }, "h", PaperWM.actions.swap_left)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd", "shift" }, "j", PaperWM.actions.swap_down)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd", "shift" }, "k", PaperWM.actions.swap_up)
-- hs.hotkey.bind({ "ctrl", "alt", "cmd", "shift" }, "l", PaperWM.actions.swap_right)
-- PaperWM:start()
