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