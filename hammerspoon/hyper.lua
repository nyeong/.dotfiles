-- HYPER
--
-- References:
-- https://github.com/evantravers/hammerspoon-config/blob/38a7d8c0ad2190d1563d681725628e4399dcbe6c/hyper.lua

local hyper = hs.hotkey.modal.new({}, nil)

hyper.pressed = function()
    hyper:enter()
end

hyper.released = function()
    hyper:exit()
end

hs.hotkey.bind({}, 'f13', hyper.pressed, hyper.released)

return hyper
