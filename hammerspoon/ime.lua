function is_contains(target, list)
    for i, v in pairs(list) do
        if string.find(target, v) then
            return true
        end
    end
    return false
end

local esc_bind
function back_to_eng()
	print(hs.keycodes.currentSourceID())
    local targetForEnglish = {'iTerm', '터미널', 'Code'}
	-- esc의 원래 동작 보장. 참고 https://github.com/johngrib/johngrib.github.io/issues/130#issuecomment-699728202
	esc_bind:disable()
	hs.eventtap.keyStroke({}, 'escape')
	esc_bind:enable()

	local frontmostApplication = hs.application.frontmostApplication()

	if frontmostApplication and is_contains(frontmostApplication:name(), targetForEnglish) then
		local inputEnglish = "com.apple.keylayout.ABC"
		local inputSource = hs.keycodes.currentSourceID()
		if not (inputSource == inputEnglish) then

			hs.eventtap.keyStroke({}, 'left')
			hs.eventtap.keyStroke({}, 'right')
			hs.keycodes.currentSourceID(inputEnglish)
		end
	end
end

esc_bind = hs.hotkey.new({}, 'escape', back_to_eng):enable()
