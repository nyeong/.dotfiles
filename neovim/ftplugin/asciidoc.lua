local function new_template()
    if vim.api.nvim_buf_line_count(0) > 1 or vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] ~= "" then
        return
    end

    local content = {}
    table.insert(content, '= Title')
    table.insert(content, 'An Nyeong <me@annyeong.me>')
    table.insert(content, ':description: ')
    table.insert(content, ':keywords: ')
    table.insert(content, ':created_at: ' .. os.date("%Y-%m-%d %H:%M:%S"))
    table.insert(content, '')
    table.insert(content, '이곳에 초록을 작성')
    table.insert(content, '')
    table.insert(content, '[bibliography]')
    table.insert(content, '== 참고')
    table.insert(content, '')
    table.insert(content, '. 작성자, 이름.')

    vim.api.nvim_buf_set_lines(0, 0, -1, false, content)
end

local function findXrefPattern(line, col, callback)
    for match in line:gmatch("<<(.-)#.->>") do
        local matchPattern = "<<" .. match:gsub("%-", "%%-") .. "#.->>"
        local s, e = line:find(matchPattern)
        print(match)
        if (s - 1) <= col and col <= (e + 1) then
            local filepath = match
            if not filepath:match("%.adoc$") then
                filepath = filepath .. ".adoc"
            end
            callback(filepath)
        else
            if not match:match("%.adoc$") then
                match = match .. ".adoc"
            end
            callback(match)
        end
    end
end

local function findHttp(line, col, callback)
    for match in line:gmatch("https?://[%w%d%.?]+[/%w%d%.%-%_%?%=]+") do
        local s, e = line:find(match)
        if s and (s - 1) <= col and col <= (e + 1) then
            callback(match)
        else
            callback(match)
        end
    end
end

local function openFromPattern()
    local _, col = unpack(vim.api.nvim_win_get_cursor(0))
    local line = vim.api.nvim_get_current_line()

    findXrefPattern(line, col, function (filepath) vim.cmd('edit ' .. filepath) end)
    findHttp(line, col, function (link) vim.cmd('silent !open ' .. "'"  .. link .. "'") end)
end

vim.opt.conceallevel = 3

-- Set up the key mapping
vim.keymap.set('n', '<CR>', openFromPattern)

vim.api.nvim_create_autocmd('BufNewFile', {
    callback = new_template
})
