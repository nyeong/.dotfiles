require("core.lazy")
require("core.config")
require("core.autocmd")
-- require 'core.keymap'
vim.cmd([[
    augroup cursor_move_selected_word
        " :so $VIMRUNTIME/syntax/hitest.vim
        autocmd CursorMoved * exe printf('match CursorSelected001 /\V\<%s\>/', escape(expand('<cword>'), '/\'))
        highlight CursorSelected001 ctermfg=14 ctermbg=23 guifg=#00ffff guibg=#005f5f
    augroup END
]])
