"
" General Configs
"
scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
set noswapfile
set nowrap
set mouse=a
set nocompatible " Neovim default
filetype plugin on
syntax on

" Visual

" coc.nvim
set hidden
set cmdheight=2
set nobackup
set nowritebackup
set updatetime=300
set shortmess+=c
set signcolumn=yes

" Indentation
set cindent
set autoindent
set smartindent

" Tab
set softtabstop=2
set shiftwidth=2
set expandtab

" Line number column
set number
set cursorline
set textwidth=80
set formatoptions-=t
set colorcolumn=+1,+2,+3 " 가로줄 긋기

" Searching
set incsearch
set ignorecase
set smartcase
set hlsearch | nohlsearch
set nowrapscan

"
" Key mappings
"
cabbrev ㅈ w
cabbrev ㅂ q
cabbrev ㅈㅂ wq
let mapleader="'"
map <leader><S-Tab> :bp<CR>
map <leader><Tab> :bn<CR>

"
" On Save
"
" remove whitespaces
autocmd BufWritePre * %s/\s\+$//e

"
" Plugins
"
try
  call plug#begin('~/.local/share/nvim/plugged')
    " Text
    Plug 'tpope/vim-surround'

    " Theme
    Plug 'joshdick/onedark.vim'
    Plug 'junegunn/seoul256.vim'

    " Visual
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'ntpeters/vim-better-whitespace'
    Plug 'itchyny/lightline.vim'
    Plug 'scrooloose/nerdtree'
    Plug 'mhinz/vim-startify' " start screen

    " Error
    Plug 'vim-syntastic/syntastic'

    " Intellisense
    Plug 'neoclide/coc.nvim', {'branch': 'release'}

    " Syntax
    Plug 'sheerun/vim-polyglot'
    Plug 'rust-lang/rust.vim'
    Plug 'plasticboy/vim-markdown'
    Plug 'elixir-editors/vim-elixir'

    " Plugins
    Plug '/usr/local/opt/fzf'
    Plug 'ybian/smartim' " IME

    let s:has_vimplug = 1
  call plug#end()
endtry

if exists('s:has_vimplug') && s:has_vimplug
  " color scheme: seoul256, onedark
  colo onedark

  " smartim
  let g:smartim_default = 'com.apple.keylayout.ABC'

  " vim-better-whitespace
  let g:strip_whitespace_on_save = 1

  " lightline.vim
  " color scheme: seoul256, one
  let g:lightline = {
        \ 'colorscheme': 'one',
        \ }

  " rust.vim
  let g:rustfmt_autosave = 1

  " vim-indent-guides
  let g:indent_guides_enable_on_vim_startup = 1
  let g:indent_guides_start_level = 2
  let g:indent_guides_auto_colors = 0

  " vim-syntastic
  set statusline+=%#warningmsg#
  set statusline+=%{SyntasticStatuslineFlag()}
  set statusline+=%*

  let g:syntastic_always_populate_loc_list = 1
  let g:syntastic_auto_loc_list = 1
  let g:syntastic_check_on_open = 1
  let g:syntastic_check_on_wq = 0

  " vim-markdown
  let g:vim_markdown_math = 1
  let g:vim_markdown_frontmatter = 1
  let g:vim_markdown_folding_disabled = 1
  let g:vim_markdown_no_default_key_mappings = 1

  " nerdtree
  map <leader>b :NERDTreeToggle<CR>

  " FZF
  map <leader>p :FZF<CR>

  " coc.nvim
  " Use tab for trigger completion
  inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ coc#refresh()
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction

  " Use K for show documentation
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  function! s:show_documentation()
    if &filetype == 'vim'
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction

  " Highlight symbol under cursor on CursorHold
  autocmd CursorHold * silent call CocActionAsync('highlight')

  " Remap for rename current word
  nmap <leader>rn <Plug>(coc-rename)
endif

