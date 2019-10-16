"
" General Configs
"
scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
set noswapfile
set nowrap
" set mouse=a          " allow to use mouse
set nocompatible " Neovim default
filetype plugin on
syntax on

" Visual
if (has("termguicolors"))
  set termguicolors
endif

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
set softtabstop=4
set shiftwidth=4
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

    " Plugins
    Plug '/usr/local/opt/fzf'
    Plug 'simnalamburt/vim-tiny-ime', { 'do': './build' } " IME switcher

    " Wiki
    Plug 'vimwiki/vimwiki'

    let s:has_vimplug = 1
  call plug#end()
endtry

if exists('s:has_vimplug') && s:has_vimplug
  " color scheme: seoul256, onedark
  colo onedark

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
endif

