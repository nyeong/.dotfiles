" General Configs "
scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
set noswapfile
set nowrap
set mouse=a
filetype plugin on
syntax on

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

" Key mappings
let g:mapleader="'"
map <leader><S-Tab> :bp<CR>
map <leader><Tab> :bn<CR>

"
" On Save
"
" remove whitespaces
autocmd BufWritePre * %s/\s\+$//e

" Plugins
try
  call plug#begin('~/.local/share/nvim/plugged')
    " Text
    Plug 'tpope/vim-surround'
    Plug 'ybian/smartim' " IME

    " Theme
    Plug 'junegunn/seoul256.vim'

    " Visual
    Plug 'tpope/vim-sensible'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'ntpeters/vim-better-whitespace'
    Plug 'vim-airline/vim-airline' " bottom bar
    Plug 'vim-airline/vim-airline-themes'
    Plug 'mhinz/vim-startify'      " start screen

    " File finding
    Plug '/usr/local/opt/fzf'
    Plug 'scrooloose/nerdtree'   " sidebar

    " Error
    Plug 'vim-syntastic/syntastic'

    " Intellisense
    Plug 'prabirshrestha/async.vim'
    Plug 'prabirshrestha/vim-lsp'

    " Syntax
    Plug 'sheerun/vim-polyglot'
    Plug 'rust-lang/rust.vim'
    Plug 'plasticboy/vim-markdown'
    Plug 'elixir-editors/vim-elixir'
  call plug#end()

  " color scheme: seoul256
  set background=dark
  let g:seoul256_background=236
  colo seoul256

  " smartim
  let g:smartim_default = 'com.apple.keylayout.ABC'

  " vim-better-whitespace
  let g:strip_whitespace_on_save = 1

  " vim-airline
  let g:airline_theme='zenburn'

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
endtry
