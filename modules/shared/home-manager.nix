{ config, pkgs, lib, ... }:

let name = "An Nyeong";
    user = "nyeong";
    email = "me@annyeong.me"; in
{
  # TODO: wezterm도 packages로 옮기자
  wezterm = {
    enable = true;
    extraConfig = ''
      local wezterm = require("wezterm")
      local config = {}
      config.color_scheme = "Nord (Gogh)"
      config.font = wezterm.font_with_fallback({
        "Liga SFMono Nerd Font",
        "Monoplex KR Wide Nerd",
      })
      config.font_size = 14.0
      config.window_padding = {
        left = 10,
        right = 10,
        top = 10,
        bottom = 10,
      }
      return config
    '';
  };

  # TODO: zsh도 packages로 옮기자
  zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    autocd = false;
    cdpath = [ "~/.local/share/src" ];
    shellAliases = {
      search = "rg -p --glob '!node_modules/*'  $@";
      grep = "rg -p --glob '!node_modules/*'  $@";
      diff = "difft";
      emacs = "emacsclient -t";
      emacsclient = "emacsclient -c -a emacs";
      switch = "nix run /Users/nyeong/.dotfiles#build-switch";

      mv = "mv -i";
      cp = "cp -i";
      rm = "rm -i";
      ln = "ln -i";
      chmod = "chmod -c";
      chown = "chown -c";
      chgrp = "chgrp -c";

      ls = "lsd";
      l = "lsd -l";
      la = "lsd -a";
      ll = "lsd -la";
      lt = "lsd --tree -I node_modules";
      tree = "lsd --tree";

      gs = "git status";
      gc = "git commit";
      gp = "git push";
      gd = "git diff";
      gds = "git diff --staged";
      gl = "git log --oneline --graph --all";
      gf = "git fetch --prune";
      gsw = "git switch";
      gcm = "git commit -m";
    };
    plugins = [
      {
          name = "powerlevel10k";
          src = pkgs.zsh-powerlevel10k;
          file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
          name = "powerlevel10k-config";
          src = lib.cleanSource ./config;
          file = "p10k.zsh";
      }
      {
        name = "zsh-expand-all";
        file = "zsh-expand-all.plugin.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/simnalamburt/zsh-expand-all";
          rev = "8efea32447ee1e390e7342e304356361a8cd16f2";
        };
      }
      {
        name ="zsh-z";
        file = "zsh-z.plugin.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/agkozak/zsh-z";
          rev = "dd94ef04acc41748ba171eb219971cb455e0040b";
        };
      }
      {
        name = "zsh-history-substring-search";
        file = "zsh-history-substring-search.plugin.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/zsh-users/zsh-history-substring-search";
          rev = "87ce96b1862928d84b1afe7c173316614b30e301";
        };
      }
    ];
    initExtra = ''
      # Define variables for directories
      export PATH=$HOME/.pnpm-packages/bin:$HOME/.pnpm-packages:$PATH
      export PATH=$HOME/.npm-packages/bin:$HOME/bin:$PATH
      export PATH=$HOME/.local/share/bin:$PATH

      # Remove history data we don't want to see
      export HISTIGNORE="pwd:ls:cd"

      # Emacs is my editor
      export ALTERNATE_EDITOR=""
      export EDITOR="emacsclient -t"
      export VISUAL="emacsclient -c -a emacs"
      export GPG_TTY=$(tty)
      export TIME_STYLE="long-iso"

      setopt complete_in_word
      setopt always_to_end
      WORDCHARS='''
      zmodload -i zsh/complist
      autoload -Uz compinit bashcompinit
      compinit -i
      bashcompinit

      zstyle ':completion:*' menu select
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
      zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"
      zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
      zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

      bindkey -e
      # up arrow
      bindkey '^[[A' history-search-backward
      # down arrow
      bindkey '^[[B' history-search-forward

      e() {
          emacsclient -t "$@"
      }

      # nix shortcuts
      shell() {
          nix-shell '<nixpkgs>' -A "$1"
      }
    '';

    initExtraFirst = ''
      if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
      fi
    '';
  };

  # TODO: vim도 packages로 옮기자
  vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-airline vim-airline-themes vim-startify vim-tmux-navigator ];
    settings = { ignorecase = true; };
    extraConfig = ''
      "" General
      set number
      set history=1000
      set nocompatible
      set modelines=0
      set encoding=utf-8
      set scrolloff=3
      set showmode
      set showcmd
      set hidden
      set wildmenu
      set wildmode=list:longest
      set cursorline
      set ttyfast
      set nowrap
      set ruler
      set backspace=indent,eol,start
      set laststatus=2
      set clipboard=autoselect

      " Dir stuff
      set nobackup
      set nowritebackup
      set noswapfile
      set backupdir=~/.config/vim/backups
      set directory=~/.config/vim/swap

      " Relative line numbers for easy movement
      set relativenumber
      set rnu

      "" Whitespace rules
      set tabstop=8
      set shiftwidth=2
      set softtabstop=2
      set expandtab

      "" Searching
      set incsearch
      set gdefault

      "" Statusbar
      set nocompatible " Disable vi-compatibility
      set laststatus=2 " Always show the statusline
      let g:airline_theme='bubblegum'
      let g:airline_powerline_fonts = 1

      "" Local keys and such
      let mapleader=","
      let maplocalleader=" "

      "" Change cursor on mode
      :autocmd InsertEnter * set cul
      :autocmd InsertLeave * set nocul

      "" File-type highlighting and configuration
      syntax on
      filetype on
      filetype plugin on
      filetype indent on

      "" Paste from clipboard
      nnoremap <Leader>, "+gP

      "" Copy from clipboard
      xnoremap <Leader>. "+y

      "" Move cursor by display lines when wrapping
      nnoremap j gj
      nnoremap k gk

      "" Map leader-q to quit out of window
      nnoremap <leader>q :q<cr>

      "" Move around split
      nnoremap <C-h> <C-w>h
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-l> <C-w>l

      "" Easier to yank entire line
      nnoremap Y y$

      "" Move buffers
      nnoremap <tab> :bnext<cr>
      nnoremap <S-tab> :bprev<cr>

      "" Like a boss, sudo AFTER opening the file to write
      cmap w!! w !sudo tee % >/dev/null

      let g:startify_lists = [
        \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
        \ { 'type': 'sessions',  'header': ['   Sessions']       },
        \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      }
        \ ]

      let g:startify_bookmarks = [
        \ '~/.local/share/src',
        \ ]

      let g:airline_theme='bubblegum'
      let g:airline_powerline_fonts = 1
      '';
     };

  # TODO: ssh도 packages로 옮기자
  ssh = {
    enable = true;
    includes = [
      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
        "/home/${user}/.ssh/config_external"
      )
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
        "/Users/${user}/.ssh/config_external"
      )
    ];
    matchBlocks = {
      "github.com" = {
        identitiesOnly = true;
        identityFile = [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
            "/home/${user}/.ssh/id_ed25519"
          )
          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
            "/Users/${user}/.ssh/id_ed25519"
          )
        ];
      };
    };
  };

}
