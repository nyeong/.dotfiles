{ pkgs, lib, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    completionInit = "autoload -U compinit && compinit -i";
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
      gl = "git log --oneline --graph";
      glg = "git log --grep";
      gls = "git log -S";
      glG = "git log -G";
      gla = "git log --oneline --graph --all";
      gf = "git fetch --prune";
      gsw = "git switch";
      gca = "git commit --amend --no-edit";
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
        name = "zsh-z";
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
    initContent = ''
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
      autoload -Uz bashcompinit
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
      if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
      fi
    '';
  };
}
