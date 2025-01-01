# ~/.dotfiles/packages/zsh/default.nix
{ config, pkgs, userConfig, ... }: {
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    historySubstringSearch.enable = true;

    shellAliases = {
      switch = "darwin-rebuild switch --flake ~/.dotfiles#${userConfig.hostname}";

      mv = "mv -i";
      cp = "cp -i";

      # git
      gs = "git status";
      gc = "git commit";
      gp = "git push";
      gd = "git diff";
      gds = "git diff --staged";
      gsw = "git switch";
      ga = "git add";
      gl = "git log --oneline --graph";
      gf = "git fetch --prune";
      gcm = "git commit -m";

      # bat
      cat = "bat";

      # lsd
      ls = "lsd";
      l = "lsd -l";
      ll = "lsd -la";
      la = "lsd -a";
      lt = "ls --tree";
      tree = "ls --tree";

      # nvim
      vi = "nvim";
      vim = "nvim";
    };

    initExtra = ''
      if [[ -r "${''XDG_CACHE_HOME:-$HOME/.cache''}/p10k-instant-prompt-${''(%):-%n''}.zsh" ]]; then
        source "${''XDG_CACHE_HOME:-$HOME/.cache''}/p10k-instant-prompt-${''(%):-%n''}.zsh"
      fi
      source ${./p10k.zsh}
      export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
      export GPG_TTY=$(tty)
      export TIME_STYLE='long-iso'

      # zsh substring completion
      setopt complete_in_word
      setopt always_to_end
      WORDCHARS=""

      bindkey -e
      bindkey '^[[A' history-substring-search-up
      bindkey '^[[B' history-substring-search-down

      zstyle ':completion:*' menu select
      zstyle ':completion:*' list-colors "${''(s.:.)LS_COLORS''}"
    '';

    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];
  };
}
