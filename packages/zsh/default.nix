{ config, pkgs, lib, userConfig, ... }: {
  home.packages = with pkgs; [
    curl
    zsh
    git
  ];

  home.activation.zinit = lib.hm.dag.entryAfter ["writeBoundary"] ''
      export PATH="${pkgs.zsh}/bin:${pkgs.git}/bin:${pkgs.curl}/bin:$PATH"
      bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
    '';

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
      export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
      source ${./config/p10k.zsh}
      source ${./config/init.zsh}
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
