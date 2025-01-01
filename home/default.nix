{ config, pkgs, inputs, home-manager, ... }: {
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    verbose = true;
    users.${config.username} = { pkgs, ... }: {
      home.username = config.username;
      home.homeDirectory = config.home;
      home.stateVersion = "24.11";
      home.packages = with pkgs; [
        curl
        neovim
        fd
        ripgrep
        jq
        git
        lsd
        discord
      ];
      home.sessionVariables = {
        EDITOR = "nvim";
      };

      programs.home-manager.enable = true;
      programs.zsh = {
        enable = true;
        initExtra = builtins.readFile ./zsh/zshrc;
        shellAliases = {
          switch = "darwin-rebuild switch --flake ~/.dotfiles#${config.hostname}";
        };
      };
    };
  };
}
