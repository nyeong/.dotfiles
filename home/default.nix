{ userConfig, pkgs, inputs, home-manager, ... }: {
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    verbose = true;
    users.${userConfig.username} = { pkgs, ... }: {
      home.username = userConfig.username;
      home.homeDirectory = userConfig.home;
      home.stateVersion = "24.11";
      home.sessionVariables = {
        EDITOR = "nvim";
      };

      programs.home-manager.enable = true;

      imports = [
        ../packages/darwin.nix
      ];
    };
  };
}
