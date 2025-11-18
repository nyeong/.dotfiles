{
  inputs,
  mkSpecialArgs,
  ...
}: let
  system = "aarch64-darwin";
  specialArgs = mkSpecialArgs system;
  palette = specialArgs.palette;
  username = palette.user.username;
in
  inputs.nix-darwin.lib.darwinSystem {
    inherit system specialArgs;
    modules = [
      inputs.nix-homebrew.darwinModules.nix-homebrew
      ./configuration.nix
      inputs.home-manager.darwinModules.home-manager
      inputs.sops-nix.darwinModules.sops
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${username} = import ./home-manager.nix;
        home-manager.extraSpecialArgs = specialArgs;
      }
    ];
  }
