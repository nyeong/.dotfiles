{
  description = "My nix config";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-homebrew = { url = "github:zhaofengli-wip/nix-homebrew"; };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    secrets = {
      url = "git+ssh://git@github.com/nyeong/secrets.git";
      flake = false;
    };
  };
  outputs = { self, nix-darwin, nix-homebrew, homebrew-bundle, homebrew-core
    , homebrew-cask, home-manager, nixpkgs, disko, secrets, }@inputs:
    let
      userConfig = import ./modules/shared/user-config.nix;

      commonArgs = {
        inherit userConfig;
      };
    in {
      darwinConfigurations."nyeong-air" = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = commonArgs;
        modules = [
          ./hosts/nyeong-air
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${userConfig.username} = import ./hosts/nyeong-air/home-manager.nix;
            home-manager.extraSpecialArgs = commonArgs;
          }
        ];
      };
    };
}
