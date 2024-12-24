{
  description = "My Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, darwin, home-manager }@inputs: {
    darwinConfigurations.nyeong = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      configuration = {
        home.homeDirectory = "/Users/nyeong";
        home.stateVersion = "23.05";
        home.username = "nyeong";
      };
      modules = [
          home-manager.darwinModules.home-manager
         ./modules/darwin/home.nix
      ];
      };
  };
}
