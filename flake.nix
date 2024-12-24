{
  description = "My Home Manager flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs: {
    "nyeong" = inputs.home-manager.lib.homeManagerConfiguration {
      system = "aarch64-darwin";
      homeDirectory = "/Users/nyeong";
      stateVersion = "21.05";
      configuration.imports = [ ./modules/darwin/home.nix ];
    };
  };
}
