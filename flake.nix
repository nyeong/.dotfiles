{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, darwin, nixpkgs, home-manager }:
  let
    config = {
      username = "nyeong";
      hostname = "subin-dev";
      home = "/Users/nyeong";
      email = "me@annyeong.me";
    };
  in
  {
    darwinConfigurations.${config.hostname}= darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit config; };
      modules = [
        home-manager.darwinModules.home-manager
        ./system/darwin.nix
        ./home/default.nix
      ];
    };
  };
}
