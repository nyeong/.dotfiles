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
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nix-darwin, home-manager, nixpkgs, disko, }@inputs:
    let
      userConfig = import ./shared/user-config.nix;

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
      nixosConfigurations."nixbox" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = commonArgs;
        modules = [
          ./hosts/nixbox
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${userConfig.username} = import ./hosts/nixbox/home-manager.nix;
            home-manager.extraSpecialArgs = commonArgs;
          }
        ];
      };
    };
}
