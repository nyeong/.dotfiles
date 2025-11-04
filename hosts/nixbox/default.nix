{
  inputs,
  mkSpecialArgs,
  ...
}: let
  system = "x86_64-linux";
  specialArgs = mkSpecialArgs system;
  palette = specialArgs.palette;
in
  inputs.nixpkgs.lib.nixosSystem {
    inherit system specialArgs;
    modules = [
      ./configuration.nix
      inputs.agenix.nixosModules.default
      inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${palette.user.username} = import ./home-manager.nix;
        home-manager.extraSpecialArgs = specialArgs;
      }
    ];
  }
