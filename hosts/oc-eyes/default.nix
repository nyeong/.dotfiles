{
  inputs,
  mkSpecialArgs,
  ...
}: let
  system = "aarch64-linux";
  specialArgs = mkSpecialArgs system;
  palette = specialArgs.palette;
in
  inputs.nixpkgs.lib.nixosSystem {
    inherit system specialArgs;
    modules = [
      ./hardware-configuration.nix
      ./configuration.nix
      inputs.sops-nix.nixosModules.sops
      inputs.home-manager.nixosModules.home-manager
      inputs.namescale.nixosModules.namescale
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${palette.user.username} = import ./home-manager.nix;
        home-manager.extraSpecialArgs = specialArgs;
      }
    ];
  }
