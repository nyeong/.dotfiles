{
  inputs = {
    # nix
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # homebrew
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    homebrew-core.url = "github:homebrew/homebrew-core";
    homebrew-core.flake = false;
    homebrew-cask.url = "github:homebrew/homebrew-cask";
    homebrew-cask.flake = false;

    # Formatting + Git hooks
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    # secrets
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    secrets.url = "git+ssh://git@github.com/nyeong/secrets.git";
    secrets.flake = false;
    nixbox-private.url = "git+ssh://git@github.com/nyeong/nixbox-private.git";
    nixbox-private.inputs.nixpkgs.follows = "nixpkgs";

    # overlays & package dependencies
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nix-ai-tools.url = "github:numtide/nix-ai-tools";
  };
  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    inherit (inputs.nixpkgs) lib;
    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-linux"
    ];
    palette = import ./palette {inherit lib systems;};

    # Load overlays from overlays/default.nix
    overlays = (import ./overlays) {inherit palette;};

    # home-manager, nixosSystem, darwinSystem context에서 쓰는 args
    mkSpecialArgs = system: {
      inherit
        palette
        inputs
        system
        overlays
        ;
      inherit (inputs) secrets;
      pkgs-stable = import inputs.nixpkgs-stable {
        inherit system overlays;
        config.allowUnfree = true;
      };
      isDarwin = palette.lib.isDarwin system;
      isLinux = palette.lib.isLinux system;
      nix-ai-tools-pkgs = inputs.nix-ai-tools.packages.${system};
    };

    perSystem = palette.lib.forAllSystems (
      system:
        palette.lib.mkPerSystemConfig {
          pkgs = import nixpkgs {inherit system;};
          inherit system;
          treefmt-nix = inputs.treefmt-nix;
          git-hooks = inputs.git-hooks;
          repoRoot = self;
        }
    );
  in {
    formatter = palette.lib.forAllSystems (
      system: (perSystem.${system}.treefmtEval.config.build.wrapper)
    );
    devShells = palette.lib.forAllSystems (system: {
      default = (perSystem.${system}.pkgs).mkShell {
        shellHook = (perSystem.${system}.preCommit).shellHook;
        packages = with perSystem.${system}.pkgs; [
          nil
          nixfmt-rfc-style
        ];
      };
    });
    checks = palette.lib.forAllSystems (system: {
      pre-commit = perSystem.${system}.preCommit;
      formatting = perSystem.${system}.treefmtEval.config.build.check self;
    });

    darwinConfigurations."nyeong-air" = import ./hosts/nyeong-air {
      inherit inputs mkSpecialArgs;
    };

    nixosConfigurations."nixvm" = import ./hosts/nixvm {
      inherit inputs mkSpecialArgs;
    };

    nixosConfigurations."nixbox" = import ./hosts/nixbox {
      inherit inputs mkSpecialArgs;
    };
  };
}
