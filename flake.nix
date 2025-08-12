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
    # Formatting + Git hooks
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    nix-darwin,
    home-manager,
    nixpkgs,
    disko,
    treefmt-nix,
    git-hooks,
  } @ inputs: let
    userConfig = import ./shared/user-config.nix;

    commonArgs = {
      inherit userConfig;
    };
    systems = ["aarch64-darwin" "x86_64-linux"];
    perSystem = nixpkgs.lib.genAttrs systems (system: let
      pkgs = import nixpkgs {inherit system;};
      treefmtEval = treefmt-nix.lib.evalModule pkgs {
        projectRootFile = "flake.nix";
        programs = {
          alejandra.enable = true; # Nix formatter
          stylua.enable = true; # Lua (Hammerspoon)
          shfmt.enable = true; # Shell
          prettier.enable = true; # JSON/Markdown/YAML/etc.
        };
      };
      preCommit = git-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          treefmt = {
            enable = true;
            package = treefmtEval.config.build.wrapper;
          };
          # Warn-only Nix linters (do not fail commit)
          statix-warn = {
            enable = true;
            name = "statix (warn)";
            language = "system";
            files = "\\.nix$";
            pass_filenames = true;
            entry = "${pkgs.bash}/bin/bash";
            args = [
              "-c"
              ''${pkgs.statix}/bin/statix check --format=stderr "$@" || true''
              "--"
            ];
          };
          deadnix-warn = {
            enable = true;
            name = "deadnix (warn)";
            language = "system";
            files = "\\.nix$";
            pass_filenames = true;
            entry = "${pkgs.bash}/bin/bash";
            args = [
              "-c"
              ''${pkgs.deadnix}/bin/deadnix --no-progress "$@" || true''
              "--"
            ];
          };
        };
      };
      # Strict variant for CI (fails on warnings)
      ciPreCommit = git-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          treefmt = {
            enable = true;
            package = treefmtEval.config.build.wrapper;
          };
          statix.enable = true;
          deadnix.enable = true;
        };
      };
    in {
      inherit pkgs treefmtEval preCommit ciPreCommit;
    });
  in {
    # nix fmt → run treefmt wrapper
    formatter = nixpkgs.lib.genAttrs systems (system: (perSystem.${system}.treefmtEval.config.build.wrapper));
    # pre-commit hook auto-installs when entering dev shell
    devShells = nixpkgs.lib.genAttrs systems (system: {
      default = (perSystem.${system}.pkgs).mkShell {
        shellHook = (perSystem.${system}.preCommit).shellHook;
      };
    });
    # CI/local checks
    checks = nixpkgs.lib.genAttrs systems (system: {
      pre-commit = perSystem.${system}.preCommit; # warn-only hooks
      formatting = perSystem.${system}.treefmtEval.config.build.check self;
    });
    # CI strict lint as a buildable package
    packages = nixpkgs.lib.genAttrs systems (system: {
      ci-lint = perSystem.${system}.ciPreCommit;
    });
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
