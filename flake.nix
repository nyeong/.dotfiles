{
  inputs = {
    # nix
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    namescale.url = "github:sinanmohd/namescale";
    namescale.inputs.nixpkgs.follows = "nixpkgs";

    # homebrew
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    homebrew-core.url = "github:homebrew/homebrew-core";
    homebrew-core.flake = false;
    homebrew-cask.url = "github:homebrew/homebrew-cask";
    homebrew-cask.flake = false;
    homebrew-playcover.url = "github:PlayCover/homebrew-playcover";
    homebrew-playcover.flake = false;
    homebrew-tw93.url = "github:tw93/homebrew-tap";
    homebrew-tw93.flake = false;

    # Formatting + Git hooks
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";

    # secrets
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    dotfiles-private.url = "git+ssh://git@github.com/nyeong/nixbox-private.git";
    dotfiles-private.inputs.nixpkgs.follows = "nixpkgs";

    # overlays & package dependencies
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    nix-ai-tools.url = "github:numtide/nix-ai-tools";
    ki-editor.url = "github:ki-editor/ki-editor";
    opencode.url = "github:aweis89/opencode/enable-acp";
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

    # Layer 4: Pure utilities and data
    utils = import ./utils {inherit lib systems;};
    data = import ./data {inherit lib utils;};

    # Backward-compatible palette that combines utils and data
    # This allows gradual migration of existing code
    palette = {
      # Data exports
      inherit (data) user nixbox oc-eyes tailscale;

      # Agent utilities (functions from utils, data from data)
      agents = {
        inherit (utils) mkServers mkCursorMcpConfig mkOpencodeMcpConfig;
        inherit (data.agents) mcpServersPath;
      };

      # Utility functions as lib
      lib = {
        inherit (utils) isDarwin isLinux forAllSystems scanPaths mkOptionalImport mkTailscaleServeService mkPerSystemConfig;
        # Bind mkMagicDnsUrl with magicdns from data
        mkMagicDnsUrl = utils.mkMagicDnsUrl data.tailscale.magicdns;
      };
    };

    # Load overlays from overlays/default.nix
    overlays = (import ./overlays) {inherit palette;};

    # home-manager, nixosSystem, darwinSystem context에서 쓰는 args
    # Note: 'utils' is renamed to 'myUtils' to avoid collision with NixOS internal utils
    mkSpecialArgs = system: {
      inherit
        palette
        data
        inputs
        system
        overlays
        ;
      myUtils = utils;
      pkgs-stable = import inputs.nixpkgs-stable {
        inherit system overlays;
        config.allowUnfree = true;
      };
      isDarwin = utils.isDarwin system;
      isLinux = utils.isLinux system;
      nix-ai-tools-pkgs = inputs.nix-ai-tools.packages.${system};
      ki-editor = inputs.ki-editor.packages.${system}.default;
      opencode = inputs.opencode.packages.${system}.default;
    };

    perSystem = utils.forAllSystems (
      system:
        utils.mkPerSystemConfig {
          pkgs = import nixpkgs {inherit system;};
          inherit system;
          treefmt-nix = inputs.treefmt-nix;
          git-hooks = inputs.git-hooks;
          repoRoot = self;
        }
    );
  in {
    formatter = utils.forAllSystems (
      system: (perSystem.${system}.treefmtEval.config.build.wrapper)
    );
    devShells = utils.forAllSystems (system: {
      default = (perSystem.${system}.pkgs).mkShell {
        shellHook = (perSystem.${system}.preCommit).shellHook;
        packages = with perSystem.${system}.pkgs; [
          nil
          nixfmt-rfc-style
        ];
      };
    });
    checks = utils.forAllSystems (system: {
      pre-commit = perSystem.${system}.preCommit;
      formatting = perSystem.${system}.treefmtEval.config.build.check self;
    });

    apps = utils.forAllSystems (system: {
      format = {
        type = "app";
        program = "${perSystem.${system}.treefmtEval.config.build.wrapper}/bin/treefmt";
      };
      format-check = {
        type = "app";
        program = lib.getExe (
          perSystem.${system}.pkgs.writeShellApplication {
            name = "format-check";
            runtimeInputs = [perSystem.${system}.treefmtEval.config.build.wrapper];
            text = ''
              treefmt --check
            '';
          }
        );
      };
      statix = {
        type = "app";
        program = lib.getExe (
          perSystem.${system}.pkgs.writeShellApplication {
            name = "statix";
            runtimeInputs = [perSystem.${system}.pkgs.statix];
            text = ''
              statix check --format=stderr "$@"
            '';
          }
        );
      };
      deadnix = {
        type = "app";
        program = lib.getExe (
          perSystem.${system}.pkgs.writeShellApplication {
            name = "deadnix";
            runtimeInputs = [perSystem.${system}.pkgs.deadnix];
            text = ''
              deadnix "$@"
            '';
          }
        );
      };
      lint = {
        type = "app";
        program = lib.getExe (
          perSystem.${system}.pkgs.writeShellApplication {
            name = "lint";
            runtimeInputs = [
              perSystem.${system}.pkgs.statix
              perSystem.${system}.pkgs.deadnix
            ];
            text = ''
              echo "Running statix..."
              statix check --format=stderr . || true
              echo ""
              echo "Running deadnix..."
              deadnix --no-progress . || true
            '';
          }
        );
      };
    });

    darwinConfigurations."nyeong-air" = import ./hosts/nyeong-air {
      inherit inputs mkSpecialArgs;
    };

    nixosConfigurations."nixbox" = import ./hosts/nixbox {
      inherit inputs mkSpecialArgs;
    };

    nixosConfigurations."oc-eyes" = import ./hosts/oc-eyes {
      inherit inputs mkSpecialArgs;
    };
  };
}
