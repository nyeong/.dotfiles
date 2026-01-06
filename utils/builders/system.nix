# System configuration helper functions
{
  # Generate per-system configuration with formatting and git hooks
  mkPerSystemConfig = {
    pkgs,
    system,
    git-hooks,
    treefmt-nix,
    repoRoot,
  }: let
    treefmtEval = treefmt-nix.lib.evalModule pkgs {
      projectRootFile = "flake.nix";
      programs = {
        alejandra.enable = true; # nix
        stylua.enable = true; # lua
        shfmt.enable = true; # shell
        prettier.enable = true; # json/markdown/yaml/etc.
      };
    };
    preCommit = git-hooks.lib.${system}.run {
      src = ./.;
      hooks.treefmt = {
        enable = true;
        package = treefmtEval.config.build.wrapper;
      };
      hooks.statix-warn = {
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
      hooks.deadnix-warn = {
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
  in {
    inherit pkgs treefmtEval preCommit;
  };
}
