{
  lib,
  config,
  pkgs,
  nix-ai-tools-pkgs,
  ...
}: let
  cfg = config.features.devTools;
in {
  options.features.devTools = {
    enable = lib.mkEnableOption "Dev tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # ai tools
      nix-ai-tools-pkgs.cursor-agent
      nix-ai-tools-pkgs.claude-code
      nix-ai-tools-pkgs.gemini-cli

      # editor
      code-cursor

      # dev
      openssh
      coreutils
      lsd
      wget
      curl

      # languages
      elixir
      python3
      ruby
    ];
  };
}
