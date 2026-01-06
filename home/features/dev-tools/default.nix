{
  lib,
  config,
  pkgs,
  nix-ai-tools-pkgs,
  ki-editor,
  ...
} @ inputs: let
  cfg = config.features.devTools;
in {
  options.features.devTools = {
    enable = lib.mkEnableOption "Dev tools";
  };

  config = lib.mkIf cfg.enable {
    # Enable emacs when devTools is enabled
    features.emacs.enable = true;

    home.packages = with pkgs; [
      # ai tools
      nix-ai-tools-pkgs.cursor-agent

      # Cursor ACP 이용을 위해 패치 커밋 이용
      # 나중에 머지되면 pkgs.opencode 이용
      inputs.opencode
      ki-editor

      # editor
      code-cursor

      # dev
      openssh
      coreutils
      lsd
      wget
      curl
      jq # JSON 병합을 위해 필요

      # languages
      elixir
      python3
      ruby
      metals # scala lsp
    ];

    sops.secrets.context7_api_key = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "context7_api_key";
    };

    sops.secrets.github_personal_access_token_for_mcp = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "github_personal_access_token_for_mcp";
    };

    sops.secrets.exa_api_key = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "exa_api_key";
    };
  };
}
