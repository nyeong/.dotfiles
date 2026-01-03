{
  lib,
  config,
  pkgs,
  nix-ai-tools-pkgs,
  ki-editor,
  ...
} @ inputs: let
  cfg = config.features.devTools;
  cursorMcpPath = "${config.home.homeDirectory}/.cursor/mcp.json";
  geminiSettingsPath = "${config.home.homeDirectory}/.gemini/settings.json";
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
      nix-ai-tools-pkgs.claude-code
      nix-ai-tools-pkgs.gemini-cli

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
      sopsFile = ../../secrets/dev-env.yaml;
      key = "context7_api_key";
    };

    home.activation.setupCursorMcp = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -f "${cursorMcpPath}" ]; then
        ${pkgs.jq}/bin/jq -n \
          --arg api_key "$(cat ${config.sops.secrets.context7_api_key.path})" \
          '{
            "mcpServers": {
              "context7": {
                "url": "https://mcp.context7.com/mcp",
                "headers": {
                  "CONTEXT7_API_KEY": $api_key
                }
              }
            }
          }' > "${cursorMcpPath}"
        chmod 0600 "${cursorMcpPath}"
      else
        ${pkgs.jq}/bin/jq \
          --arg api_key "$(cat ${config.sops.secrets.context7_api_key.path})" \
          '.mcpServers.context7 = {
            "url": "https://mcp.context7.com/mcp",
            "headers": {
              "CONTEXT7_API_KEY": $api_key
            }
          }' "${cursorMcpPath}" > "${cursorMcpPath}.tmp" && \
        mv "${cursorMcpPath}.tmp" "${cursorMcpPath}"
        chmod 0600 "${cursorMcpPath}"
      fi
    '';

    # Gemini MCP 설정
    home.activation.setupGeminiMcp = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -f "${geminiSettingsPath}" ]; then
        ${pkgs.jq}/bin/jq -n \
          --arg api_key "$(cat ${config.sops.secrets.context7_api_key.path})" \
          '{
            "mcpServers": {
              "context7": {
                "httpUrl": "https://mcp.context7.com/mcp",
                "headers": {
                  "CONTEXT7_API_KEY": $api_key,
                  "Accept": "application/json, text/event-stream"
                }
              }
            }
          }' > "${geminiSettingsPath}"
        chmod 0600 "${geminiSettingsPath}"
      else
        ${pkgs.jq}/bin/jq \
          --arg api_key "$(cat ${config.sops.secrets.context7_api_key.path})" \
          '.mcpServers.context7 = {
            "httpUrl": "https://mcp.context7.com/mcp",
            "headers": {
              "CONTEXT7_API_KEY": $api_key,
              "Accept": "application/json, text/event-stream"
            }
          }' "${geminiSettingsPath}" > "${geminiSettingsPath}.tmp" && \
        mv "${geminiSettingsPath}.tmp" "${geminiSettingsPath}"
        chmod 0600 "${geminiSettingsPath}"
      fi
    '';
  };
}
