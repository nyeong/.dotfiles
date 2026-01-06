{
  lib,
  config,
  pkgs,
  palette,
  nix-ai-tools-pkgs,
  ki-editor,
  ...
} @ inputs: let
  cfg = config.features.devTools;

  # 활성화할 MCP 서버 목록
  enabledServers = ["context7" "exa" "filesystem" "github" "git" "fetch"];

  # 설정 파일 생성 (pkgs 전달하여 nix store 경로 사용)
  cursorMcpConfig = palette.agents.mkCursorMcpConfig pkgs enabledServers;
  opencodeMcpConfig = palette.agents.mkOpencodeMcpConfig pkgs enabledServers;

  # OpenCode 설정 파일 경로
  opencodeConfigDir = "${config.home.homeDirectory}/.config/opencode";
  opencodeConfigPath = "${opencodeConfigDir}/opencode.json";
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

    # sops secrets
    sops.secrets.context7_api_key = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "context7_api_key";
    };

    sops.secrets.github_personal_access_token = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "github_personal_access_token";
    };

    sops.secrets.exa_api_key = {
      sopsFile = ../../../secrets/dev-env.yaml;
      key = "exa_api_key";
    };

    # Cursor MCP 설정 (symlink)
    home.file.".cursor/mcp.json" = {
      text = cursorMcpConfig;
    };

    # OpenCode global AGENTS.md
    home.file.".config/opencode/AGENTS.md" = {
      source = ./config/AGENTS.md;
    };

    # zsh에서 MCP용 환경변수 export
    programs.zsh.initContent = ''
      # MCP 서버용 환경변수 (sops secrets에서 로드)
      if [ -f "${config.sops.secrets.context7_api_key.path}" ]; then
        export CONTEXT7_API_KEY=$(cat "${config.sops.secrets.context7_api_key.path}")
      fi
      if [ -f "${config.sops.secrets.exa_api_key.path}" ]; then
        export EXA_API_KEY=$(cat "${config.sops.secrets.exa_api_key.path}")
      fi
      if [ -f "${config.sops.secrets.github_personal_access_token.path}" ]; then
        export GITHUB_PERSONAL_ACCESS_TOKEN=$(cat "${config.sops.secrets.github_personal_access_token.path}")
      fi
    '';

    # OpenCode MCP 설정 (activation으로 교체, 쓰기 권한 유지)
    home.activation.setupOpencodeMcp = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # 디렉토리 생성
      mkdir -p "${opencodeConfigDir}"

      # MCP 설정 JSON
      MCP_CONFIG='${opencodeMcpConfig}'

      if [ -f "${opencodeConfigPath}" ]; then
        # 기존 파일이 있으면 mcp 섹션만 교체 (다른 섹션 유지)
        ${pkgs.jq}/bin/jq --argjson mcp "$MCP_CONFIG" '.mcp = $mcp.mcp' \
          "${opencodeConfigPath}" > "${opencodeConfigPath}.tmp"
        mv "${opencodeConfigPath}.tmp" "${opencodeConfigPath}"
      else
        # 기존 파일이 없으면 새로 생성
        echo "$MCP_CONFIG" | ${pkgs.jq}/bin/jq '.' > "${opencodeConfigPath}"
      fi

      # 권한 설정 (쓰기 가능하도록)
      chmod 0600 "${opencodeConfigPath}"
    '';
  };
}
