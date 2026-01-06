# Agent 설정 생성 함수 (Cursor, OpenCode)
{lib}: rec {
  # pkgs를 받아서 servers 생성
  mkServers = pkgs: import ./mcp-servers.nix {inherit pkgs;};

  # Cursor용: mcpServers 키 사용
  # 형식: { command: string, args: [...], env: {...} } 또는 { url: string, headers: {...} }
  mkCursorMcpConfig = pkgs: serverList: let
    servers = mkServers pkgs;
    transformCursorServer = name: server:
      if server ? type && server.type == "remote"
      then
        {
          url = server.url;
        }
        // lib.optionalAttrs (server ? headers) {headers = server.headers;}
      else server;
  in
    builtins.toJSON {
      mcpServers = lib.mapAttrs transformCursorServer (
        lib.filterAttrs (name: _: builtins.elem name serverList) servers
      );
    };

  # OpenCode용: mcp 키 + 형식 변환
  # 형식: { type: "local", command: [...], environment: {...} } 또는 { type: "remote", url: string, headers: {...} }
  mkOpencodeMcpConfig = pkgs: serverList: let
    servers = mkServers pkgs;
    # Cursor 형식 → OpenCode 형식 변환
    transformServer = name: server:
      if server ? type && server.type == "remote"
      then
        {
          type = "remote";
          url = server.url;
        }
        // lib.optionalAttrs (server ? headers) {headers = server.headers;}
      else
        {
          type = "local";
          # command + args를 배열로 합침
          command = [server.command] ++ server.args;
          # env → environment로 변환 (있는 경우만)
        }
        // lib.optionalAttrs (server ? env) {environment = server.env;};
  in
    builtins.toJSON {
      mcp = lib.mapAttrs transformServer (
        lib.filterAttrs (name: _: builtins.elem name serverList) servers
      );
    };
}
