# Agent configuration generators (Cursor, OpenCode)
{lib}: rec {
  # Import MCP server definitions with pkgs
  mkServers = pkgs: import ../../data/agents/mcp-servers.nix {inherit pkgs;};

  # Generate Cursor MCP configuration
  # Format: { command: string, args: [...], env: {...} } or { url: string, headers: {...} }
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

  # Generate OpenCode MCP configuration
  # Format: { type: "local", command: [...], environment: {...} } or { type: "remote", url: string, headers: {...} }
  mkOpencodeMcpConfig = pkgs: serverList: let
    servers = mkServers pkgs;
    # Transform Cursor format -> OpenCode format
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
          # Combine command + args into array
          command = [server.command] ++ server.args;
        }
        # env -> environment conversion (only if present)
        // lib.optionalAttrs (server ? env) {environment = server.env;};
  in
    builtins.toJSON {
      mcp = lib.mapAttrs transformServer (
        lib.filterAttrs (name: _: builtins.elem name serverList) servers
      );
    };
}
