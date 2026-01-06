# Agent-related data (MCP server definitions)
{
  # Re-export mcp-servers.nix
  # Note: mcp-servers.nix requires pkgs, so it's imported where needed
  mcpServersPath = ./mcp-servers.nix;
}
