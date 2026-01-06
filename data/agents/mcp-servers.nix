# MCP Server definitions
# Used by Cursor, OpenCode, and other LLM agents
{pkgs}: let
  npx = "${pkgs.nodejs}/bin/npx";
  uvx = "${pkgs.uv}/bin/uvx";
in {
  grep = {
    type = "remote";
    url = "https://mcp.grep.app";
  };

  context7 = {
    command = npx;
    args = [
      "-y"
      "@upstash/context7-mcp"
    ];
    env.CONTEXT7_API_KEY = "\${env:CONTEXT7_API_KEY}";
  };

  exa = {
    type = "remote";
    url = "https://mcp.exa.ai/mcp";
  };

  filesystem = {
    command = npx;
    args = [
      "-y"
      "@modelcontextprotocol/server-filesystem"
      "\${userHome}"
    ];
  };

  github = {
    type = "remote";
    url = "https://api.githubcopilot.com/mcp/";
    headers.Authorization = "Bearer \${env:GITHUB_PERSONAL_ACCESS_TOKEN}";
  };

  git = {
    command = uvx;
    args = ["mcp-server-git"];
  };

  fetch = {
    command = uvx;
    args = ["mcp-server-fetch"];
  };
}
