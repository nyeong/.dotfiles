# MCP 서버 정의
# Cursor, OpenCode 등 LLM Agent에서 사용하는 MCP를 정의합니다.
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
    command = npx;
    args = [
      "-y"
      "exa-mcp-server"
    ];
    env.EXA_API_KEY = "\${env:EXA_API_KEY}";
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
