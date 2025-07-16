{
  user,
  pkgs,
  lib,
  config,
}:
{
  home-manager.users.${user} =
    { pkgs, lib, ... }:
    {
      home.packages = with pkgs; [
        bun
        nodejs_24
        deno
        pnpm
        biome
      ];
    };
}
