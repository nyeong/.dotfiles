# https://github.com/NixOS/nixpkgs/blob/release-25.05/pkgs/by-name/ca/caddy/package.nix
# 본래 caddy 패키지의 소스를 tailscale-caddy로 변경
# https://github.com/tailscale/caddy-tailscale
{
  lib,
  fetchFromGitHub,
  caddy,
  ...
}:
caddy.overrideAttrs (oldAttrs: {
  pname = "caddy-tailscale";
  version = oldAttrs.version;

  src = fetchFromGitHub {
    owner = "tailscale";
    repo = "caddy-tailscale";
    rev = "642f61fea3ccc6b04caf381e2f3bc945aa6af9cc";
    hash = "sha256-oVywoZH7+FcBPP1l+kKjh+deiI6+H/N//phAuiSC4tc=";
  };

  vendorHash = "sha256-eed3AuRhRO66xFg+447xLv7otAHbzAUuhxMcNugZMOA=";

  meta =
    oldAttrs.meta
    // {
      description = oldAttrs.meta.description + " (with Tailscale plugin)";
    };
})
