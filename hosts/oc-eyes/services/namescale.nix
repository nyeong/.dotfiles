# https://github.com/sinanmohd/namescale
{
  palette,
  config,
  ...
}: let
  magicdns = palette.tailscale.magicdns;
in {
  # Secret for Tailscale auth key
  sops.secrets.ts-auth-namescale = {
    sopsFile = ../../../secrets/api-keys.yaml;
    key = "ts-auth-namescale";
    mode = "0400";
    owner = "root";
  };

  services.namescale = {
    enable = true;
    environmentFile = config.sops.secrets.ts-auth-namescale.path;
  };
}
