# tailscale
{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs; [
    tailscale
  ];

  services.tailscale = {
    enable = true;
  };

  networking.firewall = {
    trustedInterfaces = ["tailscale0"];
    allowedTCPPorts = [22];
    allowedUDPPorts = [config.services.tailscale.port];
  };
}
