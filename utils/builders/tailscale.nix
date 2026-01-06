# Tailscale helper functions
{lib}: {
  # Create a systemd service that exposes a service via Tailscale Serve
  # Returns a systemd service configuration that can be assigned directly
  # Usage: mkTailscaleServeService { tailscaleBin, serviceName, port, webService, ... }
  mkTailscaleServeService = {
    tailscaleBin,
    serviceName,
    port,
    webService,
    enable ? true,
  }:
    lib.mkIf enable {
      description = "Expose ${serviceName} via Tailscale Serve";
      requires = [
        "tailscaled.service"
        "${webService}"
      ];
      after = [
        "tailscaled.service"
        "${webService}"
      ];
      wantedBy = ["multi-user.target"];
      partOf = [
        "tailscaled.service"
        "${webService}"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = [
          "${tailscaleBin} serve --service=svc:${serviceName} --https=443 127.0.0.1:${toString port}"
        ];
        ExecStop = [
          "${tailscaleBin} serve drain svc:${serviceName}"
          "${tailscaleBin} serve --service=svc:${serviceName} --https=443 off"
          "${tailscaleBin} serve clear svc:${serviceName}"
        ];
      };
    };
}
