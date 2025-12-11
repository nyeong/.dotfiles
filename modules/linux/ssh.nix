{
  palette,
  lib,
  ...
}: {
  services.openssh = lib.mkDefault {
    ports = [22];
    openFirewall = true;
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      KbdInteractiveAuthentication = false;
      AllowTcpForwarding = true;
      PermitOpen = "any";
      GatewayPorts = "no";
      X11Forwarding = false;
      UsePAM = true;
      StrictModes = true;
      LogLevel = "INFO";
      AddressFamily = "any";
    };

    startWhenNeeded = true;
  };
}
