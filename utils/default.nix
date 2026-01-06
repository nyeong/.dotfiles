# Utils - Pure functions for building configurations
# Layer 4: No dependencies on other layers (except lib)
{
  lib,
  systems,
}: let
  builders = import ./builders {inherit lib;};
  agents = import ./agents {inherit lib;};
in {
  # Platform detection
  isDarwin = system: lib.strings.hasSuffix "-darwin" system;
  isLinux = system: lib.strings.hasSuffix "-linux" system;

  # System iteration
  forAllSystems = func: (lib.genAttrs systems func);

  # Builder functions
  inherit (builders) mkMagicDnsUrl scanPaths mkOptionalImport mkTailscaleServeService mkPerSystemConfig;

  # Agent configuration generators
  inherit (agents) mkServers mkCursorMcpConfig mkOpencodeMcpConfig;
}
