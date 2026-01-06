# oc-eyes host configuration - pure data
{utils}: let
  mkUrl = utils.mkMagicDnsUrl;
in {
  host = "100.77.212.86";
  url = mkUrl "oc-eyes";
  services = {
    victoria-metrics = {
      port = 9090;
      subpath = "vmdb";
    };
    grafana = {
      port = 3000;
      subpath = "monitor";
    };
    gatus = {
      port = 4000;
      subdomain = "status";
    };
    homepage = {
      port = 5000;
      # No subpath - served at root
    };
    victoria-logs = {
      port = 9428;
      subpath = "logs";
    };
  };
}
