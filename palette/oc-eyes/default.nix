{...}: {
  host = "100.77.212.86";
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
      subpath = "status";
    };
    homepage = {
      port = 5000;
      # No subpath - served at root
    };
  };
}
