{mkMagicDnsUrl, ...}: {
  url = mkMagicDnsUrl "nixbox";
  services =
    (import ./_services.nix)
    // {
      # containers
      archivebox = {
        port = 8001;
      };
      "calibre-web" = {
        port = 8002;
      };
      jellyfin = {
        port = 8003;
      };
      kavita = {
        port = 8004;
      };
      immich = {
        port = 8005;
        publicUrl = "photo.nyeong.me";
      };
      sftpgo = {
        port = 9200;
      };
      paperless = {
        port = 8006;
        dbname = "paperless";
        dbuser = "paperless";
      };

      # monitoring
      vmagent = {
        port = 9091;
      };

      # databases
      postgres = {
        port = 5432;
      };
      valkey = {
        port = 6379;
      };

      # network
      adguard = {
        port = 3000;
      };
      webdav = {
        port = 8880;
      };
    };
}
