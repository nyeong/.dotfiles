{
  mkMagicDnsUrl,
  lib,
  ...
}: {
  url = mkMagicDnsUrl "nixbox";
  services = lib.mkOptionalImport "services" ./. {
    # services
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
      port = 5000;
      user = "kavita";
      subpath = "kavita";
      url = mkMagicDnsUrl "kavita";
      serviceName = "kavita";
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
      url = mkMagicDnsUrl "paperless";
      serviceName = "paperless";
    };
    filebrowser = {
      port = 8080;
      subpath = "filebrowser";
      root = "/var/lib/filebrowser/data";
      baseDir = "/var/lib/filebrowser";
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
      user = "redis";
      group = "redis";
      dbnumber = {
        paperless = 1;
        paperless-read = 2;
      };
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
