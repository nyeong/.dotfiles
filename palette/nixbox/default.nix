{
  mkMagicDnsUrl,
  lib,
  ...
}: {
  url = mkMagicDnsUrl "nixbox";
  services = lib.mkOptionalImport "services" ./. {
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
      url = mkMagicDnsUrl "paperless";
      servieName = "paperless";
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
