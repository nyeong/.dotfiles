{...}: {
  services.samba = {
    enable = true;
    settings.global = {
      "workgroup" = "WORKGROUP";
      "server string" = "nixbox";
      "netbios name" = "nixbox";
      "security" = "user";
      "hosts allow" = "100.117.231.90 localhost 127.0.0.1";
      "host deny" = "0.0.0.0/0";
      "getwd cache" = "true";
      "acl map full control" = "yes";
      "acl allow execute always" = "true";
      "strict sync" = "no";
      "sync always" = "no";
      "use sendfile" = "true";
      "read raw" = "yes";
      "write raw" = "yes";
      "min receivefile size" = "16384";
      "aio read size" = "16384";
      "aio write size" = "16384";
    };
    settings."nyeong" = {
      path = "/storage";
      browseable = "yes";
      "read only" = "no";
      "guest ok" = "no";
      "create mask" = "2644";
      "directory mask" = "2755";
      "force user" = "nyeong";
      "force group" = "share";
    };
  };
}
