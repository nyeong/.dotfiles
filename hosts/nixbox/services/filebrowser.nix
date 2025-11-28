{palette, ...}: let
  filebrowser = palette.nixbox.services.filebrowser;
  url = palette.nixbox.url;
in {
  services.filebrowser = {
    enable = true;
    settings = {
      port = filebrowser.port;
      baseUrl = "/${filebrowser.subpath}";
      # 왠지는 모르겠는데?? 아래 값이 readonly로 설정되어있음
      # root = filebrowser.root;
      # cache-dir = filebrowser.baseDir;
    };
  };
}
