{
  palette,
  pkgs,
  ...
}: {
  imports = palette.lib.scanPaths ./.;

  home.packages = with pkgs; [
    age
    age-plugin-yubikey
    gnupg
    libfido2

    # fonts
    noto-fonts
    noto-fonts-color-emoji

    fd
    bat

    # 이정도는 있어야
    ruby
    python3
    nodejs
  ];
}
