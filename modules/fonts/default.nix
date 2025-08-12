{pkgs, ...}: {
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-emoji
  ];
}
