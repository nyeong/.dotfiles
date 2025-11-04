{pkgs, ...}: {
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-color-emoji
    sf-mono-liga-bin # via overlay
    monoplex-bin # via overlay
  ];
}
