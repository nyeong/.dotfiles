# TODO: Doom Emacs 의존성 넣기
# - https://github.com/ryan4yin/nix-config/tree/main/home/base/tui/editors/emacs
# emacs가 느리다면:
# - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83
{
  pkgs,
  lib,
  config,
  ...
}: let
  xdg-data-home = ".local/share";
  xdg-config-home = ".config";
  doomConfigPath = "${config.home.homeDirectory}/.dotfiles/home/base/emacs/config/doom";
in {
  programs.emacs = {
    enable = true;
  };

  home.packages = with pkgs; [
    myTexlive
    emacs-all-the-icons-fonts
    emacs-all-the-icons-fonts
    (aspellWithDicts (d: [
      d.en
      d.sv
    ]))

    # Doom Emacs dependencies
    git
    ripgrep
    fd
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    imagemagick
    ffmpegthumbnailer
    mediainfo
    gnutar
    unzip

    # babel
    d2
  ];

  fonts.fontconfig.enable = true;
  home.sessionPath = ["${config.home.homeDirectory}/${xdg-data-home}/bin"];
  home.file = lib.mkMerge [
    {"${xdg-config-home}/doom".source = config.lib.file.mkOutOfStoreSymlink doomConfigPath;}
    # {
    #   # XDG-only wrappers so Emacs uses ~/.config/emacs (Doom core) as init dir
    #   "${xdg-data-home}/bin/emacs" = {
    #     executable = true;
    #     text = ''
    #       #!/usr/bin/env sh
    #       exec ${my-emacs-with-packages}/bin/emacs --init-directory "${config.home.homeDirectory}/.config/emacs" "$@"
    #     '';
    #   };
    #   "${xdg-data-home}/bin/emacsclient" = {
    #     executable = true;
    #     text = ''
    #       #!/usr/bin/env sh
    #       # TTY vs GUI handling
    #       if [ "x$1" = "x-t" ]; then
    #         exec ${my-emacs-with-packages}/bin/emacsclient -t -a "${my-emacs-with-packages}/bin/emacs --init-directory ${config.home.homeDirectory}/.config/emacs --daemon" "$@"
    #       else
    #         exec ${my-emacs-with-packages}/bin/emacsclient -c -n -a "${my-emacs-with-packages}/bin/emacs --init-directory ${config.home.homeDirectory}/.config/emacs --daemon" "$@"
    #       fi
    #     '';
    #   };
    # }
  ];
}
