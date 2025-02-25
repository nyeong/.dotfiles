# TODO: Doom Emacs 의존성 넣기
# - https://github.com/ryan4yin/nix-config/tree/main/home/base/tui/editors/emacs
# emacs가 느리다면:
# - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83

{ user, pkgs }:
let
  # TODO: xdg input으로 주입받기
  xdg-data-home = ".local/share";
  emacs-overlay-ref = "3e35634650753a5644cf07148cf49df1f023efce";
  emacs-overlay-sha256 = "1jkggd9ypw0g42vx2r0a9fcahrhcvga68igddznxb4rfxaw6wzra";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${emacs-overlay-ref}.tar.gz";
    sha256 = emacs-overlay-sha256;
  });
  doom-emacs = builtins.fetchGit {
    url = "https://github.com/hlissner/doom-emacs";
    rev = "2bc052425ca45a41532be0648ebd976d1bd2e6c1";
  };

in
{
  home-manager.users.${user} = {
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    home.packages = with pkgs; [
      # Doom Emacs dependencies
      git
      ripgrep
      fd
      imagemagick
      ffmpegthumbnailer
      mediainfo
      gnutar
      unzip

      # vterm depedency
      gcc
      cmake
    ];

    home.file = {
      # Raycast script so that "Run Emacs" is available and uses Emacs daemon
      # darwin only
      # TODO: isDarwin일 때에만 생성하도록 변경 필요
      "${xdg-data-home}/bin/emacsclient" = {
        executable = true;
        text = ''
          #!/bin/zsh
          #
          # Required parameters:
          # @raycast.schemaVersion 1
          # @raycast.title Run Emacs
          # @raycast.mode silent
          #
          # Optional parameters:
          # @raycast.packageName Emacs
          # @raycast.icon ${xdg-data-home}/img/icons/Emacs.icns
          # @raycast.iconDark ${xdg-data-home}/img/icons/Emacs.icns

          if [[ $1 = "-t" ]]; then
            # Terminal mode
            ${pkgs.emacs}/bin/emacsclient -t $@
          else
            # GUI mode
            ${pkgs.emacs}/bin/emacsclient -c -n $@
          fi
        '';
      };
    };
  };
}
