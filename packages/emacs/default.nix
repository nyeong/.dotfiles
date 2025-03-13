# TODO: Doom Emacs 의존성 넣기
# - https://github.com/ryan4yin/nix-config/tree/main/home/base/tui/editors/emacs
# config/doom은 nix로 관리하지 않고 직접 =ln -sf=
# emacs가 느리다면:
# - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83

{ user, pkgs, lib }:
let
  # TODO: xdg input으로 주입받기
  xdg-data-home = ".local/share";
  xdg-config-home = ".config";

in {
  home-manager.users.${user} = { pkgs, lib, ... }: {
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

    home.activation = {
      linkDoomConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
        if [ -e "$HOME/${xdg-config-home}/doom" ]; then
          $DRY_RUN_CMD rm -f "$HOME/${xdg-config-home}/doom"
        fi
        $DRY_RUN_CMD ln -sf "/Users/nyeong/.dotfiles/packages/emacs/config/doom" "$HOME/${xdg-config-home}/doom"
      '';
    };

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
