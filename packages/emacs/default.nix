{ user, pkgs }:
let
  xdg-data-home = ".local/share";
  emacs-overlay-ref = "3e35634650753a5644cf07148cf49df1f023efce";
  emacs-overlay-sha256 = "1jkggd9ypw0g42vx2r0a9fcahrhcvga68igddznxb4rfxaw6wzra";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${emacs-overlay-ref}.tar.gz";
    sha256 = emacs-overlay-sha256;
  });

in
{
  nixpkgs.overlays = [
    emacs-overlay
  ];

  services.emacs.package = pkgs.emacs-unstable;

  home-manager.users.${user}.home = {
    file = {
      ".config/emacs/init.el" = {
        source = ./config/init.el;
      };

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