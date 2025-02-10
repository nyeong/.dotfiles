{ user, pkgs }:
let
  xdg_dataHome = ".local/share";
  emacs_overlay_ref = "3e35634650753a5644cf07148cf49df1f023efce";
  emacs_overlay_sha256 = "1jkggd9ypw0g42vx2r0a9fcahrhcvga68igddznxb4rfxaw6wzra";
  emacs_overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${emacs_overlay_ref}.tar.gz";
    sha256 = emacs_overlay_sha256;
});

in
{
  nixpkgs.overlays = [
    emacs_overlay
  ];

  home-manager.users.${user}.home = {
    file = {
      ".config/emacs/init.el" = {
        source = ./config/init.el;
      };

      # Raycast script so that "Run Emacs" is available and uses Emacs daemon
      # darwin only
      "${xdg_dataHome}/bin/emacsclient" = {
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
          # @raycast.icon ${xdg_dataHome}/img/icons/Emacs.icns
          # @raycast.iconDark ${xdg_dataHome}/img/icons/Emacs.icns

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