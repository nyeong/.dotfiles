# TODO: Doom Emacs 의존성 넣기
# - https://github.com/ryan4yin/nix-config/tree/main/home/base/tui/editors/emacs
# config/doom은 nix로 관리하지 않고 직접 =ln -sf=
# emacs가 느리다면:
# - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83

{
  user,
  pkgs,
  lib,
  config,
}:
let
  # TODO: xdg input으로 주입받기
  xdg-data-home = ".local/share";
  xdg-config-home = ".config";

  emacs-overlay = import (fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/44ee05e41df82bdc7e38186c4b9a740f84ef48c3.tar.gz";
    sha256 = "1m0ry2q9ndvk3kk9k9c74wkf5nq9zbihra5qawhpw49xr456j0gx";
  });

  my-emacs = pkgs.emacs-unstable.override {
    withNativeCompilation = false;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (
    epkgs: with epkgs; [
      vterm
      treesit-grammars.with-all-grammars
    ]
  );

in
{
  # services.emacs.package = my-emacs-with-packages;
  # services.emacs.enable = true;

  nixpkgs.overlays = [ emacs-overlay ];

  # macOS only
  launchd.user.agents.emacs.path = [ config.environment.systemPath ];
  launchd.user.agents.emacs.serviceConfig = {
    KeepAlive = true;
    RunAtLoad = true;
    ProgramArguments = [
      "/bin/sh"
      "-c"
      "/bin/wait4path ${my-emacs-with-packages}/bin/emacs && exec ${my-emacs-with-packages}/bin/emacs --fg-daemon"
    ];
    StandardErrorPath = "/tmp/emacs.err.log";
    StandardOutPath = "/tmp/emacs.out.log";
  };

  home-manager.users.${user} =
    { pkgs, lib, ... }:
    {
      # programs.emacs = {
      # enable = true;
      # package = my-emacs-with-packages;
      # };

      home.packages = with pkgs; [
        # my-emacs-with-packages
        emacs-all-the-icons-fonts
        (aspellWithDicts (d: [
          d.en
          d.sv
        ]))

        # Doom Emacs dependencies
        git
        ripgrep
        fd
        imagemagick
        ffmpegthumbnailer
        mediainfo
        gnutar
        unzip
      ];

      home.activation = {
        linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ -e "$HOME/${xdg-config-home}/doom" ]; then
            $DRY_RUN_CMD rm -f "$HOME/${xdg-config-home}/doom"
          fi
          $DRY_RUN_CMD ln -sf "/Users/nyeong/.dotfiles/packages/emacs/config/doom" "$HOME/${xdg-config-home}/doom"
        '';

        # # macOS용 Emacs.app 심볼릭 링크 생성
        # linkEmacsApp = lib.hm.dag.entryAfter ["writeBoundary"] ''
        #   if [ -d "${my-emacs-with-packages}/Applications/Emacs.app" ]; then
        #     $DRY_RUN_CMD mkdir -p "$HOME/Applications"
        #     $DRY_RUN_CMD ln -sf "${my-emacs-with-packages}/Applications/Emacs.app" "$HOME/Applications/Emacs.app"
        #   fi
        # '';
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
              ${my-emacs-with-packages}/bin/emacsclient -t $@
            else
              # GUI mode
              ${my-emacs-with-packages}/bin/emacsclient -c -n $@
            fi
          '';
        };
      };
    };
}
