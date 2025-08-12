# TODO: Doom Emacs 의존성 넣기
# - https://github.com/ryan4yin/nix-config/tree/main/home/base/tui/editors/emacs
# emacs가 느리다면:
# - https://discourse.doomemacs.org/t/why-is-emacs-doom-slow/83
{
  userConfig,
  pkgs,
  lib,
  config,
  ...
}: let
  isLinux = pkgs.stdenv.isLinux;
  isDarwin = pkgs.stdenv.isDarwin;

  # TODO: xdg input으로 주입받기
  xdg-data-home = ".local/share";
  xdg-config-home = ".config";

  emacs-overlay = import (fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/44ee05e41df82bdc7e38186c4b9a740f84ef48c3.tar.gz";
    sha256 = "1m0ry2q9ndvk3kk9k9c74wkf5nq9zbihra5qawhpw49xr456j0gx";
  });

  chosenEmacsBase =
    if pkgs ? emacs-unstable
    then pkgs.emacs-unstable
    else pkgs.emacs;
  chosenEmacs =
    if pkgs ? emacs-unstable
    then
      chosenEmacsBase.override {
        withNativeCompilation = false;
        withSQLite3 = true;
        withTreeSitter = true;
        withWebP = true;
      }
    else chosenEmacsBase;

  my-emacs-with-packages = (pkgs.emacsPackagesFor chosenEmacs).emacsWithPackages (
    epkgs:
      with epkgs; [
        vterm
        treesit-grammars.with-all-grammars
      ]
  );
in {
  # Overlay must be set at the system level; safe on both darwin and linux
  nixpkgs.overlays = [emacs-overlay];

  home-manager.users.${userConfig.username} = {
    pkgs,
    lib,
    config,
    ...
  }: let
    doomConfigPath = "${config.home.homeDirectory}/.dotfiles/modules/system/emacs/config/doom";
  in {
    programs.emacs = {
      enable = true;
      package = my-emacs-with-packages;
    };

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

      # babel
      d2
    ];

    # Deploy Doom config and optional macOS helper in one merged attrset
    home.file = lib.mkMerge [
      {
        "${xdg-config-home}/doom".source = config.lib.file.mkOutOfStoreSymlink doomConfigPath;
      }
      (lib.optionalAttrs isDarwin {
        "${xdg-data-home}/bin/emacsclient" = {
          executable = true;
          text = ''
            #!/bin/zsh
            if [[ $1 = "-t" ]]; then
              ${my-emacs-with-packages}/bin/emacsclient -t $@
            else
              ${my-emacs-with-packages}/bin/emacsclient -c -n $@
            fi
          '';
        };
      })
    ];
  };
}
