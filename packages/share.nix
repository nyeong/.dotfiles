{
  pkgs,
  lib,
  config,
  ...
}:
let
  user = "nyeong";
  name = "An Nyeong";
  email = "me@annyeong.me";

  # TODO: 매번 inherit user pkgs 하지 않을 방법은 없을까?
  packages = [
    # terminal
    (import ./zsh { inherit user pkgs lib; })
    (import ./zellij { inherit user pkgs; })
    (import ./git {
      inherit
        user
        name
        email
        pkgs
        ;
    })

    # editor
    (import ./emacs {
      inherit
        user
        pkgs
        lib
        config
        ;
    })

    # font
    (import ./sf-mono { inherit user pkgs; })
    (import ./monoplex { inherit user pkgs; })

    # lang
    (import ./nix { inherit pkgs; })
  ];

  # home-manager로 설치할 프로그램
  home-packages = with pkgs; [
    # General packages for development and system management
    wezterm
    bash-completion
    bat
    btop
    bottom
    lsd
    coreutils
    killall
    neofetch
    openssh
    sqlite
    wget
    hyperfine
    curl
    zip
    yt-dlp
    tokei

    zinit

    # Encryption and security tools
    age
    age-plugin-yubikey
    gnupg
    libfido2

    # gcc
    # libgccjit

    # Media-related packages
    emacs-all-the-icons-fonts
    dejavu_fonts
    ffmpeg
    fd
    font-awesome
    hack-font
    noto-fonts
    noto-fonts-emoji
    meslo-lgs-nf

    aider-chat

    # Node.js development tools
    nodePackages.npm # globally install npm
    nodejs

    # Text and terminal utilities
    htop
    hunspell
    iftop
    jetbrains-mono
    jq
    ripgrep
    tree
    tmux
    unrar
    unzip
    zsh-powerlevel10k

    anki-bin
    zotero

    python3
    elixir
    ruby
    playwright
    playwright-driver.browsers

    # haskell
    haskell.packages.ghc98.ghc
    haskell.packages.ghc98.haskell-language-server
    haskell.packages.ghc98.stack
  ];
in
{
  imports = packages;

  home-manager.users.${user}.home = {
    packages = home-packages;
  };
}
