{
  userConfig,
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ../../modules/home/git.nix
    ../../modules/home/wezterm.nix
    ../../modules/home/ssh.nix
    ../../modules/home/zellij
    ../../modules/home/zsh
    ../../modules/home/javascript
    ../../modules/home/syncthing
  ];

  home.file."${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict".text = ''
    {
        "₩" = ("insertText:", "`");
    }
  '';

  home.username = userConfig.username;
  home.homeDirectory = "/Users/${userConfig.username}";
  home.stateVersion = "25.11";
  home.packages = with pkgs; [
    claude-code
    gemini-cli
    code-cursor

    # General packages for development and system management
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
    xz
    ripgrep

    ripgrep

    firefox

    dbeaver-bin
    postgresql

    zinit

    # Encryption and security tools
    age
    age-plugin-yubikey
    gnupg
    libfido2

    # gcc
    # libgccjit

    # Media-related packages
    dejavu_fonts
    ffmpeg
    fd
    font-awesome
    hack-font
    noto-fonts
    noto-fonts-emoji
    meslo-lgs-nf

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
    elixir-ls
    ruby
    playwright-driver.browsers
  ];
}
