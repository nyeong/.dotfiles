{ self, userConfig, pkgs, inputs, ... }: {
  environment.systemPackages = with pkgs; [];

  nix.settings.experimental-features = "nix-command flakes";
  nix.useDaemon = true;

  programs.zsh.enable = true;

  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.stateVersion = 5;

  security.pam.enableSudoTouchIdAuth = true;

  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;

  users.users.${userConfig.username} = {
    home = userConfig.home;
  };

  fonts.packages = with pkgs; [
    fira-code
    nerd-fonts.jetbrains-mono
    nerd-fonts.fira-code
  ];
}
