{ self, config, pkgs, inputs, ... }: {
  environment.systemPackages = with pkgs; [
  ];
  nix.settings.experimental-features = "nix-command flakes";
  programs.zsh.enable = true;
  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.stateVersion = 5;
  security.pam.enableSudoTouchIdAuth = true;
  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;
  users.users.${config.username} = {
    home = config.home;
  };
}
