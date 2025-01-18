{ pkgs, ... }: {
  programs.bat = {
    enable = true;
    config = {
      theme = "Nord";
    };
  };

  programs.zsh.shellAliases = {
    cat = "bat";
  };
}
