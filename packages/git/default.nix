{ userConfig, pkgs, ... }: {
  home.packages = with pkgs; [
    delta
  ];

  programs.git = {
    enable = true;
    userName = userConfig.fullname;
    userEmail = userConfig.email;

    extraConfig = {
      init.defaultBranch = "main";
      core = {
        editor = "nvim";
        autocrlf = "input";
        pager = "delta";
        excludesfile = ./config/gitignore;
      };
      color.ui = "auto";
      pull.ff = "only";
      rebase.autostash = true;
      merge.conflictstyle = "zdiff3";
      push.autoSetupRemote = true;
      push.default = "current";
      commit.verbose = true;
      rerere.enabled = true;
      diff.algorithm = "histogram";
    };

    # 유용한 alias들
    aliases = {
      st = "status";
      ci = "commit";
      co = "checkout";
      br = "branch";
      unstage = "reset HEAD --";
      last = "log -1 HEAD";
    };

    # vim 사용자를 위한 설정
    delta = {
      enable = true;
      package = pkgs.writeScriptBin "delta" ''
        if defaults read -g AppleInterfaceStyle &> /dev/null; then
          ${pkgs.delta}/bin/delta "$@"
        else
          ${pkgs.delta}/bin/delta --light "$@"
        fi
      '';
      options = {
        navigate = true;
        line-numbers = true;
        syntax-theme = "GitHub";
      };
    };
  };
}
