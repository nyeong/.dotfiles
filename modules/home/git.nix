# https://blog.gitbutler.com/how-git-core-devs-configure-git/
{
  userConfig,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    git
    delta
    gnupg
  ];
  programs.git = {
    enable = true;
    ignores = ["*.swp"];
    userName = userConfig.name;
    userEmail = userConfig.email;
    lfs = {
      enable = true;
    };
    signing = {
      key = "B8BC049D1E042935C003C1D135EF2695DD158D46";
      signByDefault = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
      core = {
        quotepath = false;
        editor = "vim";
        autocrlf = "input";
        pager = "delta";
        fsmonitor = true;
        untractedCache = true;
      };
      commit = {
        gpgsign = true;
        verbose = true;
      };
      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
      };
      branch = {
        sort = "-committerdate";
      };
      tag = {
        sort = "version:refname";
      };
      color = {
        ui = true;
      };
      rerere = {
        enabled = true;
        autoupdate = true;
      };
      push = {
        autoSetupRemote = true;
        followTags = true;
      };
      pull = {
        rebase = true;
      };
      help = {
        autocorrect = "prompt";
      };
      rebase = {
        autoStash = true;
        autoSquash = true;
        updateRefs = true;
      };
      merge = {
        conflictStyle = "zdiff3";
      };
      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        mnemonicPrefix = true;
        renames = true;
      };
      interactive = {
        diffFilter = "delta --color-only";
      };
      delta = {
        navigate = true;
        side-by-side = true;
        line-numbers = true;
      };
    };
  };
}
