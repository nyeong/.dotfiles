{ user, name, email, pkgs }:
{
  home-manager.users.${user} = {
    home.packages = with pkgs; [ git delta ];
    programs.git = {
      enable = true;
      ignores = [ "*.swp" ];
      userName = name;
      userEmail = email;
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
          editor = "vim";
          autocrlf = "input";
          pager = "delta";
        };
        commit = {
          gpgsign = true;
          verbose = true;
        };
        color = {
          ui = true;
        };
        rerere = {
          enabled = true;
        };
        pull = {
          rebase = true;
        };
        rebase = {
          autoStash = true;
        };
        merge = {
          conflictStyle = "zdiff3";
        };
        diff = {
          algorithm = "histogram";
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
  };
}