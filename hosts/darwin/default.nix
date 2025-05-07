{
  agenix,
  config,
  pkgs,
  ...
}:

let
  user = "nyeong";

in
{
  imports = [
    ../../modules/darwin/secrets.nix
    ../../modules/darwin/home-manager.nix
    ../../modules/shared
    agenix.darwinModules.default
  ];

  # Setup user, packages, programs
  nix = {
    package = pkgs.nix;
    settings = {
      trusted-users = [
        "@admin"
        "${user}"
      ];
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
      ];
      trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
    };

    gc = {
      automatic = true;
      interval = {
        Weekday = 0;
        Hour = 2;
        Minute = 0;
      };
      options = "--delete-older-than 30d";
    };

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  # Load configuration that is shared across systems
  environment.systemPackages = with pkgs; [ agenix.packages."${pkgs.system}".default ];

  # sudo 요구 시 TouchId로 패스
  security.pam.services.sudo_local.touchIdAuth = true;
  ids.gids.nixbld = 350;

  networking = {
    hostName = "nyeong-air";
  };

  system = {
    stateVersion = 4;

    defaults = {

      screensaver.askForPassword = true;
      screensaver.askForPasswordDelay = 0;
      CustomUserPreferences = {
        "com.apple.symbolichotkeys" = {
          AppleSymbolicHotKeys = {
            # 언어 변경을 shift + space로
            "60" = {
              enabled = true;
              value.parameters = [
                32
                49
                131072
              ];
              value.type = "standard";
            };
            # Spotlight Search
            "64" = {
              enabled = false;
            };
          };
        };
      };
      NSGlobalDomain = {
        AppleICUForce24HourTime = true;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
        AppleShowAllExtensions = true;
        ApplePressAndHoldEnabled = false;
        # NSToolbarTitleViewRolloverDelay = 0;

        # 키 반복 속도
        # 120, 90, 60, 30, 12, 6, 2
        KeyRepeat = 2;

        # 반복 지연 시간
        # 120, 94, 68, 35, 25, 15
        InitialKeyRepeat = 15;

        # "com.apple.Preview.NSWindowSupportsAutomaticInlineTitle" = false;
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = true;
        show-recents = false;
        launchanim = true;
        orientation = "left";
        tilesize = 24;
      };

      finder = {
        _FXShowPosixPathInTitle = false;
      };

      trackpad = {
        Clicking = false;

        # 트랙패드에서 세 손가락으로 끌기
        # 짱입니다 꼭 쓰세요
        TrackpadThreeFingerDrag = true;
      };
    };
  };

  # TODO: zsh에 의존하는 건 zsh에서 isDarwin으로 분기하기
  programs.zsh = {
    enableGlobalCompInit = false;
  };
}
