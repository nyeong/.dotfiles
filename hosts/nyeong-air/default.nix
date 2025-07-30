{ config, pkgs, userConfig, ... }: {
  imports = [
    ../../modules/system/emacs

    ../../modules/darwin/hammerspoon
    ../../modules/darwin/karabiner

    ../../modules/fonts/monoplex
    ../../modules/fonts/sf-mono
  ];

  users.users.${userConfig.username} = {
    name = "${userConfig.username}";
    home = "/Users/${userConfig.username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };
  };

  homebrew.casks = [
    "discord"
    "tailscale"
    "nextcloud"
    "raycast"
    "orbstack"
    "moonlight"
  ];

  # Mac App Store 앱
  # $ nix run nixpkgs#mas -- search <app name>
  # If you have previously added these apps to your Mac App Store profile (but not installed them on this system),
  # you may receive an error message "Redownload Unavailable with This Apple ID".
  # This message is safe to ignore. (https://github.com/dustinlyons/nixos-config/issues/83)
  homebrew.masApps = {
    "KakaoTalk" = 869223134;
    "Kyobo" = 445290463;
  };

  # Setup user, packages, programs
  nixpkgs.config.allowUnfree = true;

  nix = {
    package = pkgs.nix;
    settings = {
      trusted-users = [
        "@admin"
        "${userConfig.username}"
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

  # System Packages which are only used on this system
  environment.systemPackages = with pkgs; [
  ];

  # sudo 요구 시 TouchId로 패스
  security.pam.services.sudo_local.touchIdAuth = true;
  ids.gids.nixbld = 350;

  networking = {
    hostName = "nyeong-air";
    localHostName = "nyeong-air";
    computerName = "Nyeong's Air";
  };

  # macOS 시스템 설정
  system = {
    checks.verifyNixPath = false;
    stateVersion = 4;
    primaryUser = "${userConfig.username}";
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
}
