{
  palette,
  pkgs,
  inputs,
  ...
}: let
  username = palette.user.username;
in {
  imports = palette.lib.scanPaths ./.;

  # sudo 요구 시 TouchId로 패스
  security.pam.services.sudo_local.touchIdAuth = true;

  nix-homebrew = {
    enable = true;
    # enableRosetta = true;
    user = username;
    taps = {
      "homebrew/homebrew-core" = inputs.homebrew-core;
      "homebrew/homebrew-cask" = inputs.homebrew-cask;
    };

    mutableTaps = false;
  };

  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "zap";
    };
    casks = [
      "karabiner-elements"
      "discord"
      "tailscale-app"
      "raycast"
      "orbstack"
    ];
  };

  # macOS 시스템 설정
  system = {
    checks.verifyNixPath = false;
    stateVersion = 4;
    primaryUser = "${palette.user.username}";
    defaults = {
      screensaver.askForPassword = true;
      screensaver.askForPasswordDelay = 0;
      CustomUserPreferences = {
        # OrbStack: set default terminal to WezTerm via nix-darwin
        "dev.orbstack.gui" = {
          "DefaultTerminal" = "${pkgs.wezterm}/Applications/WezTerm.app";
        };
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

  nix.gc.interval = {
    Weekday = 0;
    Hour = 0;
    Minute = 0;
  };
}
