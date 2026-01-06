# home

호스트별 home-manager 프로필.

## 구조

```
/home/
├── profiles/         # 호스트별 프로필
│   ├── nyeong-air.nix
│   ├── nixbox.nix
│   └── oc-eyes.nix
└── default.nix       # 하위 호환성 (modules/home을 re-export)
```

## 프로필

각 호스트는 `profiles/` 디렉토리에 자신만의 프로필을 가집니다.

```nix
# home/profiles/nyeong-air.nix
{...}: {
  imports = [
    ../../modules/home
  ];

  features.devTools.enable = true;
  features.syncthing = {
    enable = true;
    folders = { ... };
  };
}
```

호스트의 `home-manager.nix`에서 해당 프로필을 import합니다:

```nix
# hosts/nyeong-air/home-manager.nix
{...}: {
  imports = [
    ../../home/profiles/nyeong-air.nix
  ];
}
```

## 모듈 (modules/home)

공유되는 home-manager 모듈은 `modules/home/`에 위치합니다:

- `modules/home/base/`: 모든 호스트에서 자동으로 불러와짐
- `modules/home/darwin/`: darwin 호스트에서 자동으로 불러와짐
- `modules/home/linux/`: linux 호스트에서 자동으로 불러와짐
- `modules/home/features/`: 선택적 기능 (mkEnableOption)

## Features

몇몇 호스트만 공유하는 기능은 `modules/home/features/`에 `mkEnableOption` 패턴으로 작성합니다:

```nix
{ config, lib, pkgs, ...  }: let
  cfg = config.features.{feature-name};
in {
  options.features.{feature-name} = {
    enable = lib.mkEnableOption "description";
  };

  config = lib.mkIf cfg.enable {
    # ...
  };
}
```

호스트 프로필에서 `features.{feature-name}.enable = true`로 활성화합니다.

## 언제 modules/home을 써야하는가?

- 왠만하면 nixosSystem, darwinSystem이 관장하는 `modules/system` 대신 `modules/home`을 사용합니다.
- System 밑에 기술해야할 때에는 제외합니다:
  - root 권한이 반드시 필요하거나, home-manager에 원하는 게 없거나 등.

## 단일 파일 vs 디렉토리

- 단일 파일로 끝난다면 `{name}.nix`로 만듭니다.
- nix 외의 파일이 필요하다면 `{name}/default.nix`를 만들고 `{name}/config/` 밑에 필요한 다른 파일들을 위치시킵니다.
