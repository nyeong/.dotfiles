# home

`home-manager`가 imports하는 파일들. 아래를 의미한다:

```nix
  inputs.nix-darwin.lib.darwinSystem {
    modules = [
      # ... 생략
      inputs.home-manager.darwinModules.home-manager
      {
        home-manager.users.${username} = {
          imports = [
            ../../home # ./default.nix를 불러온다
          ];
        };
        home-manager.extraSpecialArgs = specialArgs;
      }
    ];
  }
```

## 언제 `home`을 써야하는가?

- 왠만하면 nixosSystem, darwinSystem이 관장하는 `modules` 대신 이 `home`을 사용한다.
- System 밑에 기술해야할 때에는 제외한다.
  - root 권한이 반드시 필요하거나, home-manager에 원하는 게 없거나 등.
- 특정 호스트에서만 필요하다면 제외한다.
  - nixbox에서 돌릴 거라면 `nixbox` 호스트 밑에만 nix 파일을 만들면 된다.

## 단일 파일 vs 디렉토리

- 단일 파일로 끝난다면 `{name}.nix`로 만든다.
- nix 외의 파일이 필요하다면 `{name}/default.nix`를 만들고 `{name}/config` 밑에 필요한 다른 파일들을 위치시킨다.

## Profiles

base, darwin, linux은 프로필이다.

- base : 모든 호스트에서 자동으로 불러와짐
- darwin : darwin 호스트에서 자동으로 불러와짐
- linux : linux 호스트에서 자동으로 불러와짐

모든 호스트에서 필요하다면 위 세 디렉토리 중에 넣으면 된다.

## Features

몇몇 호스트만 공유한다면 features로 만든다. 아래처럼 `options.features.enable` 패턴을 써서 nix 파일을 만든다.

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

추후, 이 feature가 필요한 각 호스트의 `home-manager.nix` 파일에서 `enable = true`하여 사용한다.
