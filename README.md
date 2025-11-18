# .dotfiles

Nix에 모든 것을 맡기는 중...

## 구조

KISS, YAGNI. Nix에 너무 심취하지 말기.

### hosts

| hostname   | 용도                                          |
| ---------- | --------------------------------------------- |
| nixvm      | OrbStack으로 nyeong-air 위에 띄우는 개발용 VM |
| nyeong-air | 들고 다니는 MacBook Air M2                    |
| nixbox     | N150 홈랩                                     |
| oc-eyes    | OCI 인스턴스                                  |

세부 내용은 필요한 경우 각 호스트 디렉토리에 README.md로 작성.

```
/hosts/
└── ${host-name}/
    ├── default.nix # entry point
    ├── home-manager.nix
    ├── configuration.nix
    └── hardware-configuration.nix # 있으면
```

그 외에 각 호스트의 설정은 해당 호스트 디렉토리 밑에 편하게 기술하고, 공통으로 쓸 것들만 modules,
home으로 뺸다.

### modules and home

공통으로 쓸 것들.

- `modules`, `home`의 대부분의 코드는 자동으로 import된다.
- `darwin`, `linux`는 시스템에 따라 import된다.
- `features`의 코드는 명시적으로 `features.something.enable = true` 해야 활성화.

```
/home/ # home-manager context configurations
├── base/
├── linux/
├── darwin/
│   ├── ${some-module}.nix  # nix 파일 하나로 끝나는 경우
│   └── ${another-module}/  # 추가 파일이 필요한 경우
│       ├── config/
│       └── default.nix
└── features/               # 자동으로 import되지만 직접 enable 해야하는 것들
    └── dev-tools.nix       # 개발도구 같이 모든 호스트에서 필요한 게 아닌 것들을 여기로
                            # 이 features에 대한 enable은 host에게 맡긴다
/modules/ # nixosSystem or darwinSystem context
├── base/
├── linux/
└── darwin/
```

features는 아래의 패턴을 따른다:

```nix
{ ... }: let
  cfg = config.features.featureName;
in {
  options.features.featureName = {
    enable = lib.mkEnableOption "description";
    # 그 외의 필요한 옵션들
  };

  config = lib.mkIf cfg.enable {
    # 여기에 내용 정의
  };
}
```

### overlays

```
/overlays/
├── emacs.nix
└── ...
```

### palette

각 호스트의 설정도 여기에 넣어서 모든 호스트가 공유할 수 있도록

```
/palette/
├── user-config.nix
└── default.nix # entry point
```

## Naming

- Use kebab-case for file names.
- Use camelCase for function names.

## Usage

- 포매팅 : `nix format`
- 설정 적용
  - nyeong-air : `sudo darwin-rebuild switch --flake .#nyeong-air`

  - nixbox : `sudo nixos-rebuild switch --flake path:.#hostname`
    gitignore한 nix파일도 참조해야하므로 `path`로 지정해야함.

  - nixvm : `sudo nixos-rebuild switch --flake .#nixvm --impure`
    OrbStack에서 정의한 nix 파일이 프로젝트 외부에 있기 때문에 `--impure` 해야함.

## 참고

- [dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
