# .dotfiles

Nix에 모든 것을 맡기는 중...

## 구조

### hosts

- nix : OrbStack으로 띄우는 NixOS VM
- nyeong-air : MacBook Air
- nixbox : 홈랩

```
/hosts/
└── ${host-name}/
    ├── default.nix
    ├── home-manager.nix
    ├── configuration.nix
    └── hardware.nix
```

### modules and home

```
/home/ # home-manager context configurations
├── base/
├── linux/
├── darwin/
│   ├── ${some-module}.nix  # nix 파일 하나로 끝나는 경우
│   └── ${another-module}/  # 추가 파일이 필요한 경우
│       ├── config/
│       └── default.nix
└── profiles/
    └── dev-tools.nix
/modules/
├── base/
├── linux/
└── darwin/
```

### overlays

```
/overlays/
└── emacs.nix
```

### palette

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
  - nix : `sudo nixos-rebuild switch --flake .#hostname --impure`

## 참고

- [dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
