# .dotfiles

Nix에 모든 것을 맡기는 중...

## 구조

- `flake.nix` : 모두의 entry point
- `hosts/{hostname}/` : 각 시스템에 대한 설정
- `modules/` : 재사용 가능한 설정
- `modules/home/` : home-manager 내에서 재사용 가능한 설정
- `modules/system/` : nixos, nix-darwin 내에서 재사용 가능한 설정
- `modules/darwin/` : nix-darwin 내에서만 재사용 가능한 설정

우선은 한 파일에 몰아넣고, 공통된다면 분리하자!

## Usage

- 포매팅 : `nix format`
- 설정 적용
  - darwin : `sudo darwin-rebuild switch --flake .#nyeong-air`
  - linux : `sudo nixos-rebuild switch --flake .#hostname`

## hosts

- nyeong-air : 맥북에어
- nixbox : 내 홈랩

## 🤔

### Doom Emacs

이걸 어떻게 Nix에 녹여야할까

```
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

## 참고

- [dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
