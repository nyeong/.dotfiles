# .dotfiles

Nix에 모든 것을 맡기는 중...

## 구조

- `flake.nix` : 모두의 entry point
- `hosts/{hostname}/default.nix` : 각 호스트에 대한 시스템 설정
- `hosts/{hostname}/home-manager.nix` : 각 호스트에 대한 홈 설정
- `modules/darwin/{module-name}` : darwin에서만 쓰는 모듈
- `modules/home/{module-name}` : home-manager에서 쓰는 모듈
- `modules/fonts/{module-name}` : 폰트

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
