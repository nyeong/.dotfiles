# .dotfiles

Nix에 모든 것을 맡기는 중...

## 디렉토리 구조

- `hosts` : 각 환경에 대한 기본적인 설정을 합니다.
- `modules` : 각 환경의 필수적인 모듈에 대한 설정을 합니다. (예: home-manager 등)
- `overlays` : 지울 예정
- `packages` : 설치할 패키지에 대한 내용 정의.
- `packages/<패키지 이름>/default.nix` : 설정파일과 같이 파일 의존성이 있거나, overlay와 같이 derivation 작성이 필요한 패키지는 디렉토리를 만들어 관리.
- `packages/shared.nix` : packages에 별도로 만든 패키지들 로드. 별도 설정 등이 필요 없는 패키지들은 여기서 정의.
- `packages/darwin.nix` : shared.nix를 불러오고, darwin에서만 필요한 패키지도 정의.

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
