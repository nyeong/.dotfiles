# Clean Code Refactor Tasks

**Last Updated**: 2026-01-06

## Phase 1: modules/base 분리 (SRP)

### 1.1 새 파일 생성

- [ ] `modules/base/nix-settings.nix` 생성

  ```nix
  { pkgs, palette, lib, ... }: {
    nix.package = pkgs.nix;
    nix.optimise.automatic = true;
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-users = ["${palette.user.username}"]
        ++ lib.optionals pkgs.stdenv.isDarwin ["@admin"]
        ++ lib.optionals pkgs.stdenv.isLinux ["root"];
      substituters = [...];
      trusted-public-keys = [...];
      builders-use-substitutes = true;
    };
    nix.gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
  }
  ```

- [ ] `modules/base/overlays.nix` 생성

  ```nix
  { overlays, ... }: {
    nixpkgs.overlays = overlays;
    nixpkgs.config.allowUnfree = true;
  }
  ```

- [ ] `modules/base/locale.nix` 생성
  ```nix
  { ... }: {
    time.timeZone = "Asia/Seoul";
  }
  ```

### 1.2 기존 파일 수정

- [ ] `modules/base/sops.nix` 확장
  - github_fetch_token secret 추가
  - nix-access-tokens 템플릿 추가
  - environment.etc."nix/nix.conf.d/access-tokens.conf" 추가

- [ ] `modules/base/default.nix` 정리
  - imports = palette.lib.scanPaths ./.; 만 남김
  - 나머지 모든 설정 제거 (새 파일로 이동됨)

### 1.3 검증

- [ ] `nix flake check`
- [ ] `nix run .#lint`
- [ ] `nix run .#format`

---

## Phase 2: home/base 패키지 분리 (ISP)

### 2.1 새 feature 생성

- [ ] `home/features/dev-lang.nix` 생성

  ```nix
  { lib, config, pkgs, ... }: let
    cfg = config.features.devLang;
  in {
    options.features.devLang = {
      enable = lib.mkEnableOption "Development language runtimes (ruby, python, node)";
    };

    config = lib.mkIf cfg.enable {
      home.packages = with pkgs; [
        ruby
        python3
        nodejs
      ];
    };
  }
  ```

### 2.2 기존 파일 수정

- [ ] `home/features/default.nix` 수정
  - `./dev-lang.nix` import 추가 (또는 scanPaths가 자동으로 처리)

- [ ] `home/base/default.nix` 수정
  - ruby, python3, nodejs 제거
  - 남는 패키지: age, sops, age-plugin-yubikey, gnupg, libfido2, noto-fonts, noto-fonts-color-emoji, fd, bat

- [ ] `hosts/nyeong-air/home-manager.nix` 수정
  - `features.devLang.enable = true;` 추가

### 2.3 검증

- [ ] `nix flake check`
- [ ] `darwin-rebuild build --flake .#nyeong-air`
- [ ] `nix run .#lint`
- [ ] `nix run .#format`

---

## Post-Refactor

- [ ] 최종 `nix run .#format` 실행
- [ ] 최종 `nix run .#lint` 실행
- [ ] 커밋: "refactor: apply SRP and ISP to modules and home"
- [ ] (선택) `darwin-rebuild switch --flake .#nyeong-air` 적용

---

## Notes

### scanPaths 동작

`palette.lib.scanPaths`는 디렉토리 내 모든 `.nix` 파일과 서브디렉토리를 자동 import함.
`default.nix`는 제외됨.

따라서:

- Phase 1에서 새 파일 생성하면 자동으로 import됨
- Phase 2에서 `home/features/dev-lang.nix` 생성하면 자동으로 import됨

### features.devTools vs features.devLang

현재 `features.devTools`에도 python3, ruby가 있음 (`home/features/dev-tools/default.nix`).

**옵션**:

1. devLang 별도 유지 (세밀한 제어)
2. devTools 활성화 시 devLang도 자동 활성화 (`features.devLang.enable = lib.mkDefault true;`)

일단 별도 유지하고, 중복은 나중에 정리.
