# Clean Code Refactor Context

**Last Updated**: 2026-01-06

## Dependency Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              flake.nix                                      │
│                            (Entry Point)                                    │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
         ┌──────────────────────────┼──────────────────────────┐
         ▼                          ▼                          ▼
┌─────────────────┐       ┌─────────────────┐       ┌─────────────────────────┐
│ palette/        │       │ overlays/       │       │ mkSpecialArgs           │
│ default.nix     │◄──────│ default.nix     │       │ (DI Container)          │
│                 │       │                 │       │                         │
│ • lib functions │       │ scanPaths(./.)  │       │ palette, inputs,        │
│ • user config   │       │                 │       │ overlays, system,       │
│ • nixbox config │       └─────────────────┘       │ pkgs-stable, etc        │
│ • oc-eyes config│                                 └─────────────────────────┘
└─────────────────┘                                           │
         │                                                    │
         │                    ┌────────────────────────────────┘
         ▼                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    hosts/{hostname}/default.nix                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                      │
│  │ nyeong-air   │  │   nixbox     │  │   oc-eyes    │                      │
│  │ (darwin)     │  │  (nixos)     │  │   (nixos)    │                      │
│  └──────────────┘  └──────────────┘  └──────────────┘                      │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
         ┌──────────────────────────┼──────────────────────────┐
         ▼                          ▼                          ▼
┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────────┐
│ configuration.nix   │  │ modules/default.nix │  │ home-manager.nix        │
│ (host-specific)     │  │                     │  │ imports ../../home      │
└─────────────────────┘  │ if isDarwin:./darwin│  └─────────────────────────┘
                         │ if isLinux: ./linux │              │
                         │ always: ./base,fonts│              │
                         └─────────────────────┘              │
                                    │                         │
                                    ▼                         ▼
                         ┌─────────────────────┐  ┌─────────────────────────┐
                         │ modules/base/       │  │ home/                   │
                         │ ├── default.nix ◄───┼──│ ├── base/   (always)    │
                         │ ├── sops.nix        │  │ ├── darwin/ (if darwin) │
                         │ └── user.nix        │  │ ├── linux/  (if linux)  │
                         │                     │  │ └── features/(opt-in)   │
                         │ ⚠️ SRP violation    │  │                         │
                         └─────────────────────┘  │ ⚠️ ISP violation        │
                                                  └─────────────────────────┘
```

## Key Files

### Phase 1 대상 (modules/base 분리)

| 파일                       | 현재 역할                                 | 변경                    |
| -------------------------- | ----------------------------------------- | ----------------------- |
| `modules/base/default.nix` | nix settings + overlays + sops + timezone | 분리                    |
| `modules/base/sops.nix`    | sops.age 설정만                           | github_fetch_token 추가 |
| `modules/base/user.nix`    | 사용자 설정                               | 유지                    |

#### modules/base/default.nix 현재 책임 분석

```nix
# 1. Overlays 설정 → overlays.nix로
nixpkgs.overlays = overlays;
nixpkgs.config.allowUnfree = true;

# 2. Sops secrets → sops.nix로
sops.secrets.github_fetch_token = { ... };
sops.templates."nix-access-tokens" = { ... };
environment.etc."nix/nix.conf.d/access-tokens.conf" = ...;

# 3. Nix settings → nix-settings.nix로
nix.package = pkgs.nix;
nix.optimise.automatic = true;
nix.settings = { ... };
nix.gc = { ... };

# 4. Locale → locale.nix로
time.timeZone = "Asia/Seoul";

# 5. imports만 남김
imports = palette.lib.scanPaths ./.;
```

### Phase 2 대상 (home/base 패키지 분리)

| 파일                                | 역할            | 변경                     |
| ----------------------------------- | --------------- | ------------------------ |
| `home/base/default.nix`             | 기본 패키지     | ruby/python3/nodejs 제거 |
| `home/features/default.nix`         | feature imports | dev-lang.nix 추가        |
| `home/features/dev-lang.nix`        | 새 파일         | 생성                     |
| `hosts/nyeong-air/home-manager.nix` | nyeong-air 설정 | enable 추가              |

## Decisions

### D1: sops secrets 통합

**결정**: `default.nix`의 github_fetch_token을 `sops.nix`로 이동

**이유**:

- sops 관련 설정은 한 곳에서 관리
- SRP 준수

### D2: scanPaths 유지

**결정**: 분리 후에도 `scanPaths` 패턴 유지

**이유**:

- OCP 준수 (새 파일 추가만으로 확장)
- 기존 컨벤션 유지

### D3: dev-lang을 별도 feature로

**결정**: features.devTools와 별개로 features.devLang 생성

**이유**:

- devTools는 이미 많은 패키지 포함 (cursor, opencode, ki-editor 등)
- 언어 런타임만 따로 관리하면 더 세밀한 제어 가능
- 단, devTools가 devLang을 암묵적으로 활성화할 수도 있음 (선택사항)

### D4: 서버 패키지 검토 결과

**nixbox**: ruby/python3/nodejs 불필요

- 컨테이너/서비스로 앱 실행
- 스크립트는 bash로 충분

**oc-eyes**: 불필요

- 모니터링 서버
- 애플리케이션 개발 안 함

## Related Files (Reference)

```
palette/
├── default.nix          # lib.scanPaths, mkOptionalImport 정의
├── user-config.nix      # username, email
├── nixbox/default.nix   # nixbox 서비스 포트
└── oc-eyes/default.nix  # oc-eyes 서비스 포트
```
