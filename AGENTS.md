# .dotfiles

## Project overview

Nix flake 기반 선언적 시스템 관리 프로젝트.

### 관리 호스트

| 호스트       | 플랫폼             | 용도                                         |
| ------------ | ------------------ | -------------------------------------------- |
| `nyeong-air` | macOS (nix-darwin) | 개발 머신 (MacBook Air M2)                   |
| `nixbox`     | NixOS              | 홈서버 (미디어, 문서 관리, 백업)             |
| `oc-eyes`    | NixOS              | OCI 모니터링 서버 (Grafana, VictoriaMetrics) |

### 그 외 호스트

Nix로 관리하지 않아 본 저장소에는 없지만 홈 네트워크에 포함되는 장치

| 호스트     | 플랫폼     | 용도                       |
| ---------- | ---------- | -------------------------- |
| `uirouter` | OpenWRT    | 라우터                     |
| `pve`      | Proxmox VE | 로컬 VM 테스트용 (구성 전) |

### 기술 스택

- **Nix Flake**: 패키지 및 시스템 구성 관리
- **home-manager**: 사용자 환경 설정
- **nix-darwin**: macOS 시스템 설정
- **sops-nix**: 시크릿 암호화

## Architecture

```
.dotfiles/
├── flake.nix          # 진입점. 모든 호스트 및 앱 정의
├── hosts/             # 호스트별 시스템 설정
│   ├── nyeong-air/    # macOS 개발 머신
│   ├── nixbox/        # 홈서버
│   └── oc-eyes/       # 모니터링 서버
├── home/              # home-manager 설정
│   ├── base/          # 모든 호스트 공통
│   ├── darwin/        # macOS 전용
│   ├── linux/         # Linux 전용
│   └── features/      # 선택적 기능 (mkEnableOption)
├── modules/           # 시스템 레벨 NixOS/Darwin 모듈
│   ├── base/          # 공통 시스템 모듈
│   ├── darwin/        # macOS 시스템 모듈
│   └── linux/         # Linux 시스템 모듈
├── palette/           # 공유 변수, 유틸리티 함수
│   ├── default.nix    # lib 함수 및 공유 변수
│   ├── user-config.nix
│   ├── nixbox/        # nixbox 전용 설정
│   └── oc-eyes/       # oc-eyes 전용 설정
├── overlays/          # 패키지 오버레이
├── secrets/           # sops로 암호화된 시크릿 (.yaml)
└── docs/              # 문서 및 진행중인 작업
```

## Setup

Nix를 이 레포를 위한 개발환경 셋업을 위해서 사용합니다.

- 개발환경: `nix develop` 혹은 `direnv allow`
- 빌드: `nix build .#formatter` (포매터 빌드) 또는 `nix build .#darwinConfigurations.nyeong-air` (시스템 구성 빌드)
- check: `nix flake check`

고치고자 하는 호스트와 nix를 편집하고 있는 호스트가 다를 수 있습니다. 항상 `hostname`으로 hostname을 확인하십시오.

## Development workflow

### Code style

Nix 코드 생성 후 `nix run .#lint`, `nix run .#format`으로 항상 린팅, 포맷팅 하십시오.

- Nix: alejandra (포매터), statix (린터), deadnix (린터)
- Lua: stylua
- Shell: shfmt
- 기타 (JSON/Markdown/YAML 등): prettier

### Automated checks

- Format: `nix run .#format` (포매팅 적용) 또는 `nix run .#format-check` (체크만)
- Lint: `nix run .#lint` (statix + deadnix 실행)
- 개별 lint:
  - `nix run .#statix` (Nix 코드 린트)
  - `nix run .#deadnix` (사용하지 않는 Nix 코드 검사)
- All checks: `nix flake check` (pre-commit + formatting 모두 실행)

### Git hooks

- Pre-commit: treefmt (포매팅) + statix (warn) + deadnix (warn) 자동 실행
- git-hooks.nix를 통해 자동 설정됨

### Deployment

```bash
# macOS (nix-darwin)
darwin-rebuild switch --flake .#nyeong-air

# NixOS
nixos-rebuild switch --flake .#nixbox
nixos-rebuild switch --flake .#oc-eyes
```

## Core patterns

### home vs modules

- **home** (`home/`): 사용자 환경 설정. 대부분의 설정은 여기에
- **modules** (`modules/`): 시스템 레벨 설정. root 권한 필요하거나 home-manager에 없는 경우

### Profiles (base, darwin, linux)

`home/` 하위의 프로필 디렉토리:

- `base/`: 모든 호스트에서 자동 로드
- `darwin/`: macOS 호스트에서 자동 로드
- `linux/`: Linux 호스트에서 자동 로드

### Features 패턴

일부 호스트만 공유하는 설정은 `home/features/`에 `mkEnableOption` 패턴으로 작성:

```nix
{ config, lib, pkgs, ... }: let
  cfg = config.features.{feature-name};
in {
  options.features.{feature-name} = {
    enable = lib.mkEnableOption "description";
  };

  config = lib.mkIf cfg.enable {
    # 설정 내용
  };
}
```

호스트의 `home-manager.nix`에서 `features.{feature-name}.enable = true;`로 활성화.

### 단일 파일 vs 디렉토리

- **단일 nix 파일만 필요**: `{name}.nix`
- **추가 설정 파일 필요**: `{name}/default.nix` + `{name}/config/` 디렉토리에 설정 파일

### palette 사용

`palette`는 `specialArgs`를 통해 모든 모듈에서 접근 가능:

```nix
{ palette, ... }: {
  # 사용자 정보
  home.username = palette.user.username;

  # 유틸리티 함수
  someUrl = palette.lib.mkMagicDnsUrl "service";

  # 호스트별 설정
  ports = palette.nixbox.services.grafana.port;
}
```

## Adding new components

### 새 호스트 추가

1. `hosts/{hostname}/` 디렉토리 생성
2. 필수 파일 작성:
   - `default.nix`: 호스트 진입점
   - `configuration.nix`: 시스템 설정
   - `home-manager.nix`: 사용자 환경 설정
   - `README.md`: 호스트 설명
3. `flake.nix`에 호스트 등록 (darwinConfigurations 또는 nixosConfigurations)

### 새 feature 추가

1. `home/features/{feature-name}.nix` 생성 (또는 `{feature-name}/default.nix`)
2. `mkEnableOption` 패턴 사용
3. `home/features/default.nix`의 imports에 추가
4. 필요한 호스트의 `home-manager.nix`에서 enable

### 새 서비스 추가 (nixbox/oc-eyes)

1. `hosts/{hostname}/services/{service-name}.nix` 생성
2. 포트 등 설정값은 `palette/{hostname}/` 하위에 정의
3. `hosts/{hostname}/services/default.nix`의 imports에 추가

## Secrets management

### sops-nix 구조

- 암호화된 시크릿: `secrets/*.yaml`
- age 키 위치:
  - macOS: `~/Library/Application Support/sops/age/keys.txt`
  - Linux: `~/.config/sops/age/keys.txt`

### 시크릿 파일

| 파일                    | 용도               |
| ----------------------- | ------------------ |
| `secrets/api-keys.yaml` | API 키             |
| `secrets/dev-env.yaml`  | 개발 환경 변수     |
| `secrets/nixbox.yaml`   | nixbox 전용 시크릿 |
| `secrets/personal.yaml` | 개인 정보          |

### 시크릿 사용 예시

```nix
{ config, ... }: {
  sops.secrets."api-key" = {
    sopsFile = ../secrets/api-keys.yaml;
  };

  # 사용
  environment.etc."config".text = ''
    API_KEY_FILE=${config.sops.secrets."api-key".path}
  '';
}
```

## 남은 작업이 있는지 확인

- `docs/active/`에 진행중인 작업이 있는지 확인하십시오
- 작업을 재개할 때에는 각 `[task-name]/` 디렉토리의 모든 파일을 읽으십시오
- 작업을 진행할 때마다 "Last Updated" 타임스탬프를 업데이트하십시오

## 큰 작업을 진행할 때

PLAN 모드로 사용자와 계획을 검토한 후에는 아래를 진행하십시오:

1. **Task 디렉토리 생성**: `mkdir -p docs/active/[task-name]/`
2. **문서 작성**:
   - `[task-name]-plan.md` : 승인된 계획
   - `[task-name]-context.md` : 주된 맥락, 알아야할 파일, 참고할 문서, 결정 등
   - `[task-name]-tasks.md` : 진행해야할 작업에 대한 체크리스트
3. **정리**:
   - 작업이 완료되었다고 판단되면 해당 디렉토리를 `docs/to-review/[task-name]/`으로 옮기십시오

## Security guidelines

- Never commit secrets; use sops-nix for encryption
- age 키는 절대 커밋하지 말 것
- 새 시크릿 추가 시 `.sops.yaml` 규칙 확인

## Common issues

- **CI build fails**: clear cache, update lockfile (`nix flake update`)
- **Formatting errors**: `nix run .#format` 실행 후 커밋
- **Lint errors**: `nix run .#lint` 실행하여 확인
- **sops decrypt 실패**: age 키 파일 위치 및 권한 확인
