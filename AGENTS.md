# .dotfiles

## 목적

- 내 시스템들에 대한 선언적 정의
- 다루는 각 호스트에 대한 설명은 [hosts](./hosts) 참고

## Setup

Nix를 이 레포를 위한 개발환경 셋업을 위해서 사용합니다.

- 개발환경: `nix develop` 혹은 `direnv allow`
- 빌드: `nix build .#formatter` (포매터 빌드) 또는 `nix build .#darwinConfigurations.nyeong-air` (시스템 구성 빌드)
- check: `nix flake check`

고치고자 하는 호스트와 nix를 편집하고 있는 호스트가 다를 수 있습니다. 항상 `hostname`으로 hostname을 확인하십시오.

## Code style

Nix 코드 생성 후 `nix run .#lint`, `nix run .#format`으로 항상 린팅, 포맷팅 하십시오.

- Nix: alejandra (포매터), statix (린터), deadnix (린터)
- Lua: stylua
- Shell: shfmt
- 기타 (JSON/Markdown/YAML 등): prettier

## Automated checks

- Format: `nix run .#format` (포매팅 적용) 또는 `nix run .#format-check` (체크만)
- Lint: `nix run .#lint` (statix + deadnix 실행)
- 개별 lint:
  - `nix run .#statix` (Nix 코드 린트)
  - `nix run .#deadnix` (사용하지 않는 Nix 코드 검사)
- All checks: `nix flake check` (pre-commit + formatting 모두 실행)

## Git hooks

- Pre-commit: treefmt (포매팅) + statix (warn) + deadnix (warn) 자동 실행
- git-hooks.nix를 통해 자동 설정됨

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

- Never commit secrets; use environment variables
- Sanitize all inputs; follow OWASP
- Dependency audit: `pnpm audit` or `npm audit`
- `.env.local` for local secrets; prod secrets in platform vault

## Common issues

- CI build fails: clear cache, update lockfile (`nix flake update`)
- Formatting errors: `nix run .#format` 실행 후 커밋
- Lint errors: `nix run .#lint` 실행하여 확인
