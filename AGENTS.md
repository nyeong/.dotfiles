# .dotfiles

## Setup

Nix를 이 레포를 위한 개발환경 셋업을 위해서 사용합니다.

- 개발환경 진입: `nix develop` 혹은 `direnv allow`
- 빌드: `nix build .#formatter` (포매터 빌드) 또는 `nix build .#darwinConfigurations.nyeong-air` (시스템 구성 빌드)
- check: `nix flake check`

고치고자 하는 호스트와 nix를 편집하고 있는 호스트가 다를 수 있습니다. 항상 `hostname`으로 hostname을 확인하십시오.

## Docs

- 각 호스트에 대한 문서는 `/hosts/{host-name}/README.md`를 참고하십시오.
- 그 외의 문서는 `/docs/**/*.md`를 참고하십시오.

## Code style

Nix 코드 생성 후 `nix run .#lint`, `nix run .#format`으로 항상 린팅, 포맷팅 하십시오.

- Nix: alejandra (포매터), statix (린터), deadnix (린터)
- Lua: stylua
- Shell: shfmt
- 기타 (JSON/Markdown/YAML 등): prettier

## Architecture notes

- Entry points: `<paths>`
- Config files: `<list>`
- Key decisions: `<ADR refs or notes>`

## Testing

- Unit: `pnpm test` (coverage ≥ 80%)
- Integration: `pnpm run test:integration`
- E2E(옵션): `pnpm run test:e2e`
- Fix until entire suite is green

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

## PR instructions

- Title: `[<scope>] <Title>`
- Require: `nix flake check` 통과
- Update docs for public API changes

## Security guidelines

- Never commit secrets; use environment variables
- Sanitize all inputs; follow OWASP
- Dependency audit: `pnpm audit` or `npm audit`
- `.env.local` for local secrets; prod secrets in platform vault

## Common issues

- CI build fails: clear cache, update lockfile (`nix flake update`)
- Formatting errors: `nix run .#format` 실행 후 커밋
- Lint errors: `nix run .#lint` 실행하여 확인
