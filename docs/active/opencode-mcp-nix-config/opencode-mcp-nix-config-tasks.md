# OpenCode MCP 설정을 Nix로 관리 - Tasks

**Last Updated**: 2026-01-05 19:30

## 진행해야 할 작업 체크리스트

### 구현 작업

- [x] dev-tools.nix에 `github_personal_access_token_for_mcp` sops secret 추가
- [x] dev-tools.nix에 `opencodeMcpPath` 변수 추가 (`~/.config/opencode/opencode.json`)
- [x] OpenCode opencode.json 생성/병합 로직 구현 (home.activation)
  - [x] 기존 파일이 없으면 전체 구조 생성 (`plugin`, `provider`, `$schema`, `mcp` 포함)
  - [x] 기존 파일이 있으면 `mcp` 섹션만 업데이트 (기존 항목 유지, 새 항목 추가)
  - [x] 기존 MCP 서버 설정 (exa, context7, grep)
  - [x] 새 MCP 서버 설정 (filesystem, github, git, fetch)
  - [x] GitHub MCP `environment` 필드에 sops secret에서 읽은 실제 토큰 값 설정
- [x] 파일 권한 0600 설정
- [x] 코드 포맷팅 및 린팅

### 검증 작업

- [ ] darwin rebuild 수행 (사용자)
- [ ] `/Users/nyeong/.config/opencode/opencode.json` 파일 확인
  - [ ] 파일 존재 여부
  - [ ] JSON 형식 유효성
  - [ ] `mcp` 섹션에 모든 MCP 서버가 포함되어 있는지
    - [ ] exa
    - [ ] context7
    - [ ] grep
    - [ ] filesystem
    - [ ] github (environment 필드 포함)
    - [ ] git
    - [ ] fetch
  - [ ] GitHub MCP의 `environment.GITHUB_PERSONAL_ACCESS_TOKEN`에 토큰이 올바르게 설정되었는지
  - [ ] 파일 권한이 0600인지
  - [ ] 기존 `plugin`, `provider`, `$schema` 섹션이 유지되었는지
- [ ] MCP 리소스 목록 확인
  ```bash
  opencode run --model 'cursor-mcp/auto' "List all available MCP resources"
  ```
- [ ] 각 MCP 기능 테스트
  - [ ] filesystem MCP: `opencode run --model 'cursor-mcp/auto' "Use filesystem MCP to list files in current directory"`
  - [ ] git MCP: `opencode run --model 'cursor-mcp/auto' "Use git MCP to show git status"`
  - [ ] github MCP: `opencode run --model 'cursor-mcp/auto' "Use github MCP to list repositories"`
  - [ ] fetch MCP: `opencode run --model 'cursor-mcp/auto' "Use fetch MCP to fetch https://example.com"`
- [ ] 결과 피드백 제공
- [ ] 문제 발생 시 디버깅 수행

**Last Updated**: 2026-01-05 (최종 확인)

## 완료된 작업

### 2026-01-05

- ✅ 계획 수립 완료
- ✅ 계획 파일 작성 완료
- ✅ 사용자와 계획 검토 완료
- ✅ SQLite MCP 제외 결정 (global로 필요 없음)
- ✅ GitHub MCP environment 필드 처리 방식 확정 (sops secret에서 실제 토큰 값 읽기)

## 다음 단계

1. 구현 작업 수행
2. 사용자가 darwin rebuild 수행
3. 설정 파일 확인 및 검증
4. MCP 기능 테스트
5. 문제 발생 시 디버깅
6. 모든 검증 완료 후 `docs/to-review/`로 이동
