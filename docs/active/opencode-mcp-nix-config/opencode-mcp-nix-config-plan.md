# OpenCode MCP 설정을 Nix로 관리

**Last Updated**: 2026-01-05 (최종 확인)

## 목표

- OpenCode MCP 설정을 Nix로 관리하여 선언적으로 관리
- 기존 MCP 서버 유지 (exa, context7, grep)
- 새 MCP 서버 추가 (filesystem, github, git, fetch)
- GitHub MCP 환경 변수는 sops secret에서 읽어서 설정

## 구현 계획

### 1. Secrets 관리

- `secrets/dev-env.yaml`에 `github_personal_access_token_for_mcp`가 이미 존재함 (확인 완료)
- `home/features/dev-tools.nix`에서 sops secret으로 로드
- `sops.secrets.github_personal_access_token_for_mcp`로 설정하여 activation script에서 사용

### 2. OpenCode 설정 (`~/.config/opencode/opencode.json`)

- `home.activation`을 사용하여 기존 설정과 병합
- `jq`를 사용하여 JSON 병합:
  - 기존 파일이 없으면 전체 구조 생성 (`plugin`, `provider`, `$schema` 포함)
  - 기존 파일이 있으면 `mcp` 섹션만 업데이트 (기존 항목 유지, 새 항목 추가)
- 설정 내용:
  ```json
  {
    "mcp": {
      "exa": {
        "type": "local",
        "command": ["npx", "-y", "@modelcontextprotocol/server-exa"]
      },
      "context7": {
        "type": "local",
        "command": ["npx", "-y", "@upstash/context7-mcp"]
      },
      "grep": {
        "type": "local",
        "command": ["npx", "-y", "@grepapp/mcp-server-grepapp"]
      },
      "filesystem": {
        "type": "local",
        "command": ["npx", "-y", "@modelcontextprotocol/server-filesystem"]
      },
      "github": {
        "type": "local",
        "command": ["npx", "-y", "@modelcontextprotocol/server-github"],
        "environment": {
          "GITHUB_PERSONAL_ACCESS_TOKEN": "<sops secret에서 읽은 실제 토큰 값>"
        }
      },
      "git": {
        "type": "local",
        "command": ["npx", "-y", "@modelcontextprotocol/server-git"]
      },
      "fetch": {
        "type": "local",
        "command": ["npx", "-y", "@modelcontextprotocol/server-fetch"]
      }
    }
  }
  ```
- **보안**: 파일 생성 시 `chmod 0600` 설정 (owner만 읽기/쓰기)

### 3. 기존 섹션 유지

- `plugin`: 기존 내용 유지
- `provider`: 기존 내용 유지
- `$schema`: 기존 내용 유지
- `mcp` 섹션만 업데이트

## 파일 수정

- [home/features/dev-tools.nix](home/features/dev-tools.nix): MCP 설정 로직 추가
  - sops secret 추가 (`github_personal_access_token_for_mcp`)
  - `opencodeMcpPath` 변수 추가
  - home.activation에 JSON 병합 로직 추가
  - 파일 권한 0600 설정
- `secrets/dev-env.yaml`: `github_personal_access_token_for_mcp` 이미 존재 (확인 완료)

## 구현 세부사항

### JSON 병합 로직

- `jq`를 사용하여 안전하게 JSON 병합
- 기존 파일이 없으면:
  - 전체 구조 생성 (`plugin`, `provider`, `$schema`, `mcp` 포함)
  - `mcp` 섹션에 기존 3개 + 새 4개 추가
- 기존 파일이 있으면:
  - `mcp` 섹션만 업데이트 (기존 항목 유지, 새 항목 추가)
  - 다른 섹션(`plugin`, `provider`, `$schema`) 유지

### GitHub MCP 환경 변수 처리

- `environment` 필드에 sops secret에서 읽은 실제 토큰 값 설정
- `environment.GITHUB_PERSONAL_ACCESS_TOKEN`에 실제 토큰 값 삽입

### 보안

- API 키가 포함된 JSON 파일은 `mode = "0600"`으로 설정
- Nix가 생성한 파일은 쓰기 권한 제거 (Nix로만 제어)
- `home.activation`을 사용하여 파일 생성 및 권한 제어

### 의존성

- `jq`: JSON 병합을 위해 필요 (pkgs.jq, 이미 추가됨)
- `sops-nix`: API 키 관리

## 검증 및 테스트

- darwin rebuild는 사용자가 직접 수행
- rebuild 완료 후 다음 파일을 확인:
  - `/Users/nyeong/.config/opencode/opencode.json`
- 확인 사항:
  - 파일이 올바르게 생성되었는지
  - JSON 형식이 올바른지
  - `mcp` 섹션에 모든 MCP 서버가 포함되어 있는지
  - GitHub MCP의 `environment` 필드에 토큰이 올바르게 설정되었는지
  - 파일 권한이 0600인지
  - 기존 `plugin`, `provider`, `$schema` 섹션이 유지되었는지
- MCP 리소스 목록 확인:
  ```bash
  opencode run --model 'cursor-mcp/auto' "List all available MCP resources"
  ```
- 각 MCP 기능 테스트:
  ```bash
  opencode run --model 'cursor-mcp/auto' "Use filesystem MCP to list files in current directory"
  opencode run --model 'cursor-mcp/auto' "Use git MCP to show git status"
  opencode run --model 'cursor-mcp/auto' "Use github MCP to list repositories"
  opencode run --model 'cursor-mcp/auto' "Use fetch MCP to fetch https://example.com"
  ```
- 결과에 대해 피드백 제공
- 원하는 바가 잘 이루어지지 않았다면 디버깅 수행

## 참고사항

- 기존 설정 파일이 없으면 새로 생성
- 기존 설정이 있으면 JSON 병합 로직 사용
- API 키는 sops-nix로 안전하게 관리
- 파일 권한은 보안을 위해 엄격하게 제어
- 기존 `plugin`, `provider`, `$schema` 섹션은 유지
