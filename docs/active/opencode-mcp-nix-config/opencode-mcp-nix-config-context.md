# OpenCode MCP 설정을 Nix로 관리 - Context

**Last Updated**: 2026-01-05 (최종 확인)

## 주된 맥락

OpenCode MCP 설정을 Nix로 관리하여 선언적으로 관리하고, 새 MCP 서버(filesystem, github, git, fetch)를 추가하여 개발 워크플로우를 개선.

## 알아야 할 파일

### 수정할 파일

- `home/features/dev-tools.nix`: OpenCode MCP 설정 로직 추가
  - sops secret 정의 (`github_personal_access_token_for_mcp`)
  - `opencodeMcpPath` 변수 추가
  - home.activation 스크립트로 JSON 파일 생성/병합
  - 파일 권한 설정

### 참조 파일

- `secrets/dev-env.yaml`: GitHub Personal Access Token 저장소
- `home/base/sops.nix`: sops-nix 설정 예시 참고
- `home/base/ssh.nix`: sops secret 사용 예시 참고
- `home/features/dev-tools.nix`: 기존 Cursor/Gemini MCP 설정 패턴 참고
- `~/.config/opencode/opencode.json`: 기존 OpenCode 설정 파일 (참고용)

## 참고할 문서

- [Home Manager Manual - Activation Scripts](https://nix-community.github.io/home-manager/index.html#sec-activation-script)
- [jq Manual](https://stedolan.github.io/jq/manual/)
- [sops-nix Documentation](https://github.com/Mic92/sops-nix)
- [OpenCode MCP Configuration](https://opencode.ai/docs/mcp)
- [MCP Server Specifications](https://modelcontextprotocol.io/)

## 주요 결정사항

### 1. 파일 생성 방식: `home.activation` vs `home.file`

- **결정**: `home.activation` 사용
- **이유**:
  - 기존 설정 파일과 병합이 필요함
  - `home.file`은 정적 파일만 생성 가능
  - `home.activation`은 동적 로직 실행 가능
  - 기존 Cursor/Gemini MCP 설정 패턴과 일관성 유지

### 2. JSON 병합 도구: `jq` 사용

- **결정**: `jq` 패키지 사용 (이미 dev-tools.nix에 추가됨)
- **이유**:
  - 안전한 JSON 파싱 및 병합
  - Nix 패키지로 쉽게 사용 가능
  - 강력한 JSON 조작 기능
  - 기존 패턴과 일관성 유지

### 3. 파일 권한: 0600

- **결정**: `chmod 0600` 설정
- **이유**:
  - API 키가 포함된 민감한 정보
  - Owner만 읽기/쓰기 가능하도록 제한
  - 보안 강화
  - 기존 Cursor/Gemini MCP 설정 패턴과 일관성 유지

### 4. API 키 관리: sops-nix

- **결정**: `secrets/dev-env.yaml`에서 로드
- **이유**:
  - 기존 sops-nix 인프라 활용
  - 안전한 암호화 저장
  - 기존 패턴과 일관성 유지

### 5. 기존 섹션 유지

- **결정**: `plugin`, `provider`, `$schema` 섹션은 유지
- **이유**:
  - 사용자가 수동으로 설정한 내용 보존
  - `mcp` 섹션만 Nix로 관리
  - 기존 설정과의 호환성 유지

### 6. GitHub MCP 환경 변수 처리

- **결정**: `environment` 필드에 sops secret에서 읽은 실제 토큰 값 설정
- **이유**:
  - OpenCode가 `environment` 필드를 지원함
  - 환경 변수 이름이 아닌 실제 값이 필요함
  - sops secret에서 안전하게 읽어서 설정

### 7. 기존 MCP 서버 유지

- **결정**: exa, context7, grep 설정 유지
- **이유**:
  - 기존 설정이 정상 작동 중
  - Nix로 재생성하여 일관성 유지
  - 기존 패턴과 일관성 유지

## 기술적 세부사항

### OpenCode MCP 설정

- 파일 경로: `~/.config/opencode/opencode.json`
- 구조: `mcp` 객체에 각 MCP 서버 설정
- 기존 MCP:
  - `exa`: `@modelcontextprotocol/server-exa`
  - `context7`: `@upstash/context7-mcp`
  - `grep`: `@grepapp/mcp-server-grepapp`
- 새 MCP:
  - `filesystem`: `@modelcontextprotocol/server-filesystem`
  - `github`: `@modelcontextprotocol/server-github` (environment 필드 필요)
  - `git`: `@modelcontextprotocol/server-git`
  - `fetch`: `@modelcontextprotocol/server-fetch`

### GitHub MCP 환경 변수

- `environment` 필드에 실제 토큰 값 설정
- `environment.GITHUB_PERSONAL_ACCESS_TOKEN`에 sops secret에서 읽은 값 삽입

### Activation 스크립트 실행 순서

- `writeBoundary` 이후 실행
- 파일 존재 여부 확인
- 없으면 전체 구조 생성, 있으면 `mcp` 섹션만 병합
- 권한 설정

### JSON 병합 로직

- 기존 파일이 없으면:
  - `plugin`, `provider`, `$schema` 포함한 전체 구조 생성
  - `mcp` 섹션에 기존 3개 + 새 4개 추가
- 기존 파일이 있으면:
  - `jq`로 `mcp` 섹션만 병합
  - 기존 항목 유지, 새 항목 추가
  - 다른 섹션 유지

## 잠재적 이슈

1. **기존 설정 파일 손상 가능성**
   - 해결: 임시 파일 사용 후 원자적 이동 (`mv .tmp`)

2. **JSON 파싱 오류**
   - 해결: `jq`의 안전한 파싱 활용

3. **파일 권한 문제**
   - 해결: 명시적 `chmod 0600` 설정

4. **API 키 노출**
   - 해결: sops-nix로 안전하게 관리, 파일 권한 제한

5. **기존 섹션 손실**
   - 해결: `mcp` 섹션만 업데이트하고 다른 섹션은 유지

6. **GitHub MCP 환경 변수 형식 오류**
   - 해결: `environment` 필드에 실제 토큰 값 설정 (문자열로 삽입)

## 기존 패턴 참고

- Cursor MCP 설정: `home.activation.setupCursorMcp`
- Gemini MCP 설정: `home.activation.setupGeminiMcp`
- 동일한 패턴을 OpenCode MCP 설정에 적용
