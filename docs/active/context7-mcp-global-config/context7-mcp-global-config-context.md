# Context7 MCP 글로벌 설정 - Context

**Last Updated**: 2025-12-12

## 주된 맥락

Agent tool (Cursor, Gemini)이 설치될 때 Context7 MCP를 글로벌로 설정하여 모든 프로젝트에서 사용할 수 있도록 함.

## 알아야 할 파일

### 수정된 파일

- `home/features/dev-tools.nix`: MCP 설정 로직 추가
  - sops secret 정의
  - home.activation 스크립트로 JSON 파일 생성/병합
  - 파일 권한 설정

### 참조 파일

- `secrets/dev-env.yaml`: Context7 API 키 저장소
- `home/base/sops.nix`: sops-nix 설정 예시 참고
- `home/base/ssh.nix`: sops secret 사용 예시 참고
- `home/features/emacs/default.nix`: home.activation 사용 예시 참고

## 참고할 문서

- [Home Manager Manual - Activation Scripts](https://nix-community.github.io/home-manager/index.html#sec-activation-script)
- [jq Manual](https://stedolan.github.io/jq/manual/)
- [sops-nix Documentation](https://github.com/Mic92/sops-nix)

## 주요 결정사항

### 1. 파일 생성 방식: `home.activation` vs `home.file`

- **결정**: `home.activation` 사용
- **이유**:
  - 기존 설정 파일과 병합이 필요함
  - `home.file`은 정적 파일만 생성 가능
  - `home.activation`은 동적 로직 실행 가능

### 2. JSON 병합 도구: `jq` 사용

- **결정**: `jq` 패키지 사용
- **이유**:
  - 안전한 JSON 파싱 및 병합
  - Nix 패키지로 쉽게 사용 가능
  - 강력한 JSON 조작 기능

### 3. 파일 권한: 0600

- **결정**: `chmod 0600` 설정
- **이유**:
  - API 키가 포함된 민감한 정보
  - Owner만 읽기/쓰기 가능하도록 제한
  - 보안 강화

### 4. API 키 관리: sops-nix

- **결정**: `secrets/dev-env.yaml`에서 로드
- **이유**:
  - 기존 sops-nix 인프라 활용
  - 안전한 암호화 저장
  - 기존 패턴과 일관성 유지

### 5. Cursor 설정 형식

- **결정**: URL 기반 연결 사용
- **이유**:
  - 사용자 요구사항
  - 명령어 대신 URL 연결 방식 채택

## 기술적 세부사항

### Cursor MCP 설정

- 파일 경로: `~/.cursor/mcp.json`
- 구조: `mcpServers.context7` 객체
- URL: `https://mcp.context7.com/mcp`
- 헤더: `CONTEXT7_API_KEY` 환경변수

### Gemini MCP 설정

- 파일 경로: `~/.gemini/settings.json`
- 구조: `mcpServers.context7` 객체
- httpUrl: `https://mcp.context7.com/mcp`
- 헤더: `CONTEXT7_API_KEY`, `Accept: application/json, text/event-stream`

### Activation 스크립트 실행 순서

- `writeBoundary` 이후 실행
- 파일 존재 여부 확인
- 없으면 새로 생성, 있으면 병합
- 권한 설정

## 잠재적 이슈

1. **기존 설정 파일 손상 가능성**
   - 해결: 임시 파일 사용 후 원자적 이동 (`mv .tmp`)

2. **JSON 파싱 오류**
   - 해결: `jq`의 안전한 파싱 활용

3. **파일 권한 문제**
   - 해결: 명시적 `chmod 0600` 설정

4. **API 키 노출**
   - 해결: sops-nix로 안전하게 관리, 파일 권한 제한
