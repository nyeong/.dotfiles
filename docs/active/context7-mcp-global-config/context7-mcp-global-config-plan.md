# Context7 MCP 글로벌 설정 추가

**Last Updated**: 2025-12-12

## 목표

- Cursor와 Gemini가 설치될 때 Context7 MCP를 글로벌로 설정
- Cursor: `~/.cursor/mcp.json`에 context7 서버 추가
- Gemini: `~/.gemini/settings.json`에 context7를 allowed 목록에 추가

## 구현 계획

### 1. Secrets 관리

- `secrets/dev-env.yaml`에 `context7_api_key`가 이미 존재함 (확인 완료)
- `home/features/dev-tools.nix`에서 sops secret으로 로드
- `sops.secrets.context7_api_key`로 설정하여 activation script에서 사용

### 2. Cursor 설정 (`~/.cursor/mcp.json`)

- `home.activation`을 사용하여 기존 설정과 병합
- `jq`를 사용하여 JSON 병합:
  - 기존 파일이 없으면 새로 생성
  - 기존 파일이 있으면 `mcpServers.context7` 항목 추가/업데이트
- 설정 내용:
  ```json
  {
    "mcpServers": {
      "context7": {
        "url": "https://mcp.context7.com/mcp",
        "headers": {
          "CONTEXT7_API_KEY": "YOUR_API_KEY"
        }
      }
    }
  }
  ```
- **보안**: 파일 생성 시 `chmod 0600` 설정 (owner만 읽기/쓰기)

### 3. Gemini 설정 (`~/.gemini/settings.json`)

- `home.activation`을 사용하여 기존 설정과 병합
- `jq`를 사용하여 JSON 병합:
  - 기존 파일이 없으면 새로 생성
  - 기존 파일이 있으면 `mcp.allowed` 배열에 "context7" 추가 (중복 방지)
- 설정 내용:
  ```json
  {
    "mcpServers": {
      "context7": {
        "httpUrl": "https://mcp.context7.com/mcp",
        "headers": {
          "CONTEXT7_API_KEY": "YOUR_API_KEY",
          "Accept": "application/json, text/event-stream"
        }
      }
    }
  }
  ```
- **보안**: 파일 생성 시 `chmod 0600` 설정 (owner만 읽기/쓰기)

## 파일 수정

- [home/features/dev-tools.nix](home/features/dev-tools.nix): MCP 설정 로직 추가
  - sops secret 추가
  - home.activation에 JSON 병합 로직 추가
  - 파일 권한 0600 설정
- `secrets/dev-env.yaml`: context7_api_key 이미 존재 (확인 완료)

## 구현 세부사항

### JSON 병합 로직

- `jq`를 사용하여 안전하게 JSON 병합
- Cursor: `mcpServers` 객체에 `context7` 키 추가/업데이트
- Gemini: `mcp.allowed` 배열에 "context7" 추가 (중복 체크)

### 보안

- API 키가 포함된 JSON 파일은 `mode = "0600"`으로 설정
- Nix가 생성한 파일은 쓰기 권한 제거 (Nix로만 제어)
- `home.activation`을 사용하여 파일 생성 및 권한 제어

### 의존성

- `jq`: JSON 병합을 위해 필요 (pkgs.jq)
- `sops-nix`: API 키 관리

## 검증 및 테스트

- darwin rebuild는 사용자가 직접 수행
- rebuild 완료 후 다음 파일들을 확인:
  - `/Users/nyeong/.cursor/mcp.json`
  - `/Users/nyeong/.gemini/settings.json`
- 확인 사항:
  - 파일이 올바르게 생성되었는지
  - JSON 형식이 올바른지
  - Context7 MCP 설정이 포함되어 있는지
  - 파일 권한이 0600인지
- 결과에 대해 피드백 제공
- 원하는 바가 잘 이루어지지 않았다면 디버깅 수행

## 참고사항

- 기존 설정 파일이 없으면 새로 생성
- 기존 설정이 있으면 JSON 병합 로직 사용
- API 키는 sops-nix로 안전하게 관리
- 파일 권한은 보안을 위해 엄격하게 제어
