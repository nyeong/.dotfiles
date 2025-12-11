# Context7 MCP 글로벌 설정 - Tasks

**Last Updated**: 2025-12-12

## 진행해야 할 작업 체크리스트

### 구현 작업

- [x] dev-tools.nix에 context7_api_key sops secret 추가
- [x] dev-tools.nix에 jq 패키지 추가
- [x] Cursor mcp.json 생성/병합 로직 구현 (home.activation)
- [x] Gemini settings.json 생성/병합 로직 구현 (home.activation) - 계획 변경사항 반영
- [x] 파일 권한 0600 설정
- [x] 코드 포맷팅 및 린팅

### 검증 작업

- [ ] darwin rebuild 수행 (사용자)
- [ ] `/Users/nyeong/.cursor/mcp.json` 파일 확인
  - [ ] 파일 존재 여부
  - [ ] JSON 형식 유효성
  - [ ] `mcpServers.context7` 설정 포함 여부
  - [ ] API 키가 올바르게 설정되었는지
  - [ ] 파일 권한이 0600인지
- [ ] `/Users/nyeong/.gemini/settings.json` 파일 확인
  - [ ] 파일 존재 여부
  - [ ] JSON 형식 유효성
  - [ ] `mcp.allowed` 배열에 "context7" 포함 여부
  - [ ] 파일 권한이 0600인지
- [ ] 결과 피드백 제공
- [ ] 문제 발생 시 디버깅 수행

## 완료된 작업

### 2025-12-11

- ✅ sops secret 추가 완료
- ✅ jq 패키지 추가 완료
- ✅ Cursor MCP 설정 로직 구현 완료
- ✅ Gemini MCP 설정 로직 구현 완료 (초기 버전)
- ✅ 코드 포맷팅 완료
- ✅ 린팅 확인 완료 (경고 없음)

### 2025-12-12

- ✅ 계획 수정사항 확인 (Gemini 설정 형식 변경)
- ✅ Gemini 설정을 mcpServers.context7 형식으로 변경
- ✅ httpUrl 및 Accept 헤더 추가
- ✅ 코드 포맷팅 완료
- ✅ 린팅 확인 완료

## 다음 단계

1. 사용자가 darwin rebuild 수행
2. 설정 파일 확인 및 검증
3. 문제 발생 시 디버깅
4. 모든 검증 완료 후 `docs/to-review/`로 이동
