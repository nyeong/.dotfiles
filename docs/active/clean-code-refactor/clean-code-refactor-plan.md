# Clean Code Refactor Plan

**Created**: 2026-01-06
**Last Updated**: 2026-01-06
**Status**: Planning

## Goal

Clean Code 5원칙(SOLID)에 따라 dotfiles 구조 개선

## Summary

현재 구조는 전반적으로 잘 설계되어 있음. 주요 개선점:

1. **SRP 위반**: `modules/base/default.nix`에 여러 책임 혼재
2. **ISP 위반**: 서버에 불필요한 개발 패키지 설치

## Current State Analysis

### ✅ 잘 되어 있는 점

| 원칙 | 평가 | 설명                                      |
| ---- | ---- | ----------------------------------------- |
| OCP  | ✅   | `scanPaths` 패턴으로 파일 추가만으로 확장 |
| LSP  | ✅   | 모든 호스트가 동일한 인터페이스 사용      |
| DIP  | ✅   | `specialArgs`로 의존성 주입               |

### ⚠️ 개선 필요

| 원칙 | 문제            | 위치                       |
| ---- | --------------- | -------------------------- |
| SRP  | 여러 책임 혼재  | `modules/base/default.nix` |
| ISP  | 불필요한 의존성 | `home/base/default.nix`    |

## Approach

### Phase 1: modules/base 분리 (SRP)

`modules/base/default.nix` → 단일 책임 파일들로 분리

| 현재 책임                                              | 분리 후 파일           |
| ------------------------------------------------------ | ---------------------- |
| nix settings (experimental-features, gc, substituters) | `nix-settings.nix`     |
| overlays 설정                                          | `overlays.nix`         |
| sops secrets (github_fetch_token, access-tokens)       | 기존 `sops.nix`로 통합 |
| timeZone                                               | `locale.nix`           |

분리 후 `default.nix`는 `imports = palette.lib.scanPaths ./.;`만 남음.

### Phase 2: home/base 패키지 분리 (ISP)

서버에 불필요한 패키지를 feature로 이동

| 패키지                    | 현재 위치               | 이동                              |
| ------------------------- | ----------------------- | --------------------------------- |
| ruby, python3, nodejs     | `home/base/default.nix` | `features/dev-lang.nix` (새 파일) |
| age, sops, gnupg, fd, bat | `home/base/default.nix` | 유지 (모든 호스트 필요)           |

## Non-Goals

- `mkSpecialArgs` 정리 (영향 적음, 복잡도만 증가)
- 동작 변경 없음, 파일 구조만 개선
- features.devTools 내부의 언어(elixir, python3, ruby, metals)는 이미 feature로 분리됨

## Risks

| 리스크         | 수준 | 대응                     |
| -------------- | ---- | ------------------------ |
| 빌드 실패      | 낮음 | `nix flake check` 검증   |
| 기존 동작 변경 | 낮음 | 순수 리팩토링, 기능 동일 |

## Success Criteria

- [ ] `nix flake check` 통과
- [ ] `darwin-rebuild build --flake .#nyeong-air` 성공
- [ ] `nixos-rebuild build --flake .#nixbox` 성공 (원격 또는 dry-run)
- [ ] `nix run .#lint` 경고 없음
