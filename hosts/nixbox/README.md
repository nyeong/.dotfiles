# nixbox

- OS: NixOS 25.11 (Xantusia) x86_64
- Kernel: Linux 6.12.41
- Shell: zsh 5.9
- CPU: Intel(R) N150 (4) @ 3.60 GHz
- GPU: Intel Graphics @ 1.00 GHz [Integrated]
- Memory: 1.45 GiB / 31.09 GiB (5%)
- Swap: 2.96 MiB / 32.00 GiB (0%)
- Disk (/): 147.22 GiB / 930.58 GiB (16%) - btrfs
- Disk (/storage): 5.78 MiB / 12.73 TiB (0%) - btrfs

## Nix

- Use nix services over containers

## 용도

- [ ] 문서/코드 관리 (org 백업 등)
- [ ] PDF, 전자책 관리
- [ ] 미디어 스트리밍
- [ ] 웹 아카이빙
- [ ] 사진 백업
- [ ] 재무 관리 (Firefly III)
- [x] 시스템 모니터링 (Telegraf + PostgreSQL + Grafana)

## 모니터링 스택

**구조:**

```
Telegraf (메트릭 수집)
  ├─ CPU, 메모리, 디스크, 네트워크
  └─→ PostgreSQL + TimescaleDB (저장)
      └─→ Grafana (시각화)
```

**설정:**

- `services/telegraf.nix` : Telegraf 에이전트
- `services/postgres.nix` : PostgreSQL + TimescaleDB 확장
- `services/grafana.nix` : Grafana 대시보드

**배포:**

```bash
sudo nixos-rebuild switch --flake path:.#nixbox
```

**초기 설정:**

1. **Grafana 접속**
   - URL: `http://localhost:3033`
   - 기본 username/password: `admin/admin` (NixOS 기본값)

2. **메트릭 확인**

   ```bash
   # PostgreSQL에 데이터가 저장되는지 확인
   sudo -u postgres psql telegraf -c "SELECT COUNT(*) FROM telegraf LIMIT 10;"
   ```

3. **대시보드 생성**
   - Grafana UI에서 `Create > Dashboard`
   - PostgreSQL datasource로 쿼리 작성
   - 시계열 데이터 시각화
