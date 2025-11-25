# nixbox

```
nyeong@nixbox
-------------
OS: NixOS 25.11 (Xantusia) x86_64
Kernel: Linux 6.12.42
Uptime: 95 days, 20 hours, 38 mins
Packages: 652 (nix-system), 163 (nix-user)
Shell: zsh 5.9
Terminal: /dev/pts/0
CPU: Intel(R) N150 (4) @ 3.60 GHz
GPU: Intel Graphics @ 1.00 GHz [Integrated]
Memory: 5.54 GiB / 31.09 GiB (18%)
Swap: 11.84 MiB / 32.00 GiB (0%)
Disk (/): 98.36 GiB / 930.58 GiB (11%) - btrfs
Disk (/mnt/media): 1.26 TiB / 1.79 TiB (70%) - ext4
Disk (/storage): 265.59 GiB / 12.73 TiB (2%) - btrfs
Local IP (enp2s0): 192.168.1.160/24
Locale: en_US.UTF-8
```

## 용도

- [ ] 문서/코드 관리 (org 백업 등)
- [ ] PDF, 전자책 관리
- [ ] 미디어 스트리밍
- [ ] 웹 아카이빙
- [ ] 사진 백업
- [ ] 재무 관리

## 디렉토링 전략

### 하드웨어 구성

| 디스크 | 용량 | 파일시스템 | 마운트     | 용도                               |
| ------ | ---- | ---------- | ---------- | ---------------------------------- |
| SSD    | 1TB  | btrfs      | `/`        | OS, DB, 캐시, 컨테이너 설정        |
| HDD1   | 14TB | btrfs      | `/storage` | 대용량 데이터 (미디어, 사진, 문서) |
| HDD2   | 2TB  | btrfs      | `/backup`  | 백업 (미러링/스냅샷)               |

### 설계 원칙

1. **SSD는 핫 데이터 전용**: DB, 캐시, 인덱스, 메타데이터
2. **HDD는 콜드 데이터 전용**: 원본 미디어, 아카이브
3. **btrfs subvolume 활용**: 스냅샷, 증분 백업, 할당량 관리
4. **서비스별 격리**: 각 subvolume은 독립적 스냅샷/백업 주기 설정 가능

### SSD 구조 (`/`)

빠른 I/O가 필요한 데이터만 배치. `/var/lib/${service-name}`

```
/var/lib/
├── containers/           # 컨테이너별 설정/캐시
│   ├── immich/
│   │   ├── config/
│   │   ├── postgres/     # Immich 전용 PostgreSQL
│   │   ├── ml/           # ML 모델 캐시
│   │   └── valkey/       # Immich 전용 Valkey
│   ├── jellyfin/
│   │   ├── config/
│   │   └── cache/
│   ├── kavita/
│   ├── calibre-web/
│   └── valkey/           # 공용 Valkey
├── postgresql/           # 공용 PostgreSQL (sftpgo, paperless)
├── paperless/
└── sftpgo/
```

### HDD 구조 (`/storage`)

대용량 데이터를 btrfs subvolume으로 관리. 일반적으로는 `/storage/@services/{service-name}`

- `@inbox/` : 사용자가 수동 업로드한 파일.
- `@donwloads/` : 서비스가 자동으로 다운로드한 파일
- `@services/` : 서비스가 소유하는 파일들

```
/storage/
├── @inbox/
├── @downloads/
│   ├── complete/
│   └── incomplete/
└── @services/
    ├── immich/
    ├── jellyfin/
    ├── kavita/
    ├── calibre/
    ├── paperless/
    └── archivebox/
```
