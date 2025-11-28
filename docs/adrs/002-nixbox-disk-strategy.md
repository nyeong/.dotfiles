# Nixbox disk strategy

## 상황

- nixbox에서 여러 서비스를 돌리고 있음
- 아래의 디스크를 장착한 상태
  - 1TB SSD (OS)
  - 14TB HDD
  - 2TB HDD
- 서비스별로 데이터 용량과 접근 패턴이 다름. 같은 서비스라도 데이터에 따라 다름.
  - 핫 데이터: DB, 캐시, 인덱스 (빠른 I/O 필요)
  - 콜드 데이터: 미디어 파일, 사진, 문서 (대용량, 읽기 위주)

## 선택지 및 결정

### 선택지

1. btrfs subvolumn
2. S3 Compatible
3. ZFS, LVM 등
4. container 적극 활용

### 결정

(3)을 선택하기로 함.

- (2) 모든 서비스가 S3 compatible하지 않기 때문에 제외
- (3) btrfs가 NixOS에서 설정이 더 쉽기에 제외
- (4) container보다 service로 돌리는 편이 nix로 제어가 쉬워서 제외

## 세부 내용

빠른 IO가 필요하면 아래에 위치

```
/var/lib/${service-name}
├── postgresql/           # 공용 PostgreSQL
└── ...
```

많은 용량이 필요하면 아래에 위치

```
/storage
├── @library/  # share group이 소유
├── @archives/
└── services/  # 서비스 별로 소유
    ├── @immich/
    └── ...
```

## 기대 결과

- 나중에 SSD나 HDD가 늘어나면 어떻게 대응해야할까?
