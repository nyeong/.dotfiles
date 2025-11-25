# Immich - Photo management and backup
# Immich는 전용으로 Postgres를 돌리길 권장함
{
  containerConfig,
  config,
  pkgs,
  palette,
  ...
}: let
  inherit (containerConfig) puid pgid tz;
  cfg = palette.nixbox.services;
in {
  systemd.services.create-immich-network = {
    serviceConfig.Type = "oneshot";
    wantedBy = ["multi-user.target"];
    script = ''
      ${pkgs.podman}/bin/podman network ls | grep -q immich-net || \
      ${pkgs.podman}/bin/podman network create immich-net
    '';
  };

  systemd.tmpfiles.rules = [
    # SSD
    "Z /var/lib/containers/immich 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/immich/config 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/immich/postgres 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/immich/ml 2755 ${puid} ${pgid} -"
    "Z /var/lib/containers/immich/valkey 2755 999 999 -"

    # HDD
    "Z /storage/@immich 2755 ${puid} ${pgid} -"
    "Z /storage/@immich/photos 2755 ${puid} ${pgid} -"
  ];

  # Immich Server - Photo management and backup
  virtualisation.oci-containers.containers.immich-server = {
    image = "ghcr.io/immich-app/immich-server:release";
    extraOptions = [
      "--network=immich-net"
    ];
    ports = ["${toString cfg.immich.port}:2283"];
    environment = {
      # Database
      DB_HOSTNAME = "immich-postgres";
      DB_PORT = "5432";
      DB_USERNAME = "immich";
      DB_PASSWORD = "immich123";
      DB_DATABASE_NAME = "immich";

      # Redis
      REDIS_HOSTNAME = "immich-valkey";
      REDIS_PORT = "6379";
      REDIS_DB_INDEX = "1";

      IMMICH_MACHINE_LEARNING_ENABLED = "true";
      IMMICH_MACHINE_LEARNING_URL = "http://immich-ml:3003";

      # Timezone
      TZ = tz;

      # 성능 최적화 설정
      IMMICH_WORKERS_INCLUDE = "api"; # API 워커만 포함 (별도 워커 컨테이너 권장)
      IMMICH_API_METRICS_PORT = "8081"; # 메트릭 포트 분리

      # 업로드 최적화
      IMMICH_MAX_FILE_SIZE = "4GB"; # 최대 파일 크기 증가

      # 썸네일 최적화
      IMMICH_THUMBNAIL_QUALITY = "80"; # 썸네일 품질 (기본값: 80)
      IMMICH_THUMBNAIL_WEBP_SIZE = "250"; # WebP 썸네일 크기
      IMMICH_THUMBNAIL_JPEG_SIZE = "1440"; # JPEG 썸네일 크기
    };
    volumes = [
      "/storage/@immich/photos:/data"
      "/etc/localtime:/etc/localtime:ro"
    ];
    dependsOn = ["immich-postgres" "immich-ml" "immich-valkey"];
    autoStart = true;
  };

  # 별도 워커 컨테이너로 백그라운드 작업 분산 처리
  virtualisation.oci-containers.containers.immich-worker = {
    image = "ghcr.io/immich-app/immich-server:release";
    extraOptions = [
      "--network=immich-net"
    ];
    environment = {
      # Database
      DB_HOSTNAME = "immich-postgres";
      DB_PORT = "5432";
      DB_USERNAME = "immich";
      DB_PASSWORD = "immich123";
      DB_DATABASE_NAME = "immich";

      # Redis
      REDIS_HOSTNAME = "immich-valkey";
      REDIS_PORT = "6379";
      REDIS_DB_INDEX = "1";

      IMMICH_MACHINE_LEARNING_ENABLED = "true";
      IMMICH_MACHINE_LEARNING_URL = "http://immich-ml:3003";

      # Timezone
      TZ = tz;

      # 워커 전용 설정
      IMMICH_WORKERS_INCLUDE = "microservices"; # 백그라운드 작업만 처리

      # 성능 최적화
      IMMICH_MAX_FILE_SIZE = "4GB";
      IMMICH_THUMBNAIL_QUALITY = "80";
      IMMICH_THUMBNAIL_WEBP_SIZE = "250";
      IMMICH_THUMBNAIL_JPEG_SIZE = "1440";
    };
    volumes = [
      "/storage/@immich/photos:/data"
      "/etc/localtime:/etc/localtime:ro"
    ];
    dependsOn = ["immich-postgres" "immich-ml" "immich-valkey"];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-ml = {
    image = "ghcr.io/immich-app/immich-machine-learning:release";
    extraOptions = [
      "--network=immich-net"
      "--shm-size=2g" # 공유 메모리 증가로 ML 성능 향상
    ];
    environment = {
      TZ = tz;
      # ML 모델 캐싱 최적화
      TRANSFORMERS_CACHE = "/cache";
      MACHINE_LEARNING_CACHE_FOLDER = "/cache";
      # 병렬 처리 최적화 (CPU 코어 수에 맞게 조정)
      MACHINE_LEARNING_WORKERS = "4";
      MACHINE_LEARNING_WORKER_TIMEOUT = "120";
    };
    volumes = [
      "/var/lib/containers/immich/ml:/cache"
      # tmpfs로 임시 처리 파일 메모리에 저장
      # "tmpfs:/tmp:rw,noexec,nosuid,size=2g"  # 필요시 주석 해제
    ];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-postgres = {
    image = "ghcr.io/immich-app/postgres:14-vectorchord0.4.3-pgvectors0.2.0@sha256:32324a2f41df5de9efe1af166b7008c3f55646f8d0e00d9550c16c9822366b4a";
    extraOptions = [
      "--network=immich-net"
      "--shm-size=256m" # 공유 메모리 증가로 성능 향상
    ];
    environment = {
      POSTGRES_PASSWORD = "immich123";
      POSTGRES_USER = "immich";
      POSTGRES_DB = "immich";
      POSTGRES_INITDB_ARGS = "--data-checksums";
      TZ = tz;

      # PostgreSQL 성능 최적화 설정 (32GB 메모리 기준)
      POSTGRES_SHARED_PRELOAD_LIBRARIES = "vectors.so";
      POSTGRES_MAX_CONNECTIONS = "200";
      POSTGRES_SHARED_BUFFERS = "2GB"; # 메모리의 25% 정도
      POSTGRES_EFFECTIVE_CACHE_SIZE = "24GB"; # 시스템 메모리의 75%
      POSTGRES_MAINTENANCE_WORK_MEM = "512MB"; # 대용량 작업용
      POSTGRES_WORK_MEM = "64MB"; # 정렬, 해시 작업용
      POSTGRES_CHECKPOINT_COMPLETION_TARGET = "0.9";
      POSTGRES_WAL_BUFFERS = "64MB"; # WAL 버퍼 증가
      POSTGRES_DEFAULT_STATISTICS_TARGET = "500"; # 통계 정확도 향상
      POSTGRES_RANDOM_PAGE_COST = "1.1"; # SSD 최적화
      POSTGRES_EFFECTIVE_IO_CONCURRENCY = "200";
    };
    volumes = [
      "/var/lib/containers/immich/postgres:/var/lib/postgresql/data"
    ];
    autoStart = true;
  };

  virtualisation.oci-containers.containers.immich-valkey = {
    image = "docker.io/valkey/valkey:8-bookworm@sha256:a137a2b60aca1a75130022d6bb96af423fefae4eb55faf395732db3544803280";
    extraOptions = [
      "--network=immich-net"
      "--user=999:999"
    ];
    environment = {
      TZ = tz;
    };
    volumes = [
      "/var/lib/containers/immich/valkey:/data"
    ];
    cmd = [
      "valkey-server"
      "--save"
      "60"
      "1" # 60초마다 1개 이상 변경시 저장 (기본값보다 자주)
      "--maxmemory"
      "2gb" # 32GB 환경에서 넉넉하게 할당
      "--maxmemory-policy"
      "allkeys-lru" # 메모리 부족시 LRU로 제거
      "--tcp-keepalive"
      "300"
      "--timeout"
      "0"
      "--hash-max-ziplist-entries"
      "512"
      "--hash-max-ziplist-value"
      "64"
      "--list-max-ziplist-size"
      "-2"
      "--set-max-intset-entries"
      "512"
    ];
    autoStart = true;
  };
}
