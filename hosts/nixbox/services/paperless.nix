# paperless
{
  pkgs,
  config,
  palette,
  lib,
  ...
}: let
  cfg = palette.nixbox.services;
  tailscaleBin = "${pkgs.tailscale}/bin/tailscale";

  # tessdata_best: Best (most accurate) trained LSTM models for Tesseract OCR
  # https://github.com/tesseract-ocr/tessdata_best
  tessdataBest = pkgs.stdenv.mkDerivation {
    pname = "tessdata-best";
    version = "4.1.0";
    korData = pkgs.fetchurl {
      url = "https://github.com/tesseract-ocr/tessdata_best/raw/main/kor.traineddata";
      sha256 = "sha256-+IjUA4NIoMPSUVHn9FK9oNdMonWxjKsUZ5i8u5QIT/8=";
    };
    engData = pkgs.fetchurl {
      url = "https://github.com/tesseract-ocr/tessdata_best/raw/main/eng.traineddata";
      sha256 = "sha256-goCu0Hgv4nJXpo6hD+fvMkyg+Nhb0v0UXRwrVgvLZro=";
    };
    jpnData = pkgs.fetchurl {
      url = "https://github.com/tesseract-ocr/tessdata_best/raw/main/jpn.traineddata";
      sha256 = "sha256-Nr35rII/WRHmJMMNBVPokLirx8MaZbPvFNqUNljEC3k=";
    };
    osdData = pkgs.fetchurl {
      url = "https://github.com/tesseract-ocr/tessdata_best/raw/main/osd.traineddata";
      sha256 = "sha256-nPXVdvzEdWTxEmWEHlyoOQAefm84/396rPRtFalrAP8=";
    };
    jpnVertData = pkgs.fetchurl {
      url = "https://github.com/tesseract-ocr/tessdata_best/raw/main/jpn_vert.traineddata";
      sha256 = "sha256-Eli+brKphR8YBDI0rRjMoT7TJpC//2KzNciYu+o3FUg=";
    };
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/share/tessdata
      cp $korData $out/share/tessdata/kor.traineddata
      cp $engData $out/share/tessdata/eng.traineddata
      cp $jpnData $out/share/tessdata/jpn.traineddata
      cp $osdData $out/share/tessdata/osd.traineddata
      cp $jpnVertData $out/share/tessdata/jpn_vert.traineddata
      ln -s ${pkgs.tesseract}/share/tessdata/configs $out/share/tessdata/configs
      ln -s ${pkgs.tesseract}/share/tessdata/tessconfigs $out/share/tessdata/tessconfigs
    '';
    meta = {
      description = "Best (most accurate) trained LSTM models for Tesseract OCR";
      homepage = "https://github.com/tesseract-ocr/tessdata_best";
      license = lib.licenses.asl20;
      platforms = lib.platforms.all;
    };
  };
in {
  services.paperless = {
    enable = true;
    address = "0.0.0.0";
    port = cfg.paperless.port;
    settings = {
      PAPERLESS_DBENGINE = "postgresql";
      PAPERLESS_DBPORT = cfg.postgres.port;
      PAPERLESS_DBHOST = "/run/postgresql";
      PAPERLESS_DBNAME = cfg.paperless.dbname;
      PAPERLESS_DBUSER = cfg.paperless.dbuser;
      PAPERLESS_REDIS = "redis://127.0.0.1:${toString cfg.valkey.port}/${toString cfg.valkey.dbnumber.paperless}";
      PAPERLESS_REDIS_PREFIX = "paperless";
      PAPERLESS_DB_READ_CACHE_ENABLED = true;
      PAPERLESS_READ_CACHE_TTL = 3600;
      PAPERLESS_READ_CACHE_REDIS_URL = "redis://127.0.0.1:${toString cfg.valkey.port}/${toString cfg.valkey.dbnumber.paperless-read}";
      PAPERLESS_URL = "https://${cfg.paperless.url}";
      PAPERLESS_USE_X_FORWARD_HOST = true;
      PAPERLESS_USE_X_FORWARD_PORT = true;
      PAPERLESS_OCR_LANGUAGE = "kor+eng+jpn";
      PAPERLESS_OCR_OUTPUT_TYPE = "pdfa";
      PAPERLESS_OCR_IMAGE_DPI = 600;
      PAPERLESS_OCR_USER_ARGS = ''{"pdf-renderer": "sandwich", "jobs": 1, "tesseract_timeout": 300, "skip_text": true}'';
      TESSDATA_PREFIX = "${tessdataBest}/share/tessdata";
    };
  };

  systemd.services.paperless-web = {
    after = ["postgresql.service" "redis-default.service"];
    wants = ["postgresql.service" "redis-default.service"];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  systemd.services.tailscale-serve-paperless = palette.lib.mkTailscaleServeService {
    inherit tailscaleBin;
    serviceName = cfg.paperless.serviceName;
    port = cfg.paperless.port;
    webService = "paperless-web.service";
    enable = config.services.paperless.enable;
  };
}
