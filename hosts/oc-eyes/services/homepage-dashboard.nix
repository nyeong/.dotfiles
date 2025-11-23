{
  lib,
  palette,
  config,
  ...
}: let
  cfg = palette.oc-eyes;
  base-url = "https://${cfg.url}";
in {
  services.homepage-dashboard = {
    enable = true;

    settings = {
      title = "OC Eyes Dashboard";
      startUrl = base-url;
      base = base-url;
      theme = "dark";
      color = "slate";
      headerStyle = "boxed";
      layout = {
        "System Info" = {
          style = "row";
          columns = 2;
          icon = "mdi-information-outline";
        };
        "Monitoring & Observability" = {
          style = "row";
          columns = 3;
          icon = "mdi-monitor-dashboard";
        };
        "Quick Links" = {
          style = "row";
          columns = 3;
          icon = "mdi-link-variant";
        };
      };
    };

    bookmarks = [
      {
        "Development" = [
          {
            "GitHub" = [
              {
                abbr = "GH";
                href = "https://github.com/nyeong";
                icon = "github.png";
              }
            ];
          }
          {
            "NixOS Search" = [
              {
                abbr = "NS";
                href = "https://search.nixos.org";
                icon = "https://nixos.org/logo/nixos-logo-only-hires.png";
              }
            ];
          }
        ];
      }
      {
        "Resources" = [
          {
            "Tailscale Admin" = [
              {
                abbr = "TS";
                href = "https://login.tailscale.com/admin";
                icon = "tailscale.png";
              }
            ];
          }
        ];
      }
    ];

    docker = {};
    kubernetes = {};

    widgets = [
      {
        logo = {
          icon = "https://nixos.org/logo/nixos-logo-only-hires.png";
        };
      }
      {
        greeting = {
          text_size = "xl";
          text = "Welcome to OC Eyes";
        };
      }
      {
        resources = {
          backend = "resources";
          expanded = true;
          cpu = true;
          memory = true;
          disk = "/";
          uptime = true;
        };
      }
      {
        datetime = {
          text_size = "l";
          format = {
            timeStyle = "short";
            dateStyle = "short";
            hour12 = false;
          };
        };
      }
      {
        search = {
          provider = "google";
          target = "_blank";
        };
      }
    ];

    services = [
      {
        "Monitoring & Observability" = [
          {
            "Grafana" = {
              description = "Metrics visualization & dashboards";
              href = "https://${cfg.url}/${cfg.services.grafana.subpath}";
              icon = "grafana.png";
              target = "_blank";
            };
          }
          {
            "Gatus" = {
              description = "Health monitoring & status dashboard";
              href = "https://${cfg.url}/${cfg.services.gatus.subpath}";
              icon = "gatus.png";
              target = "_blank";
            };
          }
          {
            "VictoriaMetrics" = {
              description = "Time series database & query interface";
              href = "https://${cfg.url}/${cfg.services.victoria-metrics.subpath}";
              icon = "victoriametrics.png";
              target = "_blank";
            };
          }
        ];
      }
    ];

    listenPort = cfg.services.homepage.port;
    allowedHosts = lib.concatStringsSep "," [
      cfg.url
    ];
  };
}
