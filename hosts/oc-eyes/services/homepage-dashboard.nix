{
  lib,
  palette,
  config,
  ...
}: let
  svc = palette.oc-eyes.services;
  oc-eyes-url = palette.lib.mkMagicDnsUrl "oc-eyes";
  base-url = "https://${oc-eyes-url}";
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
                href = "https://github.com";
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
              href = "https://${oc-eyes-url}/${svc.grafana.subpath}";
              icon = "grafana.png";
              target = "_blank";
            };
          }
          {
            "Uptime Kuma" = {
              description = "Uptime monitoring & status page";
              href = "https://${oc-eyes-url}/${svc.uptime-kuma.subpath}";
              icon = "uptime-kuma.png";
              target = "_blank";
            };
          }
          {
            "VictoriaMetrics" = {
              description = "Time series database & query interface";
              href = "https://${oc-eyes-url}/${svc.victoria-metrics.subpath}";
              icon = "victoriametrics.png";
              target = "_blank";
            };
          }
        ];
      }
    ];

    listenPort = svc.homepage.port;
    allowedHosts = lib.concatStringsSep "," [
      oc-eyes-url
    ];
  };
}
