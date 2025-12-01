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
        "Document Management" = {
          style = "row";
          columns = 2;
          icon = "mdi-file-document-multiple";
        };
        "File Management" = {
          style = "row";
          columns = 2;
          icon = "mdi-folder-multiple";
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
              href = "https://${palette.lib.mkMagicDnsUrl cfg.services.gatus.subdomain}";
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
          {
            "VictoriaLogs" = {
              description = "Log database & query interface";
              href = "https://${cfg.url}/${cfg.services.victoria-logs.subpath}";
              icon = "victoriametrics.png";
              target = "_blank";
            };
          }
        ];
      }
      {
        "Document Management" = [
          {
            "Paperless" = {
              description = "Document management & archiving system";
              href = "https://${palette.nixbox.services.paperless.url}";
              icon = "paperless.png";
              target = "_blank";
            };
          }
          {
            "Kavita" = {
              description = "E-reader & comic book server";
              href = "https://${palette.nixbox.services.kavita.url}";
              icon = "kavita.png";
              target = "_blank";
            };
          }
          {
            "Calibre-web" = {
              description = "E-book library management";
              href = "https://${palette.nixbox.url}/${palette.nixbox.services.calibre-web.subpath}";
              icon = "calibre.png";
              target = "_blank";
            };
          }
        ];
      }
      {
        "File Management" = [
          {
            "Filebrowser" = {
              description = "Web-based file manager";
              href = "https://${palette.nixbox.url}/${palette.nixbox.services.filebrowser.subpath}";
              icon = "filebrowser.png";
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
