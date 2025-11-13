{config, ...}: {
  services.telegraf = {
    enable = true;

    extraConfig = {
      # Global configuration
      global = {
        hostname = config.networking.hostName;
        interval = "10s";
        flush_interval = "10s";
      };

      # Input plugins (메트릭 수집)
      inputs = {
        # System metrics: CPU, memory, disk, etc.
        cpu = {
          percpu = true;
          totalcpu = true;
          collect_cpu_time = false;
          report_active = false;
        };

        disk = {
          ignore_fs = ["tmpfs" "devtmpfs" "devfs" "iso9660" "overlay" "aufs" "squashfs"];
        };

        diskio = {
        };

        mem = {
        };

        net = {
        };

        netstat = {
        };

        processes = {
        };

        swap = {
        };

        system = {
        };

        # SystemD service monitoring
        systemd_units = {
          unittype = "service";
          pattern = "*.service";
        };
      };

      # Output plugins: PostgreSQL time-series storage
      # Auto-schema: Telegraf creates separate table per measurement
      # (cpu, mem, disk, diskio, net, netstat, processes, swap, system, systemd_units)
      outputs = {
        postgresql = {
          address = "unix:///run/postgresql";
          database = "telegraf";
          username = "telegraf";
        };
      };
    };
  };

  # Add telegraf to postgres group (for socket access to PostgreSQL)
  users.users.telegraf.extraGroups = ["postgres"];

  # Ensure PostgreSQL is ready before starting telegraf
  systemd.services.telegraf = {
    after = ["postgresql.service"];
    requires = ["postgresql.service"];
  };
}
