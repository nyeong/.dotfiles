# .dotfiles

Dotfiles of mine. Using Nix.

## Abstract

This repository provides a declarative configuration management system for
my personal computing environments using Nix. It manages a portable
machine and home lab including cloud-hosted and on-premise servers. The
repository is publicly maintained to enforce security practices by preventing
secret inclusion, facilitate rapid system bootstrap, and contribute practical
Nix usage patterns to the community.

README.md is the only document written in English. For my convenience, other
documents are written in Korean, which is my mother tongue.

## Structure

### hosts

| hostname                         | Purpose                              |
| -------------------------------- | ------------------------------------ |
| [nyeong-air](./hosts/nyeong-air) | Portable dev machine. MacBook Air M2 |
| [nixbox](./hosts/nixbox)         | Home lab server (N150)               |
| [oc-eyes](./hosts/oc-eyes)       | OCI cloud instance                   |

Each host directory may contain a README.md with details if needed.

```
/hosts/
└── ${host-name}/
    ├── default.nix # entry point
    ├── home-manager.nix
    ├── configuration.nix
    └── hardware-configuration.nix # optional
```

Host-specific settings go in each host directory. Shared settings are extracted to `modules` and `home`.

### modules

System and home modules organized by layer.

```
/modules/
├── system/           # nixosSystem or darwinSystem context
│   ├── base/         # all platforms
│   ├── darwin/       # macOS only
│   ├── linux/        # Linux only
│   └── fonts/
└── home/             # home-manager context
    ├── base/         # all platforms
    ├── darwin/       # macOS only
    ├── linux/        # Linux only
    └── features/     # optional features (mkEnableOption)
```

- `system`: NixOS/Darwin system-level settings
- `home`: Home Manager user-level settings
- `darwin` and `linux` are imported based on the system type.
- `features` require explicit enable: `features.something.enable = true`.

Features follow this pattern:

```nix
{ ... }: let
  cfg = config.features.featureName;
in {
  options.features.featureName = {
    enable = lib.mkEnableOption "description";
    # other options as needed
  };

  config = lib.mkIf cfg.enable {
    # feature configuration here
  };
}
```

### home

Host-specific home profiles.

```
/home/
├── profiles/         # per-host home configurations
│   ├── nyeong-air.nix
│   ├── nixbox.nix
│   └── oc-eyes.nix
└── default.nix       # backward compatibility entry point
```

### utils and data

Shared utilities and data (Layer 4 - no dependencies on other layers).

```
/utils/               # pure functions
├── builders/         # mkMagicDnsUrl, scanPaths, etc.
└── agents/           # mkCursorMcpConfig, mkOpencodeMcpConfig

/data/                # pure values
├── user.nix          # username, email
├── tailscale.nix     # magicdns
├── hosts/            # host-specific config (nixbox, oc-eyes)
└── agents/           # MCP server definitions
```

### overlays

Package overrides and modifications.

```
/overlays/
├── emacs.nix
└── ...
```

### secrets

Manage secrets using `sops-nix`. `.sops.yaml` defines encryption keys and rules.

Personal settings that cannot be public are stored in a private repository (`dotfiles-private`) and injected via flake input.

```
/
├── .sops.yaml
└── secrets/
```

## Naming

- Use kebab-case for file names.
- Use camelCase for function names.

## Usage

- Formatting: `nix run .#format`
- Linting: `nix run .#lint`
- Apply configuration:
  - nyeong-air: `darwin-rebuild switch --flake .#nyeong-air`

  - nixbox: `sudo nixos-rebuild switch --flake path:.#nixbox --impure`
    Use `path:` and `--impure` because gitignored nix files need to be referenced.

  - oc-eyes: `sudo nixos-rebuild switch --flake path:.#oc-eyes --impure`

## References

- [dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
