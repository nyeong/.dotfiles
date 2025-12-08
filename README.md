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

| hostname                         | Purpose                                    |
| -------------------------------- | ------------------------------------------ |
| [nixvm](./hosts/nixvm)           | Development VM on OrbStack (on nyeong-air) |
| [nyeong-air](./hosts/nyeong-air) | Portable dev machine. MacBook Air M2       |
| [nixbox](./hosts/nixbox)         | Home lab server (N150)                     |
| [oc-eyes](./hosts/oc-eyes)       | OCI cloud instance                         |

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

### modules and home

Shared configurations.

- `modules`: system-level settings (NixOS/Darwin)
- `home`: user-level settings (Home Manager)
- Most code in `modules` and `home` is imported automatically.
- `darwin` and `linux` are imported based on the system type.
- `features` require explicit enable: `features.something.enable = true`.

```
/home/ # home-manager context configurations
├── base/
├── linux/
├── darwin/
│   ├── ${some-module}.nix  # single file module
│   └── ${another-module}/  # module with extra files
│       ├── config/
│       └── default.nix
└── features/               # auto-imported but need explicit enable
    ├── ...
    └── dev-tools.nix       # optional tools like dev-tools go here
                            # each host decides which features to enable
/modules/ # nixosSystem or darwinSystem context
├── base/
├── linux/
└── darwin/
```

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

### overlays

Package overrides and modifications.

```
/overlays/
├── emacs.nix
└── ...
```

### palette

Host-specific settings that can be shared across all hosts.

```
/palette/
├── user-config.nix
└── default.nix # entry point
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

- Formatting: `nix format`
- Apply configuration:
  - nyeong-air: `sudo darwin-rebuild switch --flake .#nyeong-air`

  - nixbox: `sudo nixos-rebuild switch --flake path:.#hostname`
    Use `path:` because gitignored nix files need to be referenced.

  - nixvm: `sudo nixos-rebuild switch --flake .#nixvm --impure`
    Use `--impure` because OrbStack defines nix files outside the project.

## References

- [dustinlyons/nixos-config](https://github.com/dustinlyons/nixos-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
