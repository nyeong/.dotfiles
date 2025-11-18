# oc-eyes

An monitoring system on OCI(Oracle Cloud Infra).

```
nyeong@oc-eyes
--------------
OS: NixOS 25.11 (Xantusia) aarch64
Host: KVM Virtual Machine (virt-7.2)
Kernel: Linux 6.17.7
Uptime: 3 days, 13 hours, 18 mins
Packages: 3300 (nix-system), 3142 (nix-user)
Shell: zsh 5.9
Display (QEMU Monitor): 1280x800 in 15", 75 Hz
Terminal: /dev/pts/0
CPU: Neoverse-N1*4 (4)
GPU: RedHat Virtio 1.0 GPU
Memory: 1.06 GiB / 23.40 GiB (5%)
Swap: 0 B / 11.70 GiB (0%)
Disk (/): 18.18 GiB / 29.44 GiB (62%) - xfs
Locale: en_US.UTF-8
```

## Services

- [Uptime Kuma][uptime-kuma] : for uptime monitoring
- [Grafana][grafana] : for observability dashboard

[uptime-kuma]: http://100.77.212.86:4000
[grafana]: http://100.77.212.86:3000
