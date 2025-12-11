# oc-eyes

OCI에 무료 인스턴스로 돌아가는 ARM 시스템. 모니터링 서비스 역할.

- 4 OCPU
- 24GB RAM

```
nyeong@oc-eyes
--------------
OS: NixOS 25.11 (Xantusia) aarch64
Host: KVM Virtual Machine (virt-7.2)
Kernel: Linux 6.17.7
CPU: Neoverse-N1*4 (4)
GPU: RedHat Virtio 1.0 GPU
Memory: 1.06 GiB / 23.40 GiB (5%)
Swap: 0 B / 11.70 GiB (0%)
Disk (/): 18.18 GiB / 29.44 GiB (62%) - xfs
```

## Services

- [Homepage][homepage] : dashboard
- [Grafana][grafana] : observability dashboard
- [Gatus][gatus] : health monitoring
- [VictoriaMetrics][victoria-metrics] : time series database
- [VictoriaLogs][victoria-logs] : log database

[homepage]: https://oc-eyes.ts.net
[grafana]: https://oc-eyes.ts.net/monitor
[gatus]: https://status.ts.net
[victoria-metrics]: https://oc-eyes.ts.net/vmdb
[victoria-logs]: https://oc-eyes.ts.net/logs

## 주의

- `/boot`가 100MB로 작게 할당되었음 (Oracle Linux 9 이미지로 깔면 이렇더라)
- `/boot` 용량 부족으로 `nix switch`가 안된다면, 관리자 권한으로 `nix-collect-garbage`하고, `/boot` 날린 후 `nix switch` 할 것
