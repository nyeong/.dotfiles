{modulesPath, ...}: {
  imports = [(modulesPath + "/profiles/qemu-guest.nix")];
  boot.tmp.cleanOnBoot = true;

  boot.loader = {
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot";
    };
    systemd-boot = {
      enable = true;
      configurationLimit = 2;
      editor = false;
    };
    grub.enable = false;
  };

  boot.initrd = {
    availableKernelModules = ["ata_piix" "uhci_hcd" "xen_blkfront"];
    kernelModules = ["nvme"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/84AA-1B9F";
    fsType = "vfat";
  };
  fileSystems."/" = {
    device = "/dev/mapper/ocivolume-root";
    fsType = "xfs";
  };
}
