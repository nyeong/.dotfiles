{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.gpg.enable = true;

  sops.secrets.gpg_private_key = {
    sopsFile = ../../secrets/personal.yaml;
    key = "gpg_private_key";
    path = "${config.home.homeDirectory}/.config/sops-nix/secrets/gpg_private_key";
  };

  home.activation.import-pgp-key = lib.hm.dag.entryAfter ["sopsInstallSecrets"] ''
    GPG_KEY_PATH="${config.sops.secrets.gpg_private_key.path}"
    FINGERPRINT="B8BC049D1E042935C003C1D135EF2695DD158D46"

    if [ ! -f "$GPG_KEY_PATH" ]; then
      echo "Warning: GPG key file not found at $GPG_KEY_PATH"
      exit 0
    fi

    if ! ${pkgs.gnupg}/bin/gpg --list-secret-keys "$FINGERPRINT" >/dev/null 2>&1; then
      $DRY_RUN_CMD ${pkgs.gnupg}/bin/gpg --import "$GPG_KEY_PATH"
    fi
  '';
}
