{ user, config, pkgs, ... }:

let
  xdg_configHome = "${config.users.users.${user}.home}/.config";
  xdg_dataHome = "${config.users.users.${user}.home}/.local/share";
  xdg_stateHome = "${config.users.users.${user}.home}/.local/state";
in {

              "Library/KeyBindings/DefaultkeyBinding.dict" = {
                enable = true;
                recursive = true;
                text = ''
                  {
                    "₩" = ("insertText:", "`");
                  }
                '';
              };
  ".gnupg/gpg-agent.conf" = {
    text = ''
      pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
    '';
  };
}
