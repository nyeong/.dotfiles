{ pkgs, ... }: { environment.systemPackages = [ pkgs.nixfmt-rfc-style pkgs.nil pkgs.nixd ]; }
