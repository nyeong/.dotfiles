# System-level modules (NixOS/Darwin)
{
  lib,
  isDarwin,
  isLinux,
  ...
}: {
  imports =
    [
      ./base
      ./fonts
    ]
    ++ lib.optionals isDarwin [./darwin]
    ++ lib.optionals isLinux [./linux];
}
