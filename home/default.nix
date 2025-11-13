{
  lib,
  isDarwin,
  isLinux,
  inputs,
  ...
}: {
  imports =
    [
      inputs.sops-nix.homeManagerModules.sops
      ./base
      ./features
    ]
    ++ lib.optionals isDarwin [./darwin]
    ++ lib.optionals isLinux [./linux];
}
