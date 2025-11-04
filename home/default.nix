{
  lib,
  isDarwin,
  isLinux,
  ...
}: {
  imports =
    [
      ./base
      ./features
    ]
    ++ lib.optionals isDarwin [./darwin]
    ++ lib.optionals isLinux [./linux];
}
