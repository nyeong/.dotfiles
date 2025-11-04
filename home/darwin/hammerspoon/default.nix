{...}: {
  home.file.".config/hammerspoon" = {
    source = ./config;
    recursive = true;
  };
}
