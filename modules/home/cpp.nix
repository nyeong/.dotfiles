{pkgs, ...}: let
  darwinClangdConfig = ''
    CompileFlags:
      Add:
        - -std=c++17
        - -xc++
        - -isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1
        - -isystem/Library/Developer/CommandLineTools/usr/lib/clang/17/include
        - -isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
        - -isystem/Library/Developer/CommandLineTools/usr/include
  '';
  clangdConfig = '''';
in {
  home.packages = with pkgs; [
    llvm
    ccls
    cmake-language-server
  ];

  home.file =
    if pkgs.stdenv.isDarwin
    then {
      "Library/Preferences/clangd/config.yaml".text = darwinClangdConfig;
    }
    else {
      ".config/clangd/config.yaml".text = clangdConfig;
    };
}
