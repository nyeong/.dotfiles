{palette, ...}: {
  imports = palette.lib.scanPaths ./.;

  # ₩를 `으로 대체
  home.file."/Library/KeyBindings/DefaultKeyBinding.dict".text = ''
    {
        "₩" = ("insertText:", "`");
    }
  '';
}
