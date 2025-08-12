(final: prev: let
  url = "https://github.com/shaunsingh/SFMono-Nerd-Font-Ligaturized/archive/refs/heads/main.zip";
  sha256 = "0v0zsmlvfd8w1473c51426swjrjy62z8z0nybibc4jn8aanwm201";
in {
  sf-mono-liga-bin = prev.stdenvNoCC.mkDerivation {
    pname = "sf-mono-liga-bin";
    version = "dev";
    src = prev.fetchzip {
      url = url;
      sha256 = sha256;
    };
    dontConfigure = true;
    installPhase = ''
      mkdir -p $out/share/fonts/opentype
      cp -R $src/*.otf $out/share/fonts/opentype/
    '';
  };
})
