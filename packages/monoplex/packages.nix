(final: prev:
  let
    url =
      "https://github.com/y-kim/monoplex/releases/download/v0.0.2/MonoplexKR-v0.0.2.zip";
    sha256 = "0qcr8p50pm0yva05511j874vfdhi05nsp2sj9h9qnlncganqasc0";
  in {
    monoplex-bin = prev.stdenvNoCC.mkDerivation {
      pname = "monoplex-bin";
      version = "dev";
      src = prev.fetchzip {
        url = url;
        sha256 = sha256;
        stripRoot = false;
      };
      dontConfigure = true;
      installPhase = ''
        mkdir -p $out/share/fonts/truetype
        cp -R $src/**/*.ttf $out/share/fonts/truetype/
      '';
    };
  })
