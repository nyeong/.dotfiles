{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
}:
stdenv.mkDerivation rec {
  pname = "cursor-agent";
  version = "2025.08.25-896bbe1";

  src = fetchurl {
    url = "https://downloads.cursor.com/lab/${version}/${
      if stdenv.isLinux
      then "linux"
      else "darwin"
    }/${
      if stdenv.hostPlatform.isAarch64
      then "arm64"
      else "x64"
    }/agent-cli-package.tar.gz";

    sha256 = "sha256-rOcE8FMxTI+iogIziwckAsMZI8VZDCJMy3n2B2h5v7c=";
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  unpackPhase = ''
    runHook preUnpack
    mkdir source
    tar -xzf $src -C source --strip-components=1
    cd source
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/${pname}
    cp -r * $out/lib/${pname}/

    chmod +x $out/lib/${pname}/cursor-agent
    chmod +x $out/lib/${pname}/node
    chmod +x $out/lib/${pname}/rg

    makeWrapper $out/lib/${pname}/cursor-agent $out/bin/cursor-agent --chdir $out/lib/${pname}

    runHook postInstall
  '';

  meta = with lib; {
    description = "Cursor Agent CLI tool";
    homepage = "https://cursor.com";
    license = licenses.unfree;
    maintainers = [];
    platforms = platforms.linux ++ platforms.darwin;
    sourceProvenance = with sourceTypes; [binaryNativeCode];
  };
}
