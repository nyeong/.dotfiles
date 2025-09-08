{
  lib,
  stdenvNoCC,
  fetchurl,
  _7zz,
}:
stdenvNoCC.mkDerivation rec {
  pname = "gureum";
  version = "1.13.0-contrib";

  src = fetchurl {
    url = "https://github.com/gureum/gureum/releases/download/${version}/Gureum-${version}.pkg";
    sha256 = "0fv989l5nqjvlrdqxy98w586qjyy45gjirgf1x91vnshkplwgyy5";
  };

  nativeBuildInputs = [_7zz];

  sourceRoot = ".";

  unpackCmd = ''
    7zz x $curSrc
    find . -name "Payload~" -exec sh -c '
      cd "$(dirname "$1")"
      mv Payload~ Payload
      cat Payload | gunzip -dc | cpio -i 2>/dev/null
    ' _ {} \;
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications
    find . -name "*.app" -type d -exec cp -R {} $out/Applications/ \;

    runHook postInstall
  '';

  meta = with lib; {
    description = "Korean input method for macOS";
    longDescription = ''
      Gureum is a Korean input method for macOS that provides
      various Korean keyboard layouts and input methods.
    '';
    homepage = "https://github.com/gureum/gureum";
    license = licenses.mit;
    sourceProvenance = with sourceTypes; [binaryNativeCode];
    platforms = platforms.darwin;
    maintainers = with maintainers; [];
  };
}
