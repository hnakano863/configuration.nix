{ fetchFromGitHub
, stdenv
, fontforge
, hack-font
, rounded-mgenplus
, noto-fonts-emoji
, dejavu_fonts
, iconsfordevs
}:
stdenv.mkDerivation rec {
  pname = "Cica";
  version = "5.0.2";
  src = fetchFromGitHub {
    owner = "miiton";
    repo = "Cica";
    rev = "v5.0.2";
    sha256 = "1r0ww3rcl4pl52r0b2ab00fkiy5zni0gwrl5f6kw4v5clxfgk7gl";
  };

  buildInputs = [ fontforge ];

  preBuild = ''
    cp ${hack-font}/share/fonts/hack/*.ttf \
      ${rounded-mgenplus}/share/fonts/rounded-mgenplus/rounded-mgenplus-1m-{regular,bold}.ttf \
      ${noto-fonts-emoji}/share/fonts/noto/NotoEmoji-Regular.ttf \
      ${dejavu_fonts}/share/fonts/truetype/DejaVuSansMono{,-Bold}.ttf \
      ${iconsfordevs}/share/fonts/iconsfordevs/iconsfordevs.ttf \
      sourceFonts/
  '';

  patches = [ ./cica.py.patch ];

  buildPhase = ''
    runHook preBuild
    fontforge -lang=py -script cica.py
    runHook postBuild
  '';

  installPhase = ''
    install -m444 -Dt $out/share/fonts/cica dist/*.ttf
  '';
}
