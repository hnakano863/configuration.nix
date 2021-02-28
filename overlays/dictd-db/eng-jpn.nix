{ stdenv, fetchurl }:
stdenv.mkDerivation rec {
  name = "dictd-db-eng-jpn";
  src = fetchurl {
    url = "https://download.freedict.org/dictionaries/eng-jpn/2020.10.04/freedict-eng-jpn-2020.10.04.dictd.tar.xz";
    sha256 = "DZVVPKDm7K1QackNCcbU8gz5lAaVwtTrrzz+tour/uU=";
  };
  locale = "en_UK";
  dbName = "eng-jpn";
  buildPhase = ":";
  unpackPhase = ''
    tar xf ${src}
  '';
  installPhase = ''
    mkdir -p $out/share/dictd
    cp ./eng-jpn/eng-jpn.* $out/share/dictd
    echo "en_UK" > $out/share/dictd/locale
  '';

  meta = {
    description = "dictd-db dictionary for dictd";
    platforms = stdenv.lib.platforms.linux;
  };
}
