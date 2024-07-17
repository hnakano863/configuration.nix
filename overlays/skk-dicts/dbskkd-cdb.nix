{ lib
, stdenv
, fetchFromGitHub
, tinycdb
, skk-dicts-cdb
, dictname ? "SKK-JISYO.L.cdb"
}:

stdenv.mkDerivation rec {

  pname = "dbskkd-cdb";
  version = "3.00";

  src = fetchFromGitHub {
    owner = "jj1bdx";
    repo = pname;
    rev = version;
    sha256 = "PXC40IaO79sV0pVA1+lgdNLkKPQdfGBvAlvK/v1YUMY=";
  };

  patchPhase = ''
     sed -e '/^CC =/s#-I.*\b#-I${tinycdb.dev}/include#' \
       -e '/^CDBLIB =/s#/usr/local#${tinycdb.dev}#' \
       -i Makefile

     sed -e '/^#define JISYO_FILE/s#".*"#"${skk-dicts-cdb}/share/${dictname}"#' \
       -i dbskkd-cdb.c
  '';

  installPhase = ''
    mkdir -p $out/bin
    install -m 0755 dbskkd-cdb $out/bin
  '';

}
