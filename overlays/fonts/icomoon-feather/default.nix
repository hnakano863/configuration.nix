{ stdenvNoCC, fetchFromGitHub }:
stdenvNoCC.mkDerivation rec {
  pname = "feather-icon-font";
  version = "4.7.0";
  src = fetchFromGitHub {
    owner = "luizbills";
    repo = pname;
    rev = "v4.7.0";
    sha256 = "1l58n508vig32cg2301k3p7049f4ryv07mjvbykns5w3snzxcr0d";
  };

  installPhase = ''
    install -m444 -Dt $out/share/fonts/icomoon-feather.ttf dist/fonts/*.ttf
  '';
}
