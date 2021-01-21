{ stdenvNoCC, fetchFromGitHub }:
stdenvNoCC.mkDerivation rec {
  pname = "iconsfordevs";
  version = "git";
  src = fetchFromGitHub {
    owner = "mirmat";
    repo = pname;
    rev = "6a0e325e057e29b341ac4733e8a05953fa614aa2";
    sha256 = "1vcwdmzhwm22xl1qaq7pq889kk0291nmhpf756404z40q824icmc";
  };

  installPhase = ''
    install -m444 -Dt $out/share/fonts/iconsfordevs fonts/*.ttf
  '';
}
