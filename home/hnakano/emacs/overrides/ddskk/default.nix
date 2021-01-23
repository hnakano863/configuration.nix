{ stdenv, emacs, fetchFromGitHub }:
stdenv.mkDerivation rec {
  pname = "ddskk";
  version = "17.1";
  src = fetchFromGitHub {
    owner = "skk-dev";
    repo = "ddskk";
    rev = "${pname}-${version}_Neppu";
    sha256 = "0rqphxakz49rl577zy6r29g1rfiqhra846cqnbbsz2j8c4lp19zg";
  };
  buildInputs = [ emacs ];
  configurePhase = ''
    echo "(setq SKK_DATADIR \"$out/share/emacs/etc\")" > SKK-CFG
    echo "(setq SKK_INFODIR \"$out/share/info\")" >> SKK-CFG
    echo "(setq SKK_LISPDIR \"$out/share/emacs/site-lisp/skk\")" >> SKK-CFG
  '';
}
