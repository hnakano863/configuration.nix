{ pkgs }:
with pkgs;
let
  jupyterApp = python3.withPackages (ps: with ps; [ jupyter jupyterlab ]);
in stdenvNoCC.mkDerivation {
  name = "jupyter-command";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${jupyterApp}/bin/jupyter $out/bin/jupyter
  '';
}
