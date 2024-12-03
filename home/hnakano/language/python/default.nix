{ config, pkgs, lib, ... }:
with pkgs;
let
  globalPython = python3.withPackages (ps:
    with ps; [
      ipython
      numpy
      matplotlib
    ]
  );
in {
  home.packages = [
    globalPython
    pyright
    poetry
  ];

  home.file.".ipython/profile_default/ipython_config.py".source =
    ./profile_default/ipython_config.py;
}
