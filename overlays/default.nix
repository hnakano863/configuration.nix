final: prev: {

  # fonts
  cica = final.callPackage ./fonts/cica {};
  iconsfordevs = final.callPackage ./fonts/iconsfordevs {};
  feather-icon-font = final.callPackage ./fonts/icomoon-feather {};

  # polybar
  polybar = prev.polybar.override {
    i3Support = true;
    jsoncpp = final.jsoncpp;
  };

  # jupyter-command
  jupyterCmdFHS = import ./jupyterCmdFHS final prev;

  vivaldi = (prev.vivaldi.override {
    proprietaryCodecs = true;
    inherit (prev) vivaldi-ffmpeg-codecs;
  }).overrideAttrs (
    old: rec {
      version = "3.6.2165.40-1";
      src = builtins.fetchurl {
        url = "https://downloads.vivaldi.com/stable/vivaldi-stable_${version}_amd64.deb";
        sha256 = "12l6xdlgnfv5apv92jlyz324crmm8r3di421ql424bj53j6bsq3y";
      };
    }
  );

  # dictd-db
  dictdDBs = prev.dictdDBs // {
    eng2jpn = final.callPackage ./dictd-db/eng-jpn.nix {};
  };
} // (import ./julia final prev)
