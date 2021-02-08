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

  # julia
  julia-1_5_2 = final.callPackage ./julia/template.nix {
    version = "1.5.2";
    sha256 = "0k3bpw8v07b70z50dyvjgz5wc58bnvw505lfcssczmhpfqd5zj49";
  };

  julia-bin = final.callPackage ./julia/template.nix {
    version = "1.5.3";
    sha256 = "1yc60dl39sa0rbiaj2v7vf79j0c9zd93h1mwcahq99w44c81k3q6";
  };

  # jupyter-command
  jupyterCmdFHS = import ./jupyterCmdFHS final prev;

  vivaldi = (prev.vivaldi.override {
    proprietaryCodecs = true;
    inherit (prev) vivaldi-ffmpeg-codecs;
  }).overrideAttrs (
    old: rec {
      version = "3.6.2165.36-1";
      src = builtins.fetchurl {
        url = "https://downloads.vivaldi.com/stable/vivaldi-stable_${version}_amd64.deb";
        sha256 = "1wgxzggy5sg98k4lzd34k4hyw2jgc14db41z7s7j3c5whlnifh08";
      };
    }
  );
}
