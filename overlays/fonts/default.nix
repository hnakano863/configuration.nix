self: super: {
  cica = super.callPackage ./cica/default.nix {};
  iconsfordevs = super.callPackage ./iconsfordevs/default.nix {};
  feather-icon-font = super.callPackage ./icomoon-feather/default.nix {};
}
