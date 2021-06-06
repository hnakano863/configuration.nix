{ config, pkgs, lib, ... }:
with pkgs;
{
  home.packages = [
    dconf2nix
    gnome.dconf-editor
    gnome.gnome-tweak-tool
    mojave-gtk-theme
    marwaita-manjaro
    papirus-maia-icon-theme
  ];

  imports = [
    ./dconf.nix
  ];
}
