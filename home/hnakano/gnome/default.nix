{ config, pkgs, lib, ... }:
{
  home.packages = [
    dconf2nix
    gnome.gnome-tweak-tool
    gnomeExtensions.toggle-alacritty
    mojave-gtk-theme
    marwaita-manjaro
    papirus-maia-icon-theme
  ];

  imports = [
    ./dconf.nix
  ];
}
