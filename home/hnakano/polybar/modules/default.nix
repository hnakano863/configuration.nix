{ pkgs, ... }:
{
  imports = [
    ./alsa.nix
    ./backlight.nix
    ./battery.nix
    ./cpu.nix
    ./date.nix
    ./emacs.nix
    ./i3.nix
    ./memory.nix
    ./wlan.nix
  ];
}
