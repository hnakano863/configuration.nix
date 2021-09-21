{ config, pkgs, lib, ... }:
with pkgs;
{
  imports = [
    ./fonts.nix
    ./colors.nix
    ./modules
  ];
  services.polybar = {
    enable = false;

    package = polybarFull;

    config."bar/main" = {
      width = "100%";
      height = 30;
      offset-y = 0;
      bottom = true;
      fixed-center = true;

      override-redirect = false;
      enable-ipc = true;

      background = "\${colors.bg}";
      foreground = "\${colors.fg}";

      modules-left = "date emacs i3";
      modules-center = "cpu";
      modules-right = "wlan memory backlight alsa battery";

      tray-background = "\${colors.bg}";
      tray-position = "right";
      tray-maxsize = 16;
      pseudo-transparency = true;
    };

    script = ''
      polybar main &
    '';
  };
}
