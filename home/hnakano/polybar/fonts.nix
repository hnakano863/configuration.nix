{ pkgs, ... }:
{
  services.polybar.config."bar/main" = {
    font-0 = "\"Iosevka Nerd Font Mono:style=Medium:size=10;3\"";
    font-1 = "\"Iosevka Nerd Font Mono:style=Bold Oblique:size=10;3\"";
    font-2 = "\"Iosevka Nerd Font:size=12;3\"";
  };
}
