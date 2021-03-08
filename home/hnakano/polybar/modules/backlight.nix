{ pkgs, ... }:
{
  services.polybar.config."module/backlight" = {
    type = "internal/backlight";

    card = "intel_backlight";
    format = "<label>";
    format-padding = 1;
    format-prefix = "%{T2}BRT%{T-}";
    format-prefix-foreground = "\${colors.fg-alt}";
    label = "%{T1}%percentage%%%{T-}";
    label-padding = 1;
  };
}
