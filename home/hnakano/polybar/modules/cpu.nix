{ pkgs, ... }:
{
  services.polybar.config."module/cpu" = {
    type = "internal/cpu";
    interval = 1;

    format = "<label>";
    format-padding = 1;
    format-prefix = "%{T2}CPU%{T-}";
    format-prefix-foreground = "\${colors.fg-alt}";
    label = "%{T1}%percentage-cores:2%%{T-}";
    label-padding = 1;
  };
}
