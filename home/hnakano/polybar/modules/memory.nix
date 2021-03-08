{ pkgs, ... }:
{
  services.polybar.config."module/memory" = {
    type = "internal/memory";
    interval = 1;

    format = "<label>";
    format-padding = 1;
    format-prefix = "%{T2}MEM%{T-}";
    format-prefix-foreground = "\${colors.fg-alt}";
    label = "%{T1}%percentage_used%%%{T-}";
    label-foreground = "\${colors.fg}";
    label-padding = 1;
  };
}
