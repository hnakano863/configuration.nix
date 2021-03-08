{ pkgs, ... }:
{
  services.polybar.config."module/alsa" = {
    type = "internal/alsa";

    use-ui-max = false;

    format-volume = "<label-volume>";
    format-volume-padding = 1;
    format-volume-prefix = "%{T2}VOL%{T-}";
    format-volume-prefix-foreground = "\${colors.fg-alt}";
    label-volume = "%{T1}%percentage%%%{T-}";
    label-volume-padding = 1;

    format-muted = "<label-muted>";
    format-muted-foreground = "\${colors.red}";
    format-muted-padding = 1;
    format-muted-prefix = "%{T2}VOL%{T-}";
    format-muted-prefix-foreground = "\${colors.red}";
    label-muted = "%{T1}%percentage%%%{T-}";
    label-muted-padding = 1;
  };
}
