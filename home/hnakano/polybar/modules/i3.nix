{ pkgs, ... }:
{
  services.polybar.config."module/i3" = {
    type = "internal/i3";
    index-sort = true;

    format = "<label-state> <label-mode>";

    label-mode = "%mode%";
    label-mode-padding = 1;
    label-mode-foreground = "\${colors.fg}";

    label-focused = "%{T2}%index%%{T-}";
    label-focused-foreground = "\${colors.fg}";
    label-focused-padding = 1;

    label-unfocused = "%{T2}%index%%{T-}";
    label-unfocused-foreground = "\${colors.fg-alt}";
    label-unfocused-padding = 1;

    label-visible = "%{T2}%index%%{T-}";
    label-visible-foreground = "\${colors.fg-alt}";
    label-visible-padding = 1;

    label-urgent = "%{T2}%index%%{T-}";
    label-urgent-foreground = "\${colors.red}";
    label-urgent-padding = 1;
  };
}
