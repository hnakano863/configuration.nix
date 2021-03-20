{ pkgs, ... }:
{
  services.polybar.config."module/battery" = {
    type = "internal/battery";
    battery = "BAT0";
    adapter = "AC0";
    full-at = 98;

    format-charging = "<label-charging>";
    format-charging-padding = 1;
    format-charging-prefix = "%{T2}BAT%{T-}";
    format-charging-prefix-foreground = "\${colors.fg-alt}";
    label-charging = "%{T1}%percentage%%%{T-}";
    label-charging-padding = 1;
    label-charging-foreground = "\${colors.green}";

    format-discharging = "<label-discharging>";
    format-discharging-padding = 1;
    format-discharging-prefix = "%{T2}BAT%{T-}";
    format-discharging-prefix-foreground = "\${colors.fg-alt}";
    label-discharging = "%{T1}%percentage%%%{T-}";
    label-discharging-padding = 1;
    label-discharging-foreground = "\${colors.yellow}";

    format-full = "<label-full>";
    format-full-padding = 1;
    format-full-prefix = "%{T2}BAT%{T-}";
    format-full-prefix-foreground = "\${colors.fg-alt}";
    label-full = "%{T1}%percentage%%%{T-}";
    label-full-padding = 1;
    label-full-foreground = "\${colors.green}";
  };
}