{ pkgs, ... }:
{
  services.polybar.config."module/date" = {
    type = "internal/date";
    interval = 1;

    time = "\"%H:%M\"";
    time-alt = "\"%a, %b %d %H:%M:%S\"";

    format = "<label>";
    format-padding = 1;
    label = "%{T1}%time%%{T-}";
    label-padding = 1;
  };
}