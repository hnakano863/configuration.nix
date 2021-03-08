{ pkgs, ... }:
{
  services.polybar.config."module/emacs" = {
    type = "custom/script";
    interval = 5;

    exec = "${pkgs.systemd}/bin/systemctl --user is-active emacs.service | ${pkgs.coreutils}/bin/tr a-z A-Z";
    format-padding = 1;
    format-prefix = "%{T2}EMACS%{T-}";
    format-prefix-foreground = "\${colors.fg-alt}";
    label = "%{T1}%output%%{T-}";
    label-padding = 1;
  };
}
