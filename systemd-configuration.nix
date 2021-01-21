{ config, pkgs, ... }:
{ systemd.services = {
    guix-daemon = {
      enable = true;
      description = "Build daemon for GNU Guix";
      environment = {
        GUIX_LOCPATH = "/var/guix/profiles/per-user/root/guix-profile/lib/locale";
        LC_ALL = "en_US.utf8";
      };
      serviceConfig = {
        ExecStart ="/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild"; 
        RemainAfterExit = "yes";
        StandardOutput = "syslog";
        StandardError = "syslog";
        TasksMax = 8192;
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
