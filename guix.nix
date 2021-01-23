{ config, pkgs, lib, ... }:
let
  genGuixBuilder = n: {
    "guixbuilder${n}" = {
      group = "guixbuild";
      extraGroups = [ "guixbuild" ];
      home = "/var/empty";
      shell = pkgs.nologin;
      description = "Guix build user ${n}";
      isSystemUser = true;
    };
  };

  genGuixBuildersFoldl =
    lib.foldl (old: elem: old // (genGuixBuilder elem)) {};
in
{
  systemd.services.guix-daemon = {
    enable = true;
    description = "Build daemon for GNU Guix";
    environment = {
      GUIX_LOCPATH = "/var/guix/profiles/per-user/root/guix-profile/lib/locale";
      LC_ALL = "en_US.utf8";
    };
    serviceConfig = {
      ExecStart = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
      RemainAfterExit = "yes";
      StandardOutput = "syslog";
      StandardError = "syslog";
      TasksMax = 8192;
    };
    wantedBy = [ "multi-user.target" ];
  };

  users.groups.guixbuild = {};
  users.users = genGuixBuildersFoldl (map toString (lib.range 1 10));
}
