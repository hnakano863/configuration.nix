{ config, pkgs, ... }:
# Define a user account. Don't forget to set a password with ‘passwd’.
{ users = {
    mutableUsers = false;

    groups.guixbuild = { };
    groups.video = { };

    users = {
      hnakano = {
        description = "Hiroshi Nakano";
        home = "/home/hnakano";
        isNormalUser = true;
        createHome = true;
        hashedPassword = "$6$P6rnLO2gX.Q9r0B/$Sw3TJAdNBn9E2XWmOJUndXEM.8WxkN8eEGZ3VOMQ3q.IPmfBbzyTc1bctbctbtJUG5TXDq7wH/wTbWk68Fcaf/";
        extraGroups = [ 
          "wheel"
          "networkmanager"
          "video"
        ];
      };

      root = {
        home = "/root";
        isSystemUser = true;
        hashedPassword = "$6$KU99vdgXu$FeHz/Hdi/QpCgwk9qU3dDDmxT.ObfOyH7sVQZIQa5YSAVN.pLTKGUrFd/z.tDVXIL2z20Ea/dJ36Gs7ITVo.s1";
      };

      guixbuilder1 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 1";
        isSystemUser = true;
      };

      guixbuilder2 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 2";
        isSystemUser = true;
      };

      guixbuilder3 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 3";
        isSystemUser = true;
      };

      guixbuilder4 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 4";
        isSystemUser = true;
      };

      guixbuilder5 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 5";
        isSystemUser = true;
      };

      guixbuilder6 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 6";
        isSystemUser = true;
      };

      guixbuilder7 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 7";
        isSystemUser = true;
      };

      guixbuilder8 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 8";
        isSystemUser = true;
      };

      guixbuilder9 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 9";
        isSystemUser = true;
      };

      guixbuilder10 = {
        group = "guixbuild";
        extraGroups = [ "guixbuild" ];
        home = "/var/empty";
        shell = pkgs.nologin;
        description = "Guix build user 10";
        isSystemUser = true;
      };

    };
  };
}
