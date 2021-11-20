{ config, pkgs, ... }:
# Define a user account. Don't forget to set a password with ‘passwd’.
{ users = {
    mutableUsers = false;
    groups.video = { };

    users = {
      hnakano = {
        description = "Hiroshi Nakano";
        home = "/home/hnakano";
        isNormalUser = true;
        createHome = true;
        hashedPassword = "$6$P6rnLO2gX.Q9r0B/$Sw3TJAdNBn9E2XWmOJUndXEM.8WxkN8eEGZ3VOMQ3q.IPmfBbzyTc1bctbctbtJUG5TXDq7wH/wTbWk68Fcaf/";
        shell = pkgs.fish;
        extraGroups = [
          "wheel"
          "networkmanager"
          "video"
          "docker"
        ];
      };

      root = {
        home = "/root";
        isSystemUser = true;
        hashedPassword = "$6$KU99vdgXu$FeHz/Hdi/QpCgwk9qU3dDDmxT.ObfOyH7sVQZIQa5YSAVN.pLTKGUrFd/z.tDVXIL2z20Ea/dJ36Gs7ITVo.s1";
      };
    };
  };
}
