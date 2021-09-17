{ config, pkgs, lib, ... }:
# forked from github:nix-community/home-manager/master.
# the original one is by Andreas Mager (https://github.com/AndreasMager)
# home-managerのrelease-21.05には実装されていないので自分の手で簡易実装する。
with lib;
let

  cfg = config.services.trayer;

  oneOf = l: foldr types.either (head l) (tail l);

  boolToString = b: if b then "true" else "false";

  paramToString = p: if isBool p then boolToString p else toString p;

  mkParam = k: v: "--${k} ${paramToString v}";

in {

  options  = {
    services.trayer = {

      enable = mkEnableOption "trayer, the lightweight GTK2+ systray for UNIX desktops";

      package = mkOption {
        default = pkgs.trayer;
        type = types.package;
        description = "The package to use.";
      };

      settings = mkOption {
        type = with types; attrsOf (nullOr (oneOf [str int bool]));
        description = "Trayer configuration as a set of attributes.";
      };

    };
  };

  config = mkIf cfg.enable {

    home.packages = [ cfg.package ];

    systemd.user.services.trayer = let
      parameters = concatStringsSep " " (mapAttrsToList mkParam cfg.settings);
    in {

      Unit = {
        Description = "trayer -- the lightweight GTK2+ systray for UNIX desktops";
        PartOf = [ "tray.target" ];
      };

      Install.WantedBy = [ "tray.target" ];

      Service = {
        ExecStart = "${cfg.package}/bin/trayer ${parameters}";
        Restart = "on-failure";
      };

    };
  };

}
