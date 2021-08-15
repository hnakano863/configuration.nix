{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.programs.emacs.orgProtocol;
  desktopApplicationFile = pkgs.writeTextFile {
    name = "org-protocol.desktop";
    destination = "/share/applications/org-protocol.desktop";
    text = ''
      [Desktop Entry]
      Name=org-protocol
      Exec=emacsclient %u
      Type=Application
      Terminal=false
      Categories=System;
      MimeType=x-scheme-handler/org-protocol;
    '';
  };
in
{
  options.programs.emacs.orgProtocol = {
    enable = mkEnableOption "enable org-protocol support";
  };

  config = mkIf (config.programs.emacs.enable && cfg.enable) {
    home.packages = [ desktopApplicationFile ];
  };
}
