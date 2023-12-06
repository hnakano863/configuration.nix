{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.programs.emacs.compileInit;
in
{
  options.programs.emacs.compileInit = {
    enable = mkEnableOption "init.el bytecompiles";

    earlyInitFile = mkOption {
      type = with types; nullOr path;
      default = null;
      example = ./early-init.el;
      description = ''
        A path of <filename>early-init.el</filename> source.
      '';
    };

    initFile = mkOption {
      type = with types; nullOr path;
      default = null;
      example = ./init.el;
      description = ''
        A path of <filename>init.el</filename> source.
      '';
    };

    earlyInitPackages = mkOption {
      type = hm.types.selectorFunction;
      default = epkgs: [ ];
      example = epkgs: [ epkgs.leaf ];
      description = ''
        Emacs packages required to bytecompile <filename>early-init.el</filename>.
      '';
    };

    initPackages = mkOption {
      type = hm.types.selectorFunction;
      default = epkgs: [ ];
      example = epkgs: [ epkgs.leaf ];
      description = ''
        Emacs packages required to bytecompile <filename>init.el</filename>.
      '';
    };
  };

  config = mkIf (config.programs.emacs.enable && cfg.enable) {
    programs.emacs.extraPackages = epkgs: [
      (epkgs.trivialBuild {
        pname = "my-early-init";
        version = "2023-12-06";
        src = cfg.earlyInitFile;
        packageRequires = cfg.earlyInitPackages epkgs;
        preferLocalBuild = true;
        allowSubstitute = false;
      })
      (epkgs.trivialBuild {
        pname = "my-init";
        version = "2023-12-06";
        src = cfg.initFile;
        packageRequires = cfg.initPackages epkgs;
        preferLocalBuild = true;
        allowSubstitute = false;
      })
    ] ++ (cfg.earlyInitPackages epkgs)
    ++ (cfg.initPackages epkgs);

    home.file = {
      ".emacs.d/early-init.el".text = ''
        (require 'my-early-init)
        (provide 'early-init)
      '';

      ".emacs.d/init.el".text = ''
        (require 'my-init)
        (provide 'init)
      '';
    };
  };
}
