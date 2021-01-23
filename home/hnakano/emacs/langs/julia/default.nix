{ config, pkgs, lib, ... }:
with pkgs;
{
  programs.emacs.init.usePackage = {
    julia-mode = {
      enable = true;
      mode = [ ''"\\.jl\\'"'' ];
      extraConfig = ''
        :custom
        (inferior-julia-program-name "${julia-bin}/bin/julia")
      '';
    };
  };
}
