{ config, pkgs, lib, ... }:
with lib;
{
  programs.emacs.init.usePackage = {
    nix-mode = {
      enable = true;
      mode = [ ''"\\.nix\\'"'' ];
    };

    lsp-mode.hook = [ "(nix-mode . lsp-deferred)" ];
    lsp-mode.extraConfig = ''
      :custom
      (lsp-nix-server-path "${pkgs.rnix-lsp}/bin/rnix-lsp")
    '';

    company-nixos-options = {
      enable = true;
      command = [ "company-nixos-options" ];
      after = [ "company" ];
      config = ''
        (add-to-list 'company-backends 'company-nixos-options)
      '';
    };

    smartparens.hook = [ "(nix-mode . smartparens-mode)" ];
    smartparens.config = mkAfter ''
      (sp-with-modes 'nix-mode
        (sp-local-pair "[ " " ]")
        (sp-local-pair "{ " " }")
        (sp-local-pair "( " " )"))
    '';
  };
}
