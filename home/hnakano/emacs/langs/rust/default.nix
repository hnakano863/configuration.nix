{ config, pkgs, lib, ... }:
{
  programs.emacs.init.usePackage = {
    rust-mode = {
      enable = true;
      mode = [ ''"\\.rs\\'"'' ];
    };

    flycheck.hook = [ "(rust-mode . flycheck-mode)" ];
    flycheck-rust.enable = true;
    flycheck-rust.hook = [ "(rust-mode . flycheck-rust-setup)" ];

    lsp-mode.hook = [ "(rust-mode . lsp-deffered)" ];
    lsp-mode.extraConfig = ''
      :custom
      (lsp-rust-rls-server-command "${pkgs.rls}/bin/rls")
      (lsp-rust-server "${pkgs.rust-analyzer}/bin/rust-analyzer")
      (lsp-rust-analyzer-server-command "${pkgs.rust-analyzer}/bin/rust-analyzer")
    '';
  };
}
