{ config, pkgs, lib, ... }:
{
  imports = [
    ./elisp
    ./fish
    ./gnuplot
    ./julia
    ./jupyter
    ./nix
    ./python
    ./restclient
    ./rust
  ];

  programs.emacs.init.usePackage = {
    smartparens.enable = true;
    smartparens.config = "(require 'smartparens-config)";

    flycheck.enable = true;
    flycheck.config = ''
      (require 'pkg-info)
    '';

    company = {
      enable = true;
      extraConfig = ''
        :general
        (:keymaps 'company-active-map
          "C-n" 'company-select-next
          "C-p" 'company-select-previous)
      '';
    };

    yasnippet.enable = true;
    yasnippet.defer = true;

    lsp-mode = {
      enable = true;
      command = [ "lsp" "lsp-deferred" ];
      hook = [ "(lsp-mode . lsp-enable-which-key-integration)" ];
      config = ''
        (require 'lsp-modeline)
      '';
      extraConfig = ''
        :custom
        (lsp-keymap-prefix "C-c C-l")
        (lsp-auto-configure t)
        :general
        (:keymaps 'lsp-mode-map
        "C-c l" '(:keymap lsp-command-map :wk "+lsp"))
      '';
    };

    lsp-headerline = {
      enable = true;
      package = "lsp-mode";
      command = [ "lsp-headerline-breadcrumb-mode" ];
    };

    lsp-ui = {
      enable = true;
      command = [ "lsp-ui-mode" ];
      extraConfig = ''
        :custom
        (lsp-ui-sideline-show-hover t)
      '';
    };

    envrc = {
      enable = true;
      config = ''
        (envrc-global-mode 1)
      '';
    };

  };
}
