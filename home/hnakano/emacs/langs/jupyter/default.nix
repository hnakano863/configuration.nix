{ config, pkgs, lib, ... }:
with pkgs;
with lib;
{
  programs.emacs.init.usePackage = {
    jupyter = {
      enable = true;
      command = [
        "jupyter-run-repl"
        "jupyter-available-kernelspecs"
      ];
      extraConfig = ''
        :custom
        (jupyter-executable "${jupyterCmdFHS}/bin/jupyter-command")
      '';
      init = readFile ./jupyter-init.el;
    };

    ob-jupyter = {
      enable = true;
      defer = true;
      package = "jupyter";
      extraConfig = ''
        :custom
        (org-babel-default-header-args:jupyter-julia '((:kernel . "julia-1.5")
                                                       (:session . "jl")
                                                       (:async . "yes")))
        (org-babel-default-header-args:jupyter-python '((:kernel . "python3")
                                                        (:session . "py")
                                                        (:async . "yes")))
      '';
    };

    jupyter-tramp = {
      enable = true;
      package = "jupyter";
      command = [ "jupyter-tramp-file-name-p" ];
    };

    ein-jupyter = {
      enable = true;
      package = "ein";
      command = [ "ein:run" ];
      extraConfig = ''
        :custom
        (ein:jupyter-server-command "${jupyterCmdFHS}/bin/jupyter-command")
      '';
    };
  };
}
