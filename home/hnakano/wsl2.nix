# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, ... }:

let
  gcloud = pkgs.unstable.google-cloud-sdk;
  gcloudWithComp = gcloud.withExtraComponents (with gcloud.components; [
    gke-gcloud-auth-plugin
  ]);

in {

  imports = [
    ./common.nix
    ./emacs/yasnippet
    ./language/wsl
  ];

  home.packages = with pkgs; [
    awscli2
    aws-vault
    gcloudWithComp
    kubernetes-helm
    eksctl
    minikube
    dataform-cli
    look-at-me-sideways
    sqlfluff
    git-crypt
  ];

  home.sessionVariables = {
    AWS_VAULT_BACKEND = "pass";
  };

  # シェルの起動時スクリプトは共通化しない
  programs.bash.initExtra = lib.mkAfter ''
    if [ $SHLVL -eq 1 ]; then
      exec fish
    fi
  '';

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };

  programs.emacs.extraPackages = epkgs: with epkgs; [
    my-early-init
    my-init-wsl
  ];

  # xdg-open
  xdg.desktopEntries.vivaldi = {
    name = "Vivaldi";
    exec = "/mnt/c/Users/hiroshi.nakano/AppData/Local/Vivaldi/Application/vivaldi.exe";
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "vivaldi.desktop";
      "x-scheme-handler/http" = "vivaldi.desktop";
      "x-scheme-handler/https" = "vivaldi.desktop";
      "x-scheme-handler/about" = "vivaldi.desktop";
      "x-scheme-handler/unknown" = "vivaldi.desktop";
    };
  };
}
