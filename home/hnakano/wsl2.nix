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
    lookml-parser
    obsidian
  ];

  home.sessionVariables = {
    AWS_VAULT_BACKEND = "pass";
  };

  # https://nixos.wiki/wiki/Fish#Setting_fish_as_your_shell
  programs.bash.initExtra = lib.mkOrder 1501 ''
    if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
    then
      shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
      exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
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
