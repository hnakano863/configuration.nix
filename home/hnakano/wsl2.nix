# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, pkgs-unstable, ... }:

{

  home.packages = with pkgs; [
    awscli2
    aws-vault
    pkgs-unstable.google-cloud-sdk
  ];

  home.sessionVariables = {
    AWS_VAULT_BACKEND = "pass";
  };

  # シェルの起動時スクリプトは共通化しない
  programs.bash.initExtra = lib.mkAfter ''
    export GPG_TTY=$(tty)
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    if [ $SHLVL -eq 1 ]; then exec fish fi
  '';

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };
}
