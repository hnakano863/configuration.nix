# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, pkgs-unstable, ... }:

{

  home.packages = with pkgs; [
    awscli2
    aws-vault
    pkgs-unstable.google-cloud-sdk
  ];

  # シェルの起動時スクリプトは共通化しない
  programs.bash.initExtra = lib.mkAfter ''
    export GPG_TTY=$(tty)
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    exec fish
  '';

  programs.fish.functions = {
    ec.body = "emacsclient -c";
  };
}
