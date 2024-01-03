# configuraion fraction specific to wsl2 home.
{ config, pkgs, lib, pkgs-unstable, ... }:

{

  home.packages = with pkgs; [
    gopass
    git-credential-gopass
    awscli2
    pkgs-unstable.google-cloud-sdk
  ];

  programs.git = {
    userName = "hnakano863";
    userEmail = "notchi863@gmail.com";
    extraConfig.credential.helper = "gopass";
  };

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
