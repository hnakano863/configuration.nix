# configuration fraction specific to wsl2.
{ config, pkgs, lib, ... }:

{
  imports = [
    ./common.nix
    ./arize-phoenix.nix
    ./esa-mcp.nix
  ];

  # wsl
  wsl = {
    enable = true;
    defaultUser = "hnakano";
    wslConf.automount.root = "/mnt";
    interop = { register = true; includePath = false; };
  };

  # extra settings for WSL2
  nix.settings = {
    keep-outputs = false;
    keep-derivations = false;
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "ja_JP.UTF-8";
  time.timeZone = "Asia/Tokyo";
  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-gtk
    ];
  };

  # WSL2 ではブート時に正規のログインセッションが張られず、
  # systemd ユーザーインスタンス (user@<uid>.service) が起動しないため、
  # linger を有効化してブート時にユーザーサービス (default.target) を起動させる。
  users.users.hnakano.linger = true;

  # services.xserver.enable = true により startWithGraphical がデフォルト true になり、
  # emacs.service が graphical-session.target に紐付く。WSL2 ではこのターゲットが
  # 起動しないため emacs daemon が自動起動しない。default.target に紐付け直す。
  services.emacs.startWithGraphical = false;

  services.xserver.enable = true;
  services.xserver.autorun = true;

  # vscode
  services.vscode-server.enable = true;

  # Arize Phoenix (LLM 監視) サーバー + Claude Code 連携。定義は ./arize-phoenix.nix。
  # configureClient は既定 true でシステム全体に OTEL 環境変数を設定する。
  local.services.arize-phoenix = {
    enable = true;
    version = "17.14.0";
  };

  # home-manager configuration
  home-manager.users.hnakano = { config, pkgs, lib, ... }: {
    imports = [ ../home/hnakano/wsl2.nix ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
