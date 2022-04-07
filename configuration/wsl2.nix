# configuration fraction specific to wsl2.
{ config, pkgs, lib, ... }:

{
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "ja_JP.UTF-8";
  time.timeZone = "Asia/Tokyo";

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "tty";

  services.xserver.enable = true;
  services.xserver.autorun = false;
}
