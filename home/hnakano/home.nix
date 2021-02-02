{ config, pkgs, lib, ... }:
{
  imports = [
    # pkgs.nur.repos.rycee.hmModules.emacs-init
    ./emacs
    ./julia
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "hnakano";
  home.homeDirectory = "/home/hnakano";

  home.packages = with pkgs; [
    firefox-bin
    exa
    fd
    fzf
    ripgrep
    bat
    gimp
    texlive.combined.scheme-medium
    vlc
  ];

  home.sessionPath = [ "$HOME/.local/bin" "$HOME/.guix-profile/bin"];
  home.sessionVariables = {
    GUIX_PROFILE = "$HOME/.guix-profile";
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
  };

  fonts.fontconfig.enable = true;

  programs.bash.enable = true;
  programs.bash.initExtra = ''
    exec fish
  '';

  # programs.emacs.enable = true;

  programs.fish = {
    enable = true;
    functions = {
      vterm_printf = {
        body = builtins.readFile ./fish_functions/vterm_printf.fish;
      };
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "hnakano863";
    userEmail = "notchi863@gmail.com";
    ignores = [ "*~" "*.swp" ];
    extraConfig = {
      core.askPass = "";
      pull.rebase = false;
      init.defaultBranch = "main";
    };
  };

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-nix ];
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
