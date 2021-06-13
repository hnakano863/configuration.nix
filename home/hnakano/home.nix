{ config, pkgs, lib, ... }:
{
  imports = [
    ./emacs
    ./julia
    ./polybar
    ./gnome
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "hnakano";
  home.homeDirectory = "/home/hnakano";

  home.packages = with pkgs; [
    alacritty
    firefox-bin
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    exa
    fd
    fzf
    thunderbird
    R
    ripgrep
    bat
    gimp
    texlive.combined.scheme-medium
    vlc
    python3
    emacs-all-the-icons-fonts
    pandoc
    palemoon
    vivaldi
    jupyterCommand
  ];

  home.sessionPath = [ "$HOME/.local/bin" ];
  home.sessionVariables = {
    GUIX_LOCPATH = "$HOME/.guix-profile/lib/locale";
    JULIA_PROJECT = "${./julia/environments/v1.6}";
  };

  fonts.fontconfig.enable = true;

  programs.bash.enable = true;
  programs.bash.initExtra = ''
    if [ -z $IN_NIX_SHELL ]; then
      exec fish
    fi
  '';

  # programs.emacs.enable = true;

  programs.fish = {
    enable = true;
    functions = {
      vterm_printf.body = builtins.readFile ./fish_functions/vterm_printf.fish;
      mkprof.body = builtins.readFile ./fish_functions/mkprof.fish;
      swprof.body = builtins.readFile ./fish_functions/swprof.fish;
      lsprof.body = ''
      function lsprof;
          ls $GUIX_EXTRA_PROFILES
      end
      '';
    };

    plugins = [
      {
        name = "bass";
        src = pkgs.fetchFromGitHub {
          owner = "edc";
          repo = "bass";
          rev = "df4a1ebf8c0536e4bd7b7828a4c0dcb2b7b5d22b";
          sha256 = "VBqfBhHj0OyUmDzjak7OpSNxXlB0Xp1oG31To35u/rU=";
        };
      }
    ];

    interactiveShellInit = ''
      set -gx GUIX_DEFAULT_PROFILE "$HOME/.guix-profile"
      set -gx GUIX_EXTRA_PROFILES "$HOME/.guix-extra-profiles"
      set -gx GUIX_PROFILE $GUIX_DEFAULT_PROFILE
      bass source "$GUIX_PROFILE/etc/profile"
    '';
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

  programs.rofi = {
    enable = false;
    extraConfig.modi = "drun,window";
    theme = "Pop-Dark";
    terminal = "${pkgs.alacritty}/bin/alacritty";
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
