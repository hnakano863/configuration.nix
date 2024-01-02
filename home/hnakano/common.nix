{ config, pkgs, lib, ... }:
{
  imports = [
    ./emacs
    ./language
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "hnakano";
  home.homeDirectory = "/home/hnakano";

  home.packages = with pkgs; [
    eza
    fd
    fzf
    ghq
    graphviz
    R
    ripgrep
    bat
    texlive.combined.scheme-medium
    emacs-all-the-icons-fonts
    pandoc
    jupyterCommand
    plantuml
    electron
    nix-index-update
    nix-alien
    direnv
    nix-direnv
  ];

  home.sessionPath = [ "$HOME/.local/bin" ];

  home.sessionVariables = {
    GPG_TTY = "$(tty)";
  };

  fonts.fontconfig.enable = true;

  programs.bash = {
    enable = true;
    initExtra = lib.mkAfter ''
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    '';
  };
  xdg.configFile."direnv/direnvrc".text = ''
    source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
  '';

  # programs.emacs.enable = true;

  programs.fish = {
    enable = true;
    functions = {
      vterm_printf.body = builtins.readFile ./fish_functions/vterm_printf.fish;

      gfz.body = "cd (ghq list -p | fzf)";
      grt.body = "cd (ghq root)'/github.com/hnakano'";
      pluto.body =
        "julia --startup-file=no --project=@. -e 'using Pluto; Pluto.run(launch_browser=false)'";
      ll.body = "ls -lh";
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
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    settings.command_timeout = 2000;
  };

  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    extraConfig = {
      core.askPass = "";
      pull.rebase = false;
      init.defaultBranch = "main";
      github.user = "hnakano863";
    };
  };

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-nix ];
  };

  programs.nix-index = {
    enable = true;
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
