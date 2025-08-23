{ config, pkgs, lib, ... }:
{
  imports = [ ./emacs ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "hnakano";
  home.homeDirectory = "/home/hnakano";

  home.packages = with pkgs; [
    babelfish
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
    nodejs_22 # for copilot.el
    terraform
    docker-credential-helpers
    kubectl
    kind
    jq
    ispell # for forge.el
    nyxt
    git-crypt
  ];

  home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

  home.sessionVariables = {
    EDITOR = "emacseditor";
  };

  fonts.fontconfig.enable = true;

  programs.bash.enable = true;

  # programs.emacs.enable = true;

  programs.fish = {
    enable = true;
    functions = {
      vterm_printf.body = builtins.readFile ./fish_functions/vterm_printf.fish;

      gfz.body = "cd (ghq list -p | fzf)";
      grt.body = "cd (git rev-parse --show-toplevel)";
      pluto.body =
        "julia --startup-file=no --project=@. -e 'using Pluto; Pluto.run(launch_browser=false)'";
      ll.body = "ls -lh";
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    settings.command_timeout = 2000;
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
      github.user = "hnakano863";
      credential.helper =
        "${pkgs.pass-git-helper.out}/bin/pass-git-helper";
    };
  };

  home.file.".config/pass-git-helper/git-pass-mapping.ini".text = ''
  [github.com*]
  target=dev/github.com
  '';

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-nix ];
  };

  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    # enableFishIntegration = true; default
    nix-direnv.enable = true;
  };

  programs.password-store.enable = true;

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
