{ config, pkgs, lib, ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {

      shell = "fish";

      font = {
        size = 9;
        normal.style = "Regular";
        normal.family = "HackGen35 Console";
        bold.family = "HackGen35 Console";
        italic.family = "HackGen35 Console";
      };

      colors.bright = {
        black = "#555555";
        blue = "#bd93f9";
        cyan = "#8be9fd";
        green = "#50fa7b";
        magenta = "#ff79c6";
        red = "#ff5555";
        white = "#ffffff";
        yellow = "#f1fa8c";
      };

      colors.cursor = {
        cursor = "#bbbbbb";
        text = "#ffffff";
      };

      colors.normal = {
        black = "#000000";
        blue = "#bd93f9";
        cyan = "#8be9fd";
        green = "#50fa7b";
        magenta = "#ff79c6";
        red = "#ff5555";
        white = "#bbbbbb";
        yellow = "#f1fa8c";
      };

      colors.primary = {
        background = "#1e1f29";
        foreground = "#f8f8f2";
      };

      colors.selection = {
        background = "#44475a";
        text = "#ffffff";
      };

    };
  };
}
