{ pkgs, ... }:
{
  imports = [
    ./julia
  ];

  home.packages = with pkgs; [
    rnix-lsp
  ];
}
