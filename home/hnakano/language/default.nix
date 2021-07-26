{ pkgs, ... }:
{
  imports = [
    ./julia
  ];

  home.packages = with pkgs; [
    rnix-lsp
    nodePackages.typescript
    nodePackages.typescript-language-server
  ];
}
