{ pkgs, ... }:
{
  imports = [
    ./julia
  ];

  home.packages = with pkgs; [
    rnix-lsp
    nodePackages.typescript
    nodePackages.typescript-language-server
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
    python3
    nodePackages.pyright
  ];
}
