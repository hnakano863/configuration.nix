{ pkgs, ... }:
{
  imports = [
    ./julia
    ./python
  ];

  home.packages = with pkgs; [
    rnix-lsp
    nodePackages.typescript
    nodePackages.typescript-language-server
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
  ];
}
