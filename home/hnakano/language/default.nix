{ pkgs, ... }:
{
  imports = [
    ./julia
    ./python
  ];

  home.packages = with pkgs; [
    rnix-lsp
    deno
    sbcl
    clisp
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
  ];
}
