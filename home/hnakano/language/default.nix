{ pkgs, pkgs-unstable, ... }:
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
    idris
    (haskellPackages.ghcWithPackages (p: [ p.cabal-install ]))
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
    dotnet-sdk_6
    nodePackages.vscode-json-languageserver
    pkgs-unstable.elan
  ];
}
