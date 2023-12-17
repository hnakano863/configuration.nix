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
    (haskellPackages.ghcWithPackages (p: [ p.cabal-install ]))
    elmPackages.elm
    elmPackages.elm-language-server
    elmPackages.elm-format
    elmPackages.elm-test
    dotnet-sdk_6
    nodePackages.vscode-json-languageserver
    nodePackages.mermaid-cli
    # rust
    rustc
    cargo
    gcc
  ];
}
