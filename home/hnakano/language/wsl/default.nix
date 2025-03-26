{ pkgs, ... }:
{
  imports = [
    ../python
  ];

  home.packages = with pkgs; [
    nixd
    nodePackages.vscode-json-languageserver
    nodePackages.mermaid-cli
    gcc
  ];
}
