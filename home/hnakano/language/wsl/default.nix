{ pkgs, ... }:
{
  imports = [
    ../python
  ];

  home.packages = with pkgs; [
    nixd
    vscode-json-languageserver
    mermaid-cli
    gcc
  ];
}
