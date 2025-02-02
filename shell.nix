{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let

  updators = {

    dataform-cli = writeShellApplication {
      name = "dataform-cli-updator";
      runtimeInputs = [ node2nix nodejs wget ];
      text = builtins.readFile ./update-scripts/dataform-cli.sh;
    };

  };

in

mkShell {

  buildInputs = [
    updators.dataform-cli
  ];

}
