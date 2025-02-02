{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let

  updators = {

    dataform-cli = writeShellApplication {
      name = "dataform-cli-updator";
      runtimeInputs = [ node2nix nodejs wget ];
      text = builtins.readFile ./update-scripts/dataform-cli.sh;
    };

    look-at-me-sideways = writeShellApplication {
      name = "lams-updator";
      runtimeInputs = [ node2nix nodejs wget ];
      text = builtins.readFile ./update-scripts/look-at-me-sideways.sh;
    };

  };

in

mkShell {

  buildInputs = [
    updators.dataform-cli
    updators.look-at-me-sideways
  ];

}
