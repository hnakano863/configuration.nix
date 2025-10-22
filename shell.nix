{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let

  mkNode2NixUpdator =
    { name
    , package
    , repos ? null
    , template ? ./update-scripts/node2nix-template.sh
    }:
    let
      packageName = if repos != null then "${repos}/${package}" else package;
      script = runCommand "script.sh" {
        inherit package packageName;
      } ''substituteAll "${template}" $out'';
    in writeShellApplication {
      inherit name;
      runtimeInputs = [ node2nix nodejs wget ];
      text = builtins.readFile script.out;
    };

  updators = {

    dataform-cli = mkNode2NixUpdator {
      name = "dataform-cli-updator";
      repos = "@dataform";
      package = "cli";
    };

    look-at-me-sideways = mkNode2NixUpdator {
      name = "lams-updator";
      repos = "@looker";
      package = "look-at-me-sideways@3.4.1";
    };

    lookml-parser = mkNode2NixUpdator {
      name = "lookml-parser-updator";
      package = "lookml-parser";
    };

  };

in

mkShell {

  buildInputs = [
    updators.dataform-cli
    updators.look-at-me-sideways
    updators.lookml-parser
  ];

}
