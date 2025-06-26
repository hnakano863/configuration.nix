{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let

  mkNode2NixUpdator =
    { name
    , repos
    , package
    , template ? ./update-scripts/node2nix-template.sh
    }:
    let
      script = runCommand "script.sh" {
        inherit repos package;
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

    gemini-cli = mkNode2NixUpdator {
      name = "gemini-cli-updator";
      repos = "@google";
      package = "gemini-cli";
    };

  };

in

mkShell {

  buildInputs = [
    updators.dataform-cli
    updators.look-at-me-sideways
    updators.gemini-cli
  ];

}
