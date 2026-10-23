final: prev: {

  jupyterCommand = import ./jupyter-command { pkgs = prev; };

  skkDictionariesUtf8 = final.callPackage ./skk-dicts/utf8.nix { };

  dataform-cli = final.callPackage ./dataform-cli {};
  look-at-me-sideways = final.callPackage ./look-at-me-sideways {};
  lookml-parser = final.callPackage ./lookml-parser {};

}
