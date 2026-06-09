final: prev: {

  jupyterCommand = import ./jupyter-command { pkgs = prev; };

  skkDictionariesUtf8 = final.callPackage ./skk-dicts/utf8.nix { };
  skkDictionariesUtf8Cdb = final.callPackage ./skk-dicts/cdb.nix {
    skkDictionaries = final.skkDictionariesUtf8;
  };


  # my vivaldi
  myVivaldi = prev.vivaldi.override {
    proprietaryCodecs = true;
    enableWidevine = true;
    inherit (prev) vivaldi-ffmpeg-codecs widevine-cdm;
  };

  dataform-cli = final.callPackage ./dataform-cli {};
  look-at-me-sideways = final.callPackage ./look-at-me-sideways {};
  lookml-parser = final.callPackage ./lookml-parser {};

}
