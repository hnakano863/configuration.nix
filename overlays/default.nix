final: prev:
let

  nodeEnv = final.callPackage "${final.node2nix.src}/nix/node-env.nix" {
    pkgs = final;
    libtool = if final.stdenv.isDarwin then final.darwin.cctools else null;
  };

in {

  jupyterCommand = import ./jupyter-command { pkgs = prev; };

  # SKK dicts
  skktools = prev.skktools.overrideAttrs(old: {
    name = "skktools-unstable";
    src = final.skktools-unstable-src;
  });

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

  dataform-cli = (final.callPackage ./dataform-cli { inherit nodeEnv; }).package;

  look-at-me-sideways = (final.callPackage ./look-at-me-sideways { inherit nodeEnv; }).package;

  lookml-parser = (final.callPackage ./lookml-parser { inherit nodeEnv; }).package;

}
