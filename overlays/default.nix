{ config }:
final: prev:
let

  nodeEnv = final.callPackage "${final.node2nix.src}/nix/node-env.nix" {
    pkgs = final;
    libtool = if final.stdenv.isDarwin then final.darwin.cctools else null;
  };

in {

  jupyterCommand = import ./jupyter-command { pkgs = prev; };

  # SKK dicts
  skkDictionariesUtf8 = prev.callPackage ./skk-dicts/utf8.nix { inherit (prev) skkDictionaries; };
  skkDictionariesUtf8Cdb = prev.callPackage ./skk-dicts/cdb.nix {
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

}
