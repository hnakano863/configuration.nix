final: prev: {

  jupyterCommand = import ./jupyter-command { pkgs = prev; };

  # for skk server
  skk-dicts = final.callPackage ./skk-dicts/override.nix { inherit (prev) skk-dicts; };
  skk-dicts-cdb = final.callPackage ./skk-dicts/skk-dicts-cdb.nix {};
  yaskkserv2 = final.callPackage ./skk-dicts/yaskkserv2.nix {};
  dbskkd-cdb = final.callPackage ./skk-dicts/dbskkd-cdb.nix {};

  # my vivaldi
  myVivaldi = prev.vivaldi.override {
    proprietaryCodecs = true;
    enableWidevine = true;
    inherit (prev) vivaldi-ffmpeg-codecs widevine-cdm;
  };
}
