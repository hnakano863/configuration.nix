{ buildNpmPackage
, nodejs
, makeWrapper
, lib
}:

buildNpmPackage rec {
  pname = "lookml-parser";
  version = "7.1.0";

  src = ./.;

  npmDepsHash = "sha256-Ccq3e9R5O1Y9EN+yN/VlcsxZvNqk9ljlRTGFJ8BPcE0=";

  dontNpmBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/libexec/${pname} $out/bin
    cp -r node_modules $out/libexec/${pname}/  # pname固有のパスに変更

    local real_bin
    real_bin=$(readlink -f node_modules/.bin/lookml-parser)
    local rel_path="''${real_bin#$(pwd)/node_modules/}"

    makeWrapper "${nodejs}/bin/node" "$out/bin/lookml-parser" \
      --add-flags "$out/libexec/${pname}/node_modules/$rel_path"
    runHook postInstall
  '';

  meta = with lib; {
    description = "LookML parser";
    mainProgram = "lookml-parser";
    license = licenses.mit;
  };
}
