{ buildNpmPackage
, nodejs
, makeWrapper
, lib
}:

buildNpmPackage rec {
  pname = "dataform-cli";
  version = "3.1.2";

  src = ./.;

  npmDepsHash = "sha256-s/nS0Ms6Vt3NeLuX1kxFtS7ljhioBXKi0lbWYjD9MgI=";

  dontNpmBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/libexec/${pname} $out/bin
    cp -r node_modules $out/libexec/${pname}/   # pname固有のパスに変更

    local real_bin
    real_bin=$(readlink -f node_modules/.bin/dataform)
    local rel_path="''${real_bin#$(pwd)/node_modules/}"

    makeWrapper "${nodejs}/bin/node" "$out/bin/dataform" \
      --add-flags "$out/libexec/${pname}/node_modules/$rel_path"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Dataform CLI";
    mainProgram = "dataform";
    license = licenses.asl20;
  };
}
