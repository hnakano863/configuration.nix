{ runCommand
, skkDictionaries
, python3
, skktools
}:

let

  mkCdbFile = name: value: runCommand "skk-jisyo-cdb-${name}-unstable" {
    buildInputs = [ python3 ];
  } ''
    for s in $(ls ${value}/share/skk | grep SKK-JISYO); do
      python ${skktools.src}/skk2cdb.py -f cdb.tmp \
        ${value}/share/skk/$(basename s)
      install -Dm 644 cdb.tmp $out/share/skk/$(basename s).cdb
    done
  '';

in builtins.mapAttrs mkCdbFile skkDictionaries
