{ runCommand
, skkDictionaries
, python
, skktools
}:

let

  mkCdbFile = name: value: runCommand "skk-jisyo-cdb-${name}-unstable" {
    buildInputs = [ python ];
  } ''
    for s in $(ls ${value}/share/skk | grep SKK-JISYO | basename); do
      python ${skktools.src}/skk2cdb.py -f cdb.tmp \
        ${value}/share/skk/$s
      install -Dm 644 cdb.tmp $out/share/skk/$s.cdb
    done
  '';

in builtins.mapAttrs mkCdbFile skkDictionaries
