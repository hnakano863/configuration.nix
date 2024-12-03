{ runCommand
, skkDictionaries
, skktools
}:

let

  utf8dicts = builtins.mapAttrs
    (name: value: value.override { useUtf8 = true; })
    skkDictionaries;

  combined = runCommand "skk-jisyo-combined-unstable" {

    buildInputs = [ skktools ];

  } ''
    echo ";;; -*- dicts.emojicoding: utf-8 -*-" > combined.tmp

    skkdic-expr2 \
      ${utf8dicts.l}/share/skk/SKK-JISYO.L.utf8 + \
      ${utf8dicts.emoji}/share/skk/SKK-JISYO.emoji.utf8 \
      >> combined.tmp

    install -Dm644 combined.tmp $out/share/skk/SKK-JISYO.combined.utf8
  '';

in utf8dicts // { inherit combined; }
