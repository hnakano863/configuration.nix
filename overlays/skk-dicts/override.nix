{ lib, fetchurl, skktools, skk-dicts }:
let

  emoji = fetchurl {
    url = "https://raw.githubusercontent.com/skk-dev/dict/8b35d07a7d2044d48b063d2774d9f9d00bb7cb48/SKK-JISYO.emoji";
    sha256 = "UHXea+reZvAquMTr0yUR5YEcHH8RVXsiJpo9Zc8zWHc=";
  };

in skk-dicts.overrideAttrs (old: {

  installPhase = old.installPhase + ''
    dst=$out/share/SKK-JISYO.emoji
    echo ";;; -*- coding: utf-8 -*-" > $dst
    ${skktools}/bin/skkdic-expr2 ${emoji} >> $dst

    dst=$out/share/SKK-JISYO.L+emoji
    echo ";;; -*- coding: utf-8 -*-" > $dst
    ${skktools}/bin/skkdic-expr2 \
      $out/share/SKK-JISYO.L + \
      $out/share/SKK-JISYO.emoji >> $dst

    dst=$out/share/SKK-JISYO.combined+emoji
    echo ";;; -*- coding: utf-8 -*-" > $dst
    ${skktools}/bin/skkdic-expr2 \
      $out/share/SKK-JISYO.combined + \
      $out/share/SKK-JISYO.emoji >> $dst
  '';

})
