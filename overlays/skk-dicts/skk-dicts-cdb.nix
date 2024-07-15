{ runCommand, tinycdb, skk-dicts }:

runCommand "skk-dicts-cdb" {} ''

  mkdir -p $out/share

  suffixes='L L+emoji combined combined+emoji'

  for s in $suffixes; do
    dict=${skk-dicts}/share/SKK-JISYO.$s

    awk '/^[^;]/ {
      s = substr($0, index($0, " ") + 1)
      print "+" length($1) "," length(s) ":" $1 "->" s
    } END {print ""}' $dict |\
      ${tinycdb}/bin/cdb -c -t - SKK-JISYO.$s.cdb
  done

  mv SKK-JISYO.*.cdb $out/share

''
