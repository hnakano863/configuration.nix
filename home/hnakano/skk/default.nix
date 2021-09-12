{ config, pkgs, ... }:
with pkgs;
let

  skk-dicts-yaskkserv2 = runCommand "skk-dicts-yaskkserv2" {} ''
    mkdir -p $out/share
    ${yaskkserv2}/bin/yaskkserv2_make_dictionary \
      --utf8 \
      --dictionary-filename=$out/share/dictionary.yaskkserv2 \
      ${skk-dicts}/share/SKK-JISYO.combined+emoji
  '';

in {

  home.file = {

    ".config/fcitx/skk/dictionary_list".text = ''
      host=localhost,port=1178,type=server,encoding=UTF-8
      file=${skk-dicts}/share/SKK-JISYO.M,mode=readonly,type=file,encoding=UTF-8
      file=${config.home.homeDirectory}/.skk-jisyo,mode=readwrite,type=file,encoding=UTF-8
    '';

    ".config/yaskkserv2/yaskkserv2.conf".text = ''
      dictionary = ${skk-dicts-yaskkserv2}/share/dictionary.yaskkserv2
      port  = 1178
      max-connection = 16
      listen-address = 0.0.0.0
      hostname-and-ip-address-for-protocol3 = localhost:127.0.0.1
      max-server-completions = 64
      google-japanese-input = disable
    '';

  };

}
