{ config, pkgs, ... }:
with pkgs;
{

  home.file = {

    ".config/fcitx/skk/dictionary_list".text = ''
      file=${skk-dicts}/share/SKK-JISYO.combined+emoji,mode=readonly,type=file,encoding=UTF-8
      file=${config.home.homeDirectory}/.skk-jisyo,mode=readwrite,type=file,encoding=UTF-8
    '';

  };

}
