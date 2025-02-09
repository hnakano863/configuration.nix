{ config, ... }:
{

  home.file.".emacs.d/snippets/dataform-mode" = {
    source = ./dataform-mode;
    recursive = true;
  };

}
