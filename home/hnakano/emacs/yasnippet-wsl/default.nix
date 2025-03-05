{ config, ... }:
{

  # dataform-mode
  home.file.".emacs.d/snippets/dataform-mode" = {
    source = ./dataform-mode;
    recursive = true;
  };

  # lookml-mode
  home.file.".emacs.d/snippets/lookml-mode" = {
    source = ./lookml-mode;
    recursive = true;
  };

}
