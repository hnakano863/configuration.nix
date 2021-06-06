# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      saved-pathbar-path = "/org/gnome/desktop/applications/terminal/exec";
      saved-view = "/org/gnome/desktop/applications/terminal/exec";
      window-height = 500;
      window-is-maximized = false;
      window-width = 540;
    };

    "org/gnome/Geary" = {
      migrated-config = true;
      window-height = 736;
    };

    "org/gnome/control-center" = {
      last-panel = "keyboard";
    };

    "org/gnome/desktop/applications/terminal" = {
      exec = "alacritty";
      exec-arg = "";
    };

    "org/gnome/desktop/input-sources" = {
      per-window = false;
      sources = [ (mkTuple [ "xkb" "jp" ]) ];
      xkb-options = [ "ctrl:swapcaps" "lv3:ralt_switch" ];
    };

    "org/gnome/desktop/interface" = {
      font-antialiasing = "grayscale";
      font-hinting = "slight";
      gtk-im-module = "ibus";
      gtk-theme = "Marwaita Manjaro Color";
      icon-theme = "Papirus-Adapta-Maia";
    };

    "org/gnome/desktop/notifications" = {
      application-children = [ "vivaldi-stable" "gnome-power-panel" "org-gnome-epiphany" "org-gnome-geary" ];
    };

    "org/gnome/desktop/notifications/application/gnome-power-panel" = {
      application-id = "gnome-power-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-epiphany" = {
      application-id = "org.gnome.Epiphany.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-geary" = {
      application-id = "org.gnome.Geary.desktop";
    };

    "org/gnome/desktop/notifications/application/vivaldi-stable" = {
      application-id = "vivaldi-stable.desktop";
    };

    "org/gnome/epiphany" = {
      ask-for-default = false;
    };

    "org/gnome/epiphany/state" = {
      is-maximized = false;
      window-position = mkTuple [ (-1) (-1) ];
      window-size = mkTuple [ 1024 736 ];
    };

    "org/gnome/evolution-data-server" = {
      migrated = true;
      network-monitor-gio-name = "";
    };

    "org/gnome/gnome-system-monitor" = {
      current-tab = "processes";
      maximized = false;
      network-total-in-bits = false;
      show-dependencies = false;
      show-whose-processes = "user";
      window-state = mkTuple [ 700 500 ];
    };

    "org/gnome/gnome-system-monitor/disktreenew" = {
      col-6-visible = true;
      col-6-width = 0;
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      focus-change-on-pointer-rest = true;
      workspaces-only-on-primary = true;
    };

    "org/gnome/nm-applet/eap/a656f7a6-6691-4138-a2a1-689344841a9d" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Super>Return";
      command = "alacritty";
      name = "端末を起動";
    };

    "org/gnome/shell" = {
      command-history = [ "alacritty" "r" ];
      disabled-extensions = [ "toggle-alacritty@itstime.tech" ];
      enabled-extensions = [ "user-theme@gnome-shell-extensions.gcampax.github.com" ];
      welcome-dialog-last-shown-version = "40.1";
    };

    "org/gnome/shell/extensions/user-theme" = {
      name = "Mojave-dark";
    };

    "org/gnome/shell/world-clocks" = {
      locations = "@av []";
    };

    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };

    "org/gtk/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = false;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 193;
      sort-column = "name";
      sort-directories-first = false;
      sort-order = "ascending";
      type-format = "category";
      window-position = mkTuple [ 134 28 ];
      window-size = mkTuple [ 1098 700 ];
    };

  };
}
