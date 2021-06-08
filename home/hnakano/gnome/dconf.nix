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

    "org/gnome/Music" = {
      window-maximized = true;
      window-position = [ 0 28 ];
      window-size = [ 1366 700 ];
    };

    "org/gnome/calendar" = {
      active-view = "month";
      window-maximized = true;
      window-position = mkTuple [ 0 28 ];
      window-size = mkTuple [ 1366 740 ];
    };

    "org/gnome/control-center" = {
      last-panel = "mouse";
    };

    "org/gnome/desktop/applications/terminal" = {
      exec = "alacritty";
      exec-arg = "";
    };

    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      picture-uri = "file:///home/hnakano/.local/share/backgrounds/2021-06-06-16-16-16-frog_wall_paper.png";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/calendar" = {
      show-weekdate = false;
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
      application-children = [ "vivaldi-stable" "gnome-power-panel" "org-gnome-epiphany" "org-gnome-geary" "thunderbird" ];
    };

    "org/gnome/desktop/notifications/application/emacs" = {
      application-id = "emacs.desktop";
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

    "org/gnome/desktop/notifications/application/thunderbird" = {
      application-id = "thunderbird.desktop";
    };

    "org/gnome/desktop/notifications/application/vivaldi-stable" = {
      application-id = "vivaldi-stable.desktop";
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/screensaver" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      picture-uri = "file:///home/hnakano/.local/share/backgrounds/2021-06-06-16-16-16-frog_wall_paper.png";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
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
      columns-order = [ 0 1 2 3 4 5 6 ];
      sort-col = 0;
      sort-order = 1;
    };

    "org/gnome/gnome-system-monitor/proctree" = {
      columns-order = [ 0 1 2 3 4 6 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ];
      sort-col = 8;
      sort-order = 0;
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      focus-change-on-pointer-rest = true;
      workspaces-only-on-primary = true;
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
      search-filter-time-type = "last_modified";
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 890 550 ];
      maximized = false;
      sidebar-width = 199;
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
      app-picker-layout = "[{'org.gnome.Geary.desktop': <{'position': <0>}>, 'org.gnome.Contacts.desktop': <{'position': <1>}>, 'org.gnome.Weather.desktop': <{'position': <2>}>, 'org.gnome.clocks.desktop': <{'position': <3>}>, 'org.gnome.Maps.desktop': <{'position': <4>}>, 'Alacritty.desktop': <{'position': <5>}>, 'org.gnome.Totem.desktop': <{'position': <6>}>, 'org.gnome.Calculator.desktop': <{'position': <7>}>, 'ca.desrt.dconf-editor.desktop': <{'position': <8>}>, 'org.gnome.gedit.desktop': <{'position': <9>}>, 'simple-scan.desktop': <{'position': <10>}>, 'gnome-control-center.desktop': <{'position': <11>}>, 'gnome-system-monitor.desktop': <{'position': <12>}>, 'emacs.desktop': <{'position': <13>}>, 'org.gnome.Terminal.desktop': <{'position': <14>}>, 'org.gnome.Characters.desktop': <{'position': <15>}>, 'yelp.desktop': <{'position': <16>}>, 'fcitx.desktop': <{'position': <17>}>, 'org.gnome.Screenshot.desktop': <{'position': <18>}>, 'org.gnome.Cheese.desktop': <{'position': <19>}>, 'org.gnome.font-viewer.desktop': <{'position': <20>}>, 'fcitx-configtool.desktop': <{'position': <21>}>, 'firefox.desktop': <{'position': <22>}>}, {'fish.desktop': <{'position': <0>}>, 'gimp.desktop': <{'position': <1>}>, 'julia.desktop': <{'position': <2>}>, 'nixos-manual.desktop': <{'position': <3>}>, 'palemoon.desktop': <{'position': <4>}>, 'org.gnome.tweaks.desktop': <{'position': <5>}>, 'vlc.desktop': <{'position': <6>}>, 'xterm.desktop': <{'position': <7>}>, 'org.gnome.FileRoller.desktop': <{'position': <8>}>, 'org.gnome.Tour.desktop': <{'position': <9>}>, 'org.gnome.DiskUtility.desktop': <{'position': <10>}>, 'org.gnome.baobab.desktop': <{'position': <11>}>, 'org.gnome.Evince.desktop': <{'position': <12>}>, 'org.gnome.seahorse.Application.desktop': <{'position': <13>}>, 'org.gnome.Logs.desktop': <{'position': <14>}>, 'cups.desktop': <{'position': <15>}>, 'org.gnome.eog.desktop': <{'position': <16>}>, 'org.gnome.Extensions.desktop': <{'position': <17>}>, 'org.gnome.Connections.desktop': <{'position': <18>}>, 'org.gnome.Epiphany.desktop': <{'position': <19>}>}]";
      command-history = [ "alacritty" "r" ];
      disabled-extensions = [ "toggle-alacritty@itstime.tech" ];
      enabled-extensions = [ "user-theme@gnome-shell-extensions.gcampax.github.com" ];
      favorite-apps = [ "emacsclient.desktop" "vivaldi-stable.desktop" "org.gnome.Nautilus.desktop" "thunderbird.desktop" "Alacritty.desktop" "gnome-system-monitor.desktop" ];
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
