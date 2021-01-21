self: super: {
  polybar = super.polybar.override {
    i3Support = true;
    jsoncpp = super.jsoncpp;
  };
}
