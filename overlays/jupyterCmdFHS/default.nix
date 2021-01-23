final: prev:
let
  jupyterPath = prev.jupyter-kernel.create { };

  pyPkgOverrides = pyfinal: pyprev: {
    jupyter_core = pyprev.jupyter_core.overridePythonAttrs (
      old: {
        makeWrapperArgs = [
          "--set JUPYTER_PATH ${jupyterPath}"
        ];
      }
    );
    notebook = pyprev.notebook.overridePythonAttrs (
      old: {
        doCheck = false;
      }
    );
  };

  customPython = prev.python3.override {
    packageOverrides = pyPkgOverrides;
  };

  pythonWithJupyter = customPython.withPackages (
    ps: [ ps.notebook ]
  );
in
prev.buildFHSUserEnv {
  name = "jupyter-command";
  targetPkgs = pkgs: with pkgs; [ pythonWithJupyter ];
  runScript = "jupyter";
}
