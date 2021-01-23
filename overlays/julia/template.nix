{ stdenv
, autoPatchelfHook
, version
, sha256
}:

stdenv.mkDerivation rec {
  pname = "julia-bin";
  inherit version;

  src = builtins.fetchTarball {
    url = "https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-${version}-linux-x86_64.tar.gz";
    inherit sha256;
  };

  dontStrip = true;
  nativeBuildInputs = [ autoPatchelfHook ];
  installPhase = ''
    mkdir -p $out
    rm lib/julia/libccalltest.so.debug
    cp -a * $out
  '';
}
