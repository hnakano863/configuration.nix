{ lib, rustPlatform, fetchFromGitHub, pkg-config, openssl }:
rustPlatform.buildRustPackage rec {
  pname = "yaskkserv2";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "wachikun";
    repo = pname;
    rev = "705fc240e3c9d3007b61662a84ea84a450817628";
    sha256 = "Bbg1oDjiK4xspoS+3rHmpjc5+nV2dHTA1c6HHHrSNhM=";
  };

  nativeBuildInputs = [
    pkg-config
    openssl
    openssl.dev
  ];

  cargoSha256 = "JCU3A+l1OogHGs2HpU7NWe0JReZN5WSTmvKKfs+L5AE=";

  PKG_CONFIG_PATH = "${openssl.dev}/lib/pkgconfig";

  doCheck = false;

  meta = with lib; {
    description = "Yet Another SKK server";
    homepage = "https://github.com/wachikun/yaskkserv2";
    license = licenses.asl20;
  };
}
