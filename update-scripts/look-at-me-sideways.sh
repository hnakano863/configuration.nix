mkdir tmp
cd ./tmp
wget "$(npm view @looker/look-at-me-sideways dist.tarball)"
tar xf look-at-me-sideways-*.tgz
cd ./package
node2nix
sed -i 's/args = {/args = rec {/' node-packages.nix
sed -i 's:src = ./.:src = import ./src.nix { inherit fetchurl version; }:' node-packages.nix
cd ../..
mv ./tmp/package/node-packages.nix ./default.nix
rm -rf ./tmp
