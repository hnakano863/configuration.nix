mkdir tmp
cd ./tmp
wget "$(npm view @repos@/@package@ dist.tarball)"
tar xf @package@-*.tgz
cd ./package
node2nix
sed -i 's/args = {/args = rec {/' node-packages.nix
sed -i 's:src = ./.:src = import ./src.nix { inherit fetchurl version; }:' node-packages.nix
cd ../..
mv ./tmp/package/node-packages.nix ./default.nix
rm -rf ./tmp
