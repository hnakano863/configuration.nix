mkdir tmp
cd ./tmp
wget "$(npm view @packageName@ dist.tarball)"
tar xf "$(echo '@package@' | cut -d'@' -f1)"-*.tgz
cd ./package
node2nix
sed -i 's/args = {/args = rec {/' node-packages.nix
sed -i 's:src = ./.:src = import ./src.nix { inherit fetchurl version; }:' node-packages.nix
cd ../..
mv ./tmp/package/node-packages.nix ./default.nix
rm -rf ./tmp
