{ pkgs, ... }:
with pkgs;
{
  imports = [
    ./julia
  ];
}
