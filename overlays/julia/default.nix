final: prev: {
  julia-1_5_2 = final.callPackage ./template.nix {
    version = "1.5.2";
    sha256 = "0k3bpw8v07b70z50dyvjgz5wc58bnvw505lfcssczmhpfqd5zj49";
  };

  julia-1_5_3 = final.callPackage ./template.nix {
    version = "1.5.3";
    sha256 = "1yc60dl39sa0rbiaj2v7vf79j0c9zd93h1mwcahq99w44c81k3q6";
  };

  julia-bin = final.callPackage ./template.nix {
    version = "1.5.4";
    sha256 = "0bb8fr692dqrf718p644y5swd3z8ivh4ysdaw7q0cir10cc16nym";
  };

}
