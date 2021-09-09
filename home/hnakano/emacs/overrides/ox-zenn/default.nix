{ trivialBuild, fetchFromGitHub }:

trivialBuild {

  pname = "ox-zenn";
  version = "unstable";

  src = fetchFromGitHub {

    owner = "conao3";
    repo = "ox-zenn.el";
    rev = "b53bd82116c9f7dbb5b476d2cfcc8ed0f3bc9c78";
    sha256 = "sha256-gWasQYoLrRRDxO8fOieBI7lIvYaSMGgbjuWNc/Vfp9g=";

  };

  preBuild = "rm ox-zenn-tests.el";

}
