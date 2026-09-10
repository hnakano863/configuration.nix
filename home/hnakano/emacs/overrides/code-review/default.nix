{ trivialBuild
, fetchFromGitHub
, closql
, magit
, transient
, a
, ghub
, uuidgen
, deferred
, markdown-mode
, forge
, emojify
}:

trivialBuild {
  src = fetchFromGitHub {
    owner = "doomelpa";
    repo = "code-review";
    rev = "303edcfbad8190eccb9a9269dfc58ed26d386ba5";
    hash = "sha256-NknisWQeaCSs5zVRmntWbOHvpPiOOiIAfdwxwb7wIiY=";
  };
  pname = "code-review";
  version = "2025-05-12";
  packageRequires = [
    closql
    magit
    transient
    a
    ghub
    uuidgen
    deferred
    markdown-mode
    forge
    emojify
  ];
  preferLocalBuild = true;
  allowSubstitute = false;
}
