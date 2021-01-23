{ trivialBuild, fetchFromGitHub }:
trivialBuild {
  pname = "initchart";
  version = "unstable";
  src = fetchFromGitHub {
    owner = "yuttie";
    repo = "initchart";
    rev = "2df1fbc965c5ef82906a0fa76b3517c5831d3581";
    sha256 = "03sk64j0063xsp6f8n8ya7y51mg415qjyjhq0q7k68gnhp2bl0hd";
  };
}
