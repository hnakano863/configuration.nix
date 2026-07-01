# esa MCP サーバー (Claude Code / Gemini CLI 用) のトークンを agenix で管理し、
# トークンを注入するラッパー `esa-mcp-server` を提供するモジュール。
#
# 方針:
#   - 宣言的構成 (このリポジトリ) は GitHub 公開しているため、機密は一切載せない。
#   - agenix で暗号化した secrets/esa-env.age (dotenv 形式) だけを Git 管理し、
#     システム起動時に /run/agenix/esa-env (owner=hnakano, 0400) へ復号する。
#   - Claude Code の MCP コマンドはこのラッパーを呼ぶだけにし、~/.claude.json 側には
#     トークンもチーム名も書かない。

{ config, pkgs, lib, ... }:

let
  # esa MCP サーバーを起動する前に、復号済み env ファイルを読み込むラッパー。
  # npx は実行時に @esaio/esa-mcp-server を取得する (nixpkgs 未収録のため)。
  esa-mcp-server = pkgs.writeShellApplication {
    name = "esa-mcp-server";
    runtimeInputs = [ pkgs.nodejs pkgs.coreutils ];
    text = ''
      # 復号済み dotenv を環境変数へ展開。
      set -a
      # shellcheck disable=SC1091
      source ${config.age.secrets."esa-env".path}
      set +a
      export LANG="ja"
      exec npx -y @esaio/esa-mcp-server "$@"
    '';
  };
in
{
  # 復号先とオーナー。Claude Code は hnakano で動くので owner を合わせる。
  age.secrets."esa-env" = {
    file = ../secrets/esa-env.age;
    owner = "hnakano";
    mode = "0400";
  };

  # 復号に使う identity。WSL では sshd を有効化しておらずホスト鍵が無いため、
  # 専用の age 秘密鍵 (リポジトリ外・Git 管理外) を指定する。
  age.identityPaths = [ "/home/hnakano/.config/agenix/identity.txt" ];

  environment.systemPackages = [ esa-mcp-server ];
}
