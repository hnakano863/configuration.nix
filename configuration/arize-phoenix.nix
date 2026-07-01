# Arize Phoenix (LLM 監視ツール) を Docker コンテナのシステムサービスとして常駐させ、
# 併せて Claude Code のトレース送信用環境変数を home-manager ユーザーに注入するモジュール。
#
# arize-phoenix は nixpkgs に無いため公式イメージ (arizephoenix/phoenix) を利用する。
# system サービスなので linger / graphical-session.target に依存せず、
# WSL ディストロ起動時 (multi-user.target) に自動起動する。
{ config, lib, pkgs, ... }:

let
  cfg = config.local.services.arize-phoenix;
  # Claude Code から見た OTLP (gRPC) エンドポイント。
  endpoint = "http://localhost:${toString cfg.grpcPort}";
in
{
  options.local.services.arize-phoenix = {
    enable = lib.mkEnableOption "Arize Phoenix サーバーと Claude Code 連携";

    version = lib.mkOption {
      type = lib.types.str;
      default = "latest";
      description = ''
        arizephoenix/phoenix の Docker イメージタグ。
        pull ポリシーはデフォルト "missing" のため、更新したいときはこの
        バージョンを上げて nixos-rebuild すればイメージが取り直される。
      '';
    };

    httpPort = lib.mkOption {
      type = lib.types.port;
      default = 6006;
      description = "Phoenix UI / OTLP over HTTP のホスト側ポート。";
    };

    grpcPort = lib.mkOption {
      type = lib.types.port;
      default = 4317;
      description = "OTLP over gRPC のホスト側ポート。Claude Code のトレース送信先。";
    };

    dataVolume = lib.mkOption {
      type = lib.types.str;
      default = "phoenix-data";
      description = "SQLite データ永続化に使う Docker volume 名。";
    };

    clientUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "hnakano" ];
      description = ''
        Claude Code のトレース送信用環境変数を注入する home-manager ユーザー名のリスト。
      '';
    };

    logSensitiveData = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        true にするとプロンプト本文・ツール詳細・ツール出力をスパンに記録する
        (OTEL_LOG_USER_PROMPTS など)。デフォルトはマスクあり。機微情報が残る点に注意。
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # --- サーバー: Phoenix を OCI コンテナのシステムサービスとして常駐 ---
    virtualisation.oci-containers = {
      backend = "docker";
      containers.phoenix = {
        image = "arizephoenix/phoenix:${cfg.version}";
        ports = [
          "${toString cfg.httpPort}:6006" # UI + OTLP over HTTP
          "${toString cfg.grpcPort}:4317" # OTLP over gRPC
        ];
        volumes = [ "${cfg.dataVolume}:/mnt/data" ];
        environment.PHOENIX_WORKING_DIR = "/mnt/data";
        # autoStart はデフォルト true → docker-phoenix.service が multi-user.target に紐づく。
      };
    };

    # --- クライアント: Claude Code のトレースを Phoenix (gRPC) に送る環境変数 ---
    # Phoenix はトレース収集特化のため metrics/logs は none で無効化する。
    home-manager.users = lib.genAttrs cfg.clientUsers (_user: {
      home.sessionVariables = {
        CLAUDE_CODE_ENABLE_TELEMETRY = "1";
        CLAUDE_CODE_ENHANCED_TELEMETRY_BETA = "1"; # 分散トレーシング(spans)を有効化
        OTEL_TRACES_EXPORTER = "otlp";
        OTEL_EXPORTER_OTLP_PROTOCOL = "grpc";
        OTEL_EXPORTER_OTLP_ENDPOINT = endpoint;
        OTEL_METRICS_EXPORTER = "none";
        OTEL_LOGS_EXPORTER = "none";
      } // lib.optionalAttrs cfg.logSensitiveData {
        OTEL_LOG_USER_PROMPTS = "1";
        OTEL_LOG_TOOL_DETAILS = "1";
        OTEL_LOG_TOOL_CONTENT = "1";
      };
    });
  };
}
