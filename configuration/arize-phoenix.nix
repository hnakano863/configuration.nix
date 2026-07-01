# Arize Phoenix (LLM 監視ツール) を Docker コンテナのシステムサービスとして常駐させ、
# 併せて Claude Code のトレース送信用環境変数を設定するモジュール。
#
# arize-phoenix は nixpkgs に無いため公式イメージ (arizephoenix/phoenix) を利用する。
# system サービスなので linger / graphical-session.target に依存せず、
# WSL ディストロ起動時 (multi-user.target) に自動起動する。
#
# トークン/コスト表示について:
#   Claude Code はトークン数をフラット属性 (input_tokens / output_tokens など) で
#   スパンに載せるが、Phoenix はコスト/トークンを OpenInference 規約
#   (llm.token_count.*) からしか読まない。そのため tokenCostTransform を有効にすると
#   Claude Code と Phoenix の間に OpenTelemetry Collector を挟み、属性を
#   llm.token_count.* にリマップして Phoenix へ転送する。
#     Claude Code --(localhost:grpcPort)--> otelcol --(phoenix:4317)--> Phoenix
{ config, lib, pkgs, ... }:

let
  cfg = config.local.services.arize-phoenix;
  useCollector = cfg.tokenCostTransform;

  # Claude Code から見た OTLP (gRPC) エンドポイント。
  # Collector 有効時は Collector が、無効時は Phoenix が grpcPort を受ける。
  endpoint = "http://localhost:${toString cfg.grpcPort}";

  # OTel Collector 設定: gen_ai/フラット属性を OpenInference の llm.token_count.* に変換。
  collectorConfig = pkgs.writeText "otelcol-config.yaml" ''
    receivers:
      otlp:
        protocols:
          grpc:
            endpoint: 0.0.0.0:4317

    processors:
      transform/openinference:
        error_mode: ignore
        trace_statements:
          - context: span
            statements:
              - set(attributes["llm.token_count.prompt"], attributes["input_tokens"]) where attributes["input_tokens"] != nil
              - set(attributes["llm.token_count.completion"], attributes["output_tokens"]) where attributes["output_tokens"] != nil
              - set(attributes["llm.token_count.prompt_details.cache_read"], attributes["cache_read_tokens"]) where attributes["cache_read_tokens"] != nil
              - set(attributes["llm.token_count.prompt_details.cache_write"], attributes["cache_creation_tokens"]) where attributes["cache_creation_tokens"] != nil

    exporters:
      otlp/phoenix:
        endpoint: phoenix:4317
        tls:
          insecure: true

    service:
      pipelines:
        traces:
          receivers: [otlp]
          processors: [transform/openinference]
          exporters: [otlp/phoenix]
  '';
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
      description = ''
        Claude Code のトレース送信先ホスト側ポート (OTLP gRPC)。
        tokenCostTransform 有効時は Collector が、無効時は Phoenix が待ち受ける。
      '';
    };

    dataVolume = lib.mkOption {
      type = lib.types.str;
      default = "phoenix-data";
      description = "SQLite データ永続化に使う Docker volume 名。";
    };

    configureClient = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Claude Code のトレース送信用環境変数をシステム全体 (environment.sessionVariables)
        に設定するか。emacs デーモン (systemd user service) は set-environment
        (= environment.sessionVariables) を source して起動するため、システムレベルに
        置くことで emacs 経由で起動される Claude Code にも設定が届く。
        home.sessionVariables はログインシェル限定でデーモン起動には届かないため使わない。
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

    tokenCostTransform = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        OpenTelemetry Collector を挟んで Claude Code のトークン数属性を Phoenix が
        読める OpenInference 規約 (llm.token_count.*) に変換するか。
        有効にすると Phoenix 上でトークン数とコストが per-trace で表示される。
      '';
    };

    collectorImage = lib.mkOption {
      type = lib.types.str;
      default = "otel/opentelemetry-collector-contrib:0.155.0";
      description = "属性変換に使う OpenTelemetry Collector イメージ (transform processor に contrib が必要)。";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    # --- 共通: Phoenix サーバー + クライアント環境変数 ---
    {
      virtualisation.oci-containers = {
        backend = "docker";
        containers.phoenix = {
          image = "arizephoenix/phoenix:${cfg.version}";
          # UI(6006) は常にホスト公開。gRPC(4317) は Collector 無効時のみホスト公開し、
          # 有効時は phoenix-net 内部で Collector からのみ受ける。
          ports = [ "${toString cfg.httpPort}:6006" ]
            ++ lib.optional (!useCollector) "${toString cfg.grpcPort}:4317";
          networks = lib.optional useCollector "phoenix-net";
          volumes = [ "${cfg.dataVolume}:/mnt/data" ];
          environment.PHOENIX_WORKING_DIR = "/mnt/data";
          # autoStart はデフォルト true → docker-phoenix.service が multi-user.target に紐づく。
        };
      };

      # クライアント: Claude Code のトレースを Phoenix (経由 Collector) に送る環境変数。
      # Phoenix はトレース収集特化のため metrics/logs は none で無効化する。
      # emacs デーモン経由でも届くよう environment.sessionVariables (= set-environment) に設定。
      environment.sessionVariables = lib.mkIf cfg.configureClient ({
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
      });
    }

    # --- Collector を挟む構成 (トークン/コスト表示用) ---
    (lib.mkIf useCollector {
      virtualisation.oci-containers.containers.otelcol = {
        image = cfg.collectorImage;
        ports = [ "${toString cfg.grpcPort}:4317" ];
        volumes = [ "${collectorConfig}:/etc/otelcol/config.yaml:ro" ];
        cmd = [ "--config=/etc/otelcol/config.yaml" ];
        networks = [ "phoenix-net" ];
        dependsOn = [ "phoenix" ];
      };

      # docker には Phoenix/Collector を繋ぐユーザーネットワークを事前作成する必要がある。
      systemd.services.init-phoenix-net = {
        description = "Create docker network for Phoenix and OTel Collector";
        after = [ "docker.service" ];
        requires = [ "docker.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig.Type = "oneshot";
        script = ''
          ${config.virtualisation.docker.package}/bin/docker network inspect phoenix-net >/dev/null 2>&1 \
            || ${config.virtualisation.docker.package}/bin/docker network create phoenix-net
        '';
      };

      # コンテナ起動はネットワーク作成後に。
      systemd.services.docker-phoenix = {
        after = [ "init-phoenix-net.service" ];
        requires = [ "init-phoenix-net.service" ];
      };
      systemd.services.docker-otelcol = {
        after = [ "init-phoenix-net.service" ];
        requires = [ "init-phoenix-net.service" ];
      };
    })
  ]);
}
