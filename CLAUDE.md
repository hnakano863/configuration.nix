# CLAUDE.md

## 「configurationを更新」するときの手順

`nix flake update` だけでは更新されないものがあるため、更新のたびに以下も確認すること。
`nixos-rebuild` はユーザーが実施するので、こちらでは実行しない。

1. `nix flake update` で `flake.lock` を更新する。
2. flake input 外の手動管理パッケージについて、最新版との差分を確認する(下表)。
3. 古いものがあれば、更新するか、ユーザーに提示して可否を確認する。

### flake update で更新されないもの

| 対象 | 定義場所 | 最新版の確認先 |
|---|---|---|
| arize-phoenix(Dockerイメージタグ) | `configuration/wsl2.nix` の `version` | PyPI `arize-phoenix` / Docker Hub `arizephoenix/phoenix` |
| dataform-cli(npm) | `overlays/dataform-cli/` | npm `@dataform/cli` |
| lookml-parser(npm) | `overlays/lookml-parser/` | npm `lookml-parser` |
| look-at-me-sideways | `overlays/look-at-me-sideways/`(`rev` 固定) | GitHub `looker-open-source/look-at-me-sideways` |
| code-review(Emacs) | `home/hnakano/emacs/overrides/code-review/`(`rev` 固定) | GitHub `doomelpa/code-review` |
| claude-code-ide(Emacs) | `home/hnakano/emacs/overrides/claude-code-ide/`(`rev` 固定) | GitHub `manzaltu/claude-code-ide.el` |

### 注意

- npm パッケージ(dataform-cli, lookml-parser)の更新は `package.json` / `package-lock.json` の更新と `npmDepsHash` の再計算が必要。パッケージマネージャーの実行はグローバルルールで禁止されているため、実行前にユーザーへ確認すること。
- `fetchFromGitHub` の `rev` 固定のものは、`hash` の更新も必要。
