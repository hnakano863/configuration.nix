---
name: update-configuration
description: configuration.nix リポジトリの更新(flake.lock の更新と、flake input 外の手動管理パッケージの更新確認・反映)を行う。「configurationを更新」「flakeを更新」などの依頼で使う。nixos-rebuild は実行しない。
---

# configuration の更新

`nix flake update` だけでは更新されないパッケージがあるため、以下を順に実施する。
`nixos-rebuild` はユーザーが行うので、実行しない。コミットはユーザーが依頼したときのみ行う。

## 1. flake.lock の更新

```bash
nix flake update
```

更新された input を一覧でユーザーに報告する。

## 2. 手動管理パッケージの最新版確認

| 対象 | 定義場所 | 最新版の確認方法 |
|---|---|---|
| arize-phoenix(Dockerイメージタグ) | `configuration/wsl2.nix` の `version` | `curl -s https://pypi.org/pypi/arize-phoenix/json` の `info.version` |
| dataform-cli(npm) | `overlays/dataform-cli/` | `curl -s https://registry.npmjs.org/@dataform/cli/latest` の `version` |
| lookml-parser(npm) | `overlays/lookml-parser/` | `curl -s https://registry.npmjs.org/lookml-parser/latest` の `version` |
| look-at-me-sideways | `overlays/look-at-me-sideways/`(`rev` 固定) | `curl -s https://api.github.com/repos/looker-open-source/look-at-me-sideways/commits?per_page=1` の `sha` |
| code-review(Emacs) | `home/hnakano/emacs/overrides/code-review/`(`rev` 固定) | `curl -s https://api.github.com/repos/doomelpa/code-review/commits?per_page=1` の `sha` |
| claude-code-ide(Emacs) | `home/hnakano/emacs/overrides/claude-code-ide/`(`rev` 固定) | `curl -s https://api.github.com/repos/manzaltu/claude-code-ide.el/commits?per_page=1` の `sha` |

現在の値と最新版を比較し、結果を表でユーザーに報告する。
arize-phoenix は PyPI のバージョンを目安にする(Docker Hub にそのタグがあるかは別途確認が必要な場合がある)。

## 3. 古いものの更新

更新が必要なものは、ユーザーに提示したうえで以下のとおり更新する。

### arize-phoenix

`configuration/wsl2.nix` の `version` を書き換える。

### npm パッケージ(dataform-cli, lookml-parser)

`<pkg>` は `dataform-cli` または `lookml-parser`。

1. `overlays/<pkg>/package.json` の `version` と `dependencies` の値、`overlays/<pkg>/default.nix` の `version` を新バージョンに書き換える(`sed -i` で可)。
2. `overlays/<pkg>/` に移動し、`npm install --package-lock-only --ignore-scripts` で `package-lock.json` を更新する。
   - このコマンドだけは、`configuration.nix/overlays` 以下に限りフックで許可されている。他の npm / pip 操作は禁止なので、実行前にユーザーへ確認すること。
   - コマンドは完全一致が必要なので、フラグの追加や `&&` での連結はしない。作業ディレクトリは事前に別の呼び出しで移動する。
3. `nix run nixpkgs#prefetch-npm-deps -- package-lock.json` を `overlays/<pkg>/` で実行し、出力されたハッシュで `default.nix` の `npmDepsHash` を書き換える。

### `rev` 固定のもの(look-at-me-sideways, code-review, claude-code-ide)

1. `default.nix` の `rev` を最新コミットの sha に書き換える。`version` に日付を含むものは日付も更新する。
2. `hash` を再計算する。`nix-prefetch-github` 等が無ければ、`hash` を `lib.fakeHash` にして `nix build` し、エラー出力の正しいハッシュを貼り直す。
3. look-at-me-sideways は `npmDepsHash` も再計算が必要な場合がある。

## 4. 報告

- 更新したもの、更新不要だったもの、ユーザー判断待ちのものを分けて報告する。
- ビルド確認(`nix build` / `nixos-rebuild`)はしていない場合、その旨を明記する。
