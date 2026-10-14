# CLAUDE.md

## 「configurationを更新」するとき

`update-configuration` スキル(`.claude/skills/update-configuration/SKILL.md`)の手順に従う。
`nix flake update` だけでは更新されないパッケージ(arize-phoenix、overlays 以下の npm パッケージ、`rev` 固定のもの)の確認も含む。
`nixos-rebuild` はユーザーが実施するので、こちらでは実行しない。

## パッケージマネージャーの例外

グローバルルールで禁止されているが、configurationの更新時に限り、`overlays/*/` で `npm install --package-lock-only --ignore-scripts` のみ実行してよい(ユーザー許可済み。`~/.claude/hooks/block-pkg-install.sh` で許可)。
それ以外のパッケージマネージャー操作は従来どおり禁止で、実行前にユーザーへ確認すること。
