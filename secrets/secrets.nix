# agenix の受信者ルール定義。`agenix -e <name>.age` はこのファイルを読み、
# 対応する publicKeys で暗号化する。ここには「公開鍵」だけを書く (秘密情報ではない)。
#
# 手順:
#   1. age 鍵を生成 (未生成の場合):
#        mkdir -p ~/.config/agenix
#        age-keygen -o ~/.config/agenix/identity.txt
#      → 出力される "Public key: age1..." を下の hnakano に貼り付ける。
#   2. このディレクトリで暗号化:
#        cd secrets && agenix -e esa-env.age
#      → エディタが開くので dotenv 形式で以下を記入して保存:
#          ESA_ACCESS_TOKEN=xxxxxxxx
#          ESA_TEAM_NAME=andpad-dev
let
  # ↓↓↓ 手順1で得た age 公開鍵 (age1... で始まる) に置き換えてください。
  hnakano = "age1qaf8ghprnpz87z9j33myxjcvwcxy0qq8sdqmpj4x5y0x0ctngs3q2srdg5";
in
{
  "esa-env.age".publicKeys = [ hnakano ];
}
