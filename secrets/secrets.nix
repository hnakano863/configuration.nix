# agenix の受信者ルール定義。`agenix -e <name>.age` はこのファイルを読み、
# 対応する publicKeys で暗号化する。ここには「公開鍵」だけを書く (秘密情報ではない)。
let
  hnakano = "age1qaf8ghprnpz87z9j33myxjcvwcxy0qq8sdqmpj4x5y0x0ctngs3q2srdg5";
in
{
  "esa-env.age".publicKeys = [ hnakano ];
}
