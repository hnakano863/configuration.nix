# agenix の受信者ルール定義。`agenix -e <name>.age` はこのファイルを読み、
# 対応する publicKeys で暗号化する。ここには「公開鍵」だけを書く (秘密情報ではない)。
let
  hnakano = "age1l03sntvutyj8r3jpsc7cwrvtjxpyq9k4vhjy576sk8l4tmqkacss7t5nvx";
in
{
  "esa-env.age".publicKeys = [ hnakano ];
}
