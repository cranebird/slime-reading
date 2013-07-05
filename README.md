# SLIME Code Reading

## SLIME 実行時の process およびバッファ

+ (slime-connection)の返り値 #<process SLIME Lisp>


## SLIME の重要な関数 (elisp)

### slime-send 関数

slime-net-send

### slime-net-send 関数

slime-prin1-to-string で header と payload を作成する。

### slime-dispatch-event 関数

(:emacs-rex form package thread continuation) のイベントを受けとると、continuation の代わりに incf した slime-continuation-counter を slime-send する。 (:emacs-rex form package thread id)
そのうえで、 id と continuation の組を connection-local variable として保存する。

### slime-rex マクロ

## イベント

先頭がキーワードであるリスト。キーワード名が ":emacs-" で始まるイベントは、Emacs から生成されたもの。


