# SLIME Code Reading

## SLIME 実行時の process およびバッファ

* SLIME connection  (slime-connection)の返り値
* SLIME process (slime-process) の返り値

    (slime-connection)
    => #<process SLIME Lisp>
    (slime-process)
    => #<process inferior-lisp>  ;; local で swank を実行中の場合
    (slime-process)
    => nil ;; 既に起動済みの swank に接続した場合

### SLIME connection

実体はネットワーク接続である。Elisp のプロセス関連関数 #'process-contact で詳細情報を得ることができる。

    (pp (process-contact (slime-connection) t))
    =>
    (:name "SLIME Lisp" :buffer #<buffer  *cl-connection*>
    :host "127.0.0.1" :service 49178 :nowait nil
    :remote [127 0 0 1 49178]
    :local  [127 0 0 1 49179]
    :filter slime-net-filter :sentinel slime-net-sentinel)
    
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


