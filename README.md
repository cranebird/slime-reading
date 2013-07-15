# コンポーネント

FIXME
![components diagram slime](comp-slime.png)
![components diagram swank](comp-swank.png)

# Emacs 側

## SLIME 実行時の process およびバッファ

- SLIME connection  (`slime-connection` 関数 の返り値)
- SLIME process (`slime-process` 関数の返り値
- `*slime-events*` バッファ

Emacs のスクラッチバッファで確認できる。

    (slime-connection)
    => #<process SLIME Lisp>
    (slime-process)
    => #<process inferior-lisp>  ;; local で swank を実行中の場合
    (slime-process)
    => nil ;; 既に起動済みの swank に接続した場合

### SLIME connection

実体はネットワーク接続。Elisp のプロセス関連関数 `process-contact` で詳細情報が得られる。

    ;; local
    (pp (process-contact (slime-connection) t))
    =>
    (:name "SLIME Lisp" :buffer #<buffer  *cl-connection*>
    :host "127.0.0.1" :service 49178 :nowait nil
    :remote [127 0 0 1 49178] :local  [127 0 0 1 49179]
    :filter slime-net-filter :sentinel slime-net-sentinel)

    ;; remote
    ;; xxxx は ssh tunnering に使用しているポート
    (:name "SLIME Lisp" :buffer #<buffer  *cl-connection*>
    :host "127.0.0.1" :service xxxx :nowait nil
    :remote [127 0 0 1 xxxx] :local [127 0 0 1 49188]
    :filter slime-net-filter :sentinel slime-net-sentinel)

#### buffer \*cl-connection\*

SLIME connection に紐づいたバッファ。バッファ名は先頭にスペースあり。

    (process-buffer (slime-connection))
    => #<buffer  *cl-connection*>

#### slime-net-filter

process filter.
メッセージを処理し、 event dispatcher に渡す。

- `*cl-connection*` バッファに受けとったメッセージを出力する。
- メッセージを全て受けとった場合、メッセージを read する。
- event 

#### process sentinel

`slime-net-sentinel`.
"Lisp connection closed unexpectedly: %s " をメッセージに出力し、後始末をする。

#### connection-local 変数

connection 毎に異なる値を持つ connection-local な変数が使われる。実体は、suffix が ":connlocal" のバッファローカル変数。slime-def-connection-var マクロで定義される。

    (pp (loop for (name . value) in
       (buffer-local-variables (get-buffer " *cl-connection*"))
      if (string-match ".*:connlocal" (symbol-name name))
      collect (cons name value)))
    =>

- slime-connection-number
- slime-lisp-features
- slime-lisp-modules
- slime-pid
- slime-lisp-implementation-type
- slime-lisp-implementation-version
- slime-lisp-implementation-name
- slime-lisp-implementation-program
- slime-connection-name
- slime-inferior-process
- slime-communication-style
- slime-machine-instance
- slime-connection-coding-systems

- slime-rex-continuations
- slime-continuation-counter
- slime-channels
- slime-channels-counter

### SLIME process



### \*slime-events\* バッファ

変数 `slime-log-events` が `t` の場合にこのバッファにイベントがログとして出力される。
ただし、イベントは pretty print され、全てが出力されない場合がある。

## slime の重要な関数、マクロ (elisp)

### slime-send 関数

slime-net-send

### slime-net-send 関数

`slime-prin1-to-string` 関数で header と payload を作成する。

### slime-net-read 関数

パケットを read し、 S-式を返す。その後、読んだ部分をバッファから削除する。

### slime-dispatch-event 関数

(:emacs-rex form package thread continuation) のイベントを受けとると、continuation の代わりに incf した slime-continuation-counter を slime-send する。 (:emacs-rex form package thread id)
そのうえで、 id と continuation の組を connection-local variable として保存する。

### slime-rex マクロ

## RPC protocol

### パケット

16進数6桁のS-式の長さ部分とS-式本体からなる。`slime-net-send` 関数が生成する。

    ;; 例
    00004c(:emacs-rex (swank:listener-eval \"9\n\")
    \"COMMON-LISP-USER\" :repl-thread 120)\n

## イベント

先頭がキーワードであるリスト。キーワード名が ":emacs-" で始まるイベントは、Emacs 側で生成されたもの。

## 例: C-c C-m 押下時のシーケンス図

![seq](seq-C-c-C-m.png)

# Swank 側

## パッケージ

- \:swank
- \:swank-io-package
- \:swank-match
- \:swank-rpc
- \:swank-backend

## connection

変数 `*emacs-connection*` が Emacs 側との接続を管理する。multithread 環境の場合、実体は swank.lisp で定義される構造体 `multithreaded-connection`。

    SWANK> (multithreaded-connection-p *emacs-connection*)
    T
    SWANK> (mconn.socket-io *emacs-connection*)
    #<SB-SYS:FD-STREAM for "socket 127.0.0.1:62279, peer: 127.0.0.1:62280" {100472C203}>

## Threads

変数 `*thread-list*` で管理される。

     SWANK> (list-threads )
     ((:ID :NAME :STATUS)
     (4 "repl-thread" "Running")
     (5 "auto-flush-thread" "Running")
     (6 "swank-indentation-cache-thread" "Running")
     (7 "reader-thread" "Running")
     (8 "control-thread" "Running")
     (9 "Swank xxxx" "Running")
     (10 "Swank Sentinel" "Running")
     (11 "main thread" "Running"))


- repl-thread

- auto-flush-thread

- swank-indentation-cache-thread

- reader-thread

- control-thread

send-to-emacs での送信先。

- Swank port-number

- Swank Sentinel

- main thread


## インターフェース

swank サーバのインターフェースはマクロ `definterface` で定義される。定義されたインターフェースはパラメータ `swank-backend::*interfaces-functions*` で管理される。インターフェースは `defimplementation` で実装する。全てのインターフェースが実装される必要はなく、未実装のインターフェースは、パラメータ `*unimplemented-interfaces*` で管理され、起動時に警告される(`warn-unimplemented-interfaces` 関数)。

### 全インターフェース

|Category | interface|
|---------|----------|
|UTF8 | STRING-TO-UTF8 UTF8-TO-STRING|
|Codepoint length | CODEPOINT-LENGTH|
|TCP server | CREATE-SOCKET LOCAL-PORT CLOSE-SOCKET ACCEPT-CONNECTION ADD-SIGIO-HANDLER REMOVE-SIGIO-HANDLERS ADD-FD-HANDLER REMOVE-FD-HANDLERS PREFERRED-COMMUNICATION-STYLE SET-STREAM-TIMEOUT EMACS-CONNECTED|
|Unix signals | GETPID INSTALL-SIGINT-HANDLER CALL-WITH-USER-BREAK-HANDLER QUIT-LISP LISP-IMPLEMENTATION-TYPE-NAME LISP-IMPLEMENTATION-PROGRAM SOCKET-FD MAKE-FD-STREAM DUP EXEC-IMAGE COMMAND-LINE-ARGS|
|pathnames | FILENAME-TO-PATHNAME PATHNAME-TO-FILENAME DEFAULT-DIRECTORY SET-DEFAULT-DIRECTORY CALL-WITH-SYNTAX-HOOKS DEFAULT-READTABLE-ALIST|
|Compilation | CALL-WITH-COMPILATION-HOOKS SWANK-COMPILE-STRING SWANK-COMPILE-FILE FIND-EXTERNAL-FORMAT GUESS-EXTERNAL-FORMAT|
|Streams | MAKE-OUTPUT-STREAM MAKE-INPUT-STREAM|
|Documentation | ARGLIST TYPE-SPECIFIER-P FUNCTION-NAME VALID-FUNCTION-NAME-P MACROEXPAND-ALL COMPILER-MACROEXPAND-1 COMPILER-MACROEXPAND FORMAT-STRING-EXPAND DESCRIBE-SYMBOL-FOR-EMACS DESCRIBE-DEFINITION|
|Debugging | INSTALL-DEBUGGER-GLOBALLY CALL-WITH-DEBUGGING-ENVIRONMENT CALL-WITH-DEBUGGER-HOOK COMPUTE-BACKTRACE PRINT-FRAME FRAME-RESTARTABLE-P FRAME-SOURCE-LOCATION FRAME-CATCH-TAGS FRAME-LOCALS FRAME-VAR-VALUE DISASSEMBLE-FRAME EVAL-IN-FRAME FRAME-PACKAGE FRAME-CALL RETURN-FROM-FRAME RESTART-FRAME FORMAT-SLDB-CONDITION CONDITION-EXTRAS GDB-INITIAL-COMMANDS ACTIVATE-STEPPING SLDB-BREAK-ON-RETURN SLDB-BREAK-AT-START SLDB-STEPPER-CONDITION-P SLDB-STEP-INTO SLDB-STEP-NEXT SLDB-STEP-OUT|
|Definition finding | FIND-DEFINITIONS FIND-SOURCE-LOCATION BUFFER-FIRST-CHANGE |
|XREF | WHO-CALLS CALLS-WHO WHO-REFERENCES WHO-BINDS WHO-SETS WHO-MACROEXPANDS WHO-SPECIALIZES LIST-CALLERS LIST-CALLEES|
|Profiling | PROFILE PROFILED-FUNCTIONS UNPROFILE UNPROFILE-ALL PROFILE-REPORT PROFILE-RESET PROFILE-PACKAGE|
|Trace | TOGGLE-TRACE|
|Inspector | EVAL-CONTEXT DESCRIBE-PRIMITIVE-TYPE|
|Multithreading | INITIALIZE-MULTIPROCESSING SPAWN THREAD-ID FIND-THREAD THREAD-NAME THREAD-STATUS THREAD-ATTRIBUTES CURRENT-THREAD ALL-THREADS THREAD-ALIVE-P INTERRUPT-THREAD KILL-THREAD SEND RECEIVE RECEIVE-IF REGISTER-THREAD FIND-REGISTERED SET-DEFAULT-INITIAL-BINDING WAIT-FOR-INPUT|
|Locks | MAKE-LOCK CALL-WITH-LOCK-HELD|
|Weak datastructures | MAKE-WEAK-KEY-HASH-TABLE MAKE-WEAK-VALUE-HASH-TABLE HASH-TABLE-WEAKNESS|
|Character names | CHARACTER-COMPLETION-SET SAVE-IMAGE BACKGROUND-SAVE-IMAGE|

## swank の重要な関数、マクロ、構造体 (Common Lisp)

### connection 構造体

Emacs と Lisp のネットワーク接続を表現する。


### defslimefun マクロ

Emacs が RPC で呼び出せる関数を定義する。

### add-hook マクロ、 run-hook 関数

Emacs の add-hook, run-hook 相当。

### destructure-case マクロ

パターンマッチ。

### decode-message 関数、encode-message 関数

### read-message 関数、read-form 関数、read-packet 関数、parse-header 関数


# SLIME の起動

# SWANK サーバの起動 

![sequence diagram slime](seq-swank-boot.png)

- swank-loader.lisp を `load` する。
- `swank-loader:init` 関数に必要なパラメータを渡す。
- `swank:create-server` 関数を実行する。
-- `swank:setup-server` 関数を実行する。
--- `init-log-output` 関数を実行する。
--- `sb-bsd-sockets:inet-socket` 関数を実行し、ソケット生成する。
`definterface` で定義される `preferred-communication-style` によって、デフォルトの `*communication-style*` を決定する。シンボル `:sb-thread` が `*features*` 変数内にあれば、 `:spawn` となる。
--- `announce-fn` を funcall する。

# ./contrib/swank-media


