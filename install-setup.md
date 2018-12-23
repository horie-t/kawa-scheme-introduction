## Kawaのインストール・セットアップ

Ubuntu 16.04 LTSでのインストール・セットアップについて説明します。

Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。それだけではあんまりなので、その他の設定等を記載します。

### Java Development Kit(JDK)のインストール

Kawaの最新バージョンは、OracleのJDK 8を前提としているので、OracleのJDKをインストールします。

1. PPA(Personal Package Archive)からインストールします。
```bash
$ sudo add-apt-repository ppa:webupd8team/java
$ sudo apt-get update
$ sudo apt-get install oracle-java8-installer
```

### Kawaのインストール

Kawaのインストールには、バイナリのディストリビューション(jarファイル)をダンロードする方法と、ソースコードをダウンロードする方法があります。

jarファイルの扱いに慣れておいた方が色々な環境で扱う時に都合がいいので、ここではjarファイルをダウンロードして使う方法について説明します。

1. KawaのFTPサイトから、 [最新(2.1)のjarファイル](http://ftp.gnu.org/pub/gnu/kawa/kawa-2.1.jar) をダウンロードします。
2. ダウンロードしたJarファイルを、適切なディレクトリに保存します。(/usr/local/libあたりがいいでしょう)
3. ダウンロードしたjarファイルは、バージョンが付いているので、バージョンのないリンクを作成しておきます。  
```bash
$ cd /usr/local/lib
$ sudo ln -s kawa-2.1.jar kawa.jar
```
4. CLASSPATHを設定します。(Bashを使用している場合は、~/.bashrcファイルに以下を追加します)  
```bash
CLASSPATH=$CLASSPATH:/usr/local/lib/kawa.jar
export CLASSPATH
```

以上でインストールは完了です。

以下のコマンドを実行しすれば、Kawaを実行できます。
```bash
$ java kawa.repl
```

終了するには、「(exit)」と入力します。
```bash
$ java kawa.repl
#|kawa:1|# (exit)
$
```
-wのオプションを指定すると新規のWindowが作成されてそこでKawaが実行されるのですが、エラーになるようです
```bash
$ java kawa.repl -w
```

環境変数にCLASSPATHを指定していない場合は以下のようにして実行します。
```bash
$java -cp /usr/local/lib/kawa-2.1.jar kawa.repl
```

### Emacsのインストール

SchemeやLispの開発は大抵の場合、Emacsを使用するので、Emacsをインストールします。
```bash
$ sudo apt-get install emacs24
```

emacsの設定ファイル(~/.emacs.d/init.el)に以下を追加します。
```lisp
;;;; For Kawa
(setq scheme-program-name "java -cp /usr/local/bin/kawa.jar kawa.repl --full-tailcalls --warn-undefined-variable=no --warn-invoke-unknown-method=no --no-inline --output-format readable-scheme -s")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)
```

### Emacs上でのKawaの実行

Emacs を起動して、`M-x run-scheme` と入力すれば、EmacsからKawaを起動できます。
