## XML/HTMLの取り扱い

XMLをプログラムのコードで出力しようとすると、大抵は面倒な事が多いのですが、Kawaでは非常に簡単にできます。

XMLは、本質的にタグで囲われたデータのリストです。リストの要素のデータは、タグデータのリストにして、入れ子にする事ができます。Lispのプログラムも、S式と呼ばれる、カッコで囲われたデータのリストになっています。やっぱり、要素のデータは､入れ子にできます。

なので、XMLのデータはS式で表現する事ができます。

```html
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Kawa Scheme入門</title>
  </head>
  <body>
    <h1>Kawaとは</h1>
    <p><a href="http://www.gnu.org/software/kawa/">Kawa</a>は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。</p>
    <h1>Kawaのインストール</h1>
    <p>Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。</p>
  </body>
</html>
```

例えば、上記のXML(XHTMLですが)は、Lispプログラマから見ると、まず、閉じタグに要素名がいちいち出てくるのが邪魔に見えます。なので、閉じタグは全部&lt;/&gt;と表現してもいいように思えてきます。(どうせ、開始タグと対応付けないと文法的に不正ですから、本来なくても構わない)

```html
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Kawa Scheme入門</>
  </>
  <body>
    <h1>Kawaとは</>
    <p><a href="http://www.gnu.org/software/kawa/">Kawa</>は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。</>
    <h1>Kawaのインストール</>
    <p>Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。</>
  </>
</>
```


そして、タグの<>の記号は、尖った感じで目に優しくない(Lisperは丸括弧が好き)ので、開始タグは<html>は「(html 」としてしまって、「</>」となっている部分は単に「)」としてしまいます。(文章の部分がわかりにくくなるので""でくくってしまいます)

```scheme
(!DOCTYPE html)
(html xmlns="http://www.w3.org/1999/xhtml"
  (head
    (title "Kawa Scheme入門")
  )
  (body
    (h1 "Kawaとは")
    (p (a href="http://www.gnu.org/software/kawa/" "Kawa") "は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。")
    (h1 "Kawaのインストール")
    (p "Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。")
  )
)
```
XMLの属性も、属性名=値のリスト(連想リストでも表現できるもの)なので、(@ を最初につけて、連想リストっぽく表現できる)
```scheme
(!DOCTYPE html)
(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
  (head
    (title "Kawa Scheme入門")
  )
  (body
    (h1 "Kawaとは")
    (p (a (@ (href "http://www.gnu.org/software/kawa/")) "Kawa") "は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。")
    (h1 "Kawaのインストール")
    (p "Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。")
  )
)
```
こうなってくると、閉じカッコのためだけに一行つかうのは無駄ですね。

```scheme
(!DOCTYPE html)
(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
  (head
    (title "Kawa Scheme入門"))
  (body
    (h1 "Kawaとは")
    (p (a (@ (href "http://www.gnu.org/software/kawa/")) "Kawa") "は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。")
    (h1 "Kawaのインストール")
    (p "Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。")))</pre>
```
気がついたらS式になっていました。この表記法はXMLのS式表現の[SXML](https://ja.wikipedia.org/wiki/SXML)と呼ばれる表記法です。

このように元のXMLの文書構造と同じようにプログラムを書ければ、保守しやすいコードになるわけです。Kawaの場合、make-element、make-attribute関数を呼び出して、XMLデータを構築します。
```scheme
(as-xml
 (values
  (unescaped-data "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;")
  (unescaped-data "&lt;!DOCTYPE html&gt;")
  (make-element (symbol 'html "http://www.w3.org/1999/xhtml")
		(make-element 'head
			      (make-element
			       'title "Kawa Scheme入門"))
		(make-element 'body
			      (make-element 'h1 "Kawaとは")
			      (make-element 'p
					    (make-element 'a
							  (make-attribute 'href "http://www.gnu.org/software/kawa/")
							  "Kawa")
					    "は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。")
			      (make-element 'h1 "Kawaのインストール")
			      (make-element 'p "Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。")))))
```			      

なお、HTMLの場合は、 以下のように html:タグ名 の書式で短く書くことができます。

```scheme
(as-xml
 (values
  (unescaped-data "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;")
  (unescaped-data "&lt;!DOCTYPE html&gt;")
  (html:html 
   (html:head
    (html:title "Kawa Scheme入門"))
   (html:body
    (html:h1 "Kawaとは")
    (html:p (html:a href: "http://www.gnu.org/software/kawa/" "Kawa")
	    "は、Java上で動作するScheme(Lispの方言)を拡張したプログラミング言語です。")
    (html:h1 "Kawaのインストール")
    (html:p "Kawaを動かすだけなら、Kawaのjarファイルをダウンロードして実行するだけです。")))))
```
