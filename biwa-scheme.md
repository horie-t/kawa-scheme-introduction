## Webブラウザ側のScheme(BiwaScheme)との連携

サーバ・サイド側でSchemeを使えるようになると、クライアント・サイド(つまりブラウザ側)でもJavaScriptではなく、Schemeを使いたいと言う欲求が頭をもたげてきます。Node.jsは、クライアント・サイドで使っている言語をサーバ・サイドでも使いたいと言う欲求から開発されましたが、ちょうどその逆の発想です。

ブラウザ側で動作するScheme処理系には、[BiwaScheme](http://www.biwascheme.org/index.html)があります。BiwaとKawaとでよく似ていますが、Biwaの由来は作者が住んでいた近くの湖の琵琶湖なので、関係はないようです。

BiwaSchemeと連携したWebベージを作ってみます。

まず、適当なディレクトリを作成します。

```bash
$ mkdir HelloBIWA
$ cd HelloBIWA
$ mkdir WEB-INF WEB-INF/lib WEB-INF/classes
```

WEB-INF/libディレクトリにkawaのjarファイルをコピーします。

```bash
$ cp /usr/local/lib/kawa-2.1.jar WEB-INF/lib
```

BiwaSchemeのサイトから、BiwaSchemeのJavaScriptをダウンロードして、HelloBIWAディレクトリにコピーします。

```bash
$ ls HelloBIWA
WEB-INF  biwascheme.js
```

WEB-INF/classesディレクトリに以下のようにBiwaScheme.scmを作成します。

```scheme
(require 'xml)

(define response-html
  (as-xml
   (values
    (unescaped-data "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;")
    (unescaped-data "&lt;!DOCTYPE html&gt;")  
    (html:html
     (html:head
      (html:title "Hello BiwaScheme"))
     (html:body
      (html:div id: "bs-console")
      (apply html:script src: "biwascheme.js" 
	     (map unescaped-data 
		  '((print "\"Hello, world!\"")  ;; BiwaScheme側でも文字列として扱わせるのでエスケープが必要。
		    (print (current-date))
		    (console-log "\"ok.\"")))))))))

;; javax.servlet.http.HttpServletResponse が返ってくるので、使えるメソッドはjavadoc参照
(define response (get-response))
(response:setContentType "text/html")

(define oport (gnu.kawa.io.OutPort (response:getWriter)))
(display response-html oport)
(flush-output-port oport)
```

そして、以下のように、コンパイルします。jarファイルのパスは、インストール環境に合わせます。

```bash
$ java -cp /usr/local/lib/kawa-2.1.jar:/opt/tomcat/lib/servlet-api.jar kawa.repl --servlet -C BiwaScheme.scm
```

WEB-INFフォルダに以下のようなweb.xmlファイルを作成します。

```xml
<web-app>
  <display-name>Hello BiwaScheme</display-name>

  <servlet>
    <servlet-name>HelloBiwa</servlet-name>
    <servlet-class>BiwaScheme</servlet-class>
  </servlet>

  <servlet-mapping>
    <servlet-name>HelloBiwa</servlet-name>
    <url-pattern>/hello</url-pattern>
  </servlet-mapping>
</web-app>
```

HelloBIWAディレクトリに移動して、warファイルを作成し、Tomcatのwebappsディレクトリにコピーします。

```bash
$ cd HelloBIWA
$ jar -cvf ../HelloBIWA.war .
$ cp ../HelloBIWA.war /opt/tomcat/webapps/
```
ローカルのマシンでTomcatを動かしている場合、Webブラウザで、 http://localhost:8080/HelloBIWA/hello にアクセスするとWebページが表示されます。

BiwaSchemeの方でも、HTMLの要素を以下のように作成する事ができます。

```scheme
  (element-new '(div "foo"))        => <div>foo</div>
  (element-new '("div#main" "foo")) => <div id='main'>foo</div>
  (element-new '("div.red" "foo"))  => <div class='red'>foo</div>
  (element-new '(div align "right" "foo"))  => <div align='right'>foo</div>
  (element-new '(div (span "foo"))  => <div><span>foo</span></div>
```

こうなると、HTMLのタグをほとんど見ずにWebサービスを構築する事ができます。

また、BiwaSchemeの方でも、サーバとの通信ができるので、サーバとクライアントの間では、S式でデータだけをやり取りして、クライアント側で表示用のHTMLを組み立てるような事ができます。
