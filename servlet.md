## Webサーバ(Java Servlet)として動かす

Kawaでは、簡単にHTMLをプログラムから組み立てる事ができました。このぐらい簡単にHTMLを出力できるのはPHPぐらいです。

PHPも、最近ではテンプレート・エンジンを使うのが一般的になってきて、PHPそのものではHTMLの要素を出力しなくなってきています。その代わりにPHPではない別のテンプレート用の言語を習得する必要が出てきてしまっています。

S式を使えば、HTMLのテンプレートも簡単に書けるので、Lisp処理系をWebサーバとして使えないだろうかと考えてしまいます。実際に[Lisp処理系を使ってWebサイトを構築した有名な例](https://practical-scheme.net/wiliki/wiliki.cgi?%E3%82%A6%E3%82%A7%E3%83%96%E3%83%99%E3%83%BC%E3%82%B9%E3%82%A2%E3%83%97%E3%83%AA%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%81%AE%E3%81%9F%E3%82%81%E3%81%AELisp)があります。Schemeでのオープンソースの実装には、[Kahua](https://github.com/kahua)があります。

Kawaの場合は、Java上で動作するので、JSchemeで書いたコードをJava Servletとして動かす事ができます。

まず、適当なディレクトリを作成します。

```bash
$ mkdir hello
$ cd hello
$ mkdir WEB-INF WEB-INF/lib WEB-INF/classes
```
WEB-INF/libディレクトリにkawaのjarファイルをコピーします。

```bash
$ cp /usr/local/lib/kawa-2.1.jar WEB-INF/lib
```

WEB-INF/classesディレクトリに以下のようにhelloservlet.scmを作成します。
```scheme
(require 'xml)

(define response-html
  (as-xml
   (values
    (unescaped-data "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;")
    (unescaped-data "&lt;!DOCTYPE html&gt;")  
    (html:html
     (html:head
      (html:title "Hello Servlet"))
     (html:body
      (html:h1 "Hello")
      (html:p (format "The time is &lt;~s&gt;." (java.util.Date))))))))

;; javax.servlet.http.HttpServletResponse が返ってくるので、使えるメソッドはjavadoc参照
(define response (get-response))
(response:setContentType "text/html")

(define oport (gnu.kawa.io.OutPort (response:getWriter)))
(display response-html oport)
(flush-output-port oport)
```

そして、以下のように、コンパイルします。jarファイルのパスは、インストール環境に合わせます。

```scheme
$ cd WEB-INF/classes/
$ java -cp /usr/local/lib/kawa-2.1.jar:/opt/tomcat/lib/servlet-api.jar kawa.repl --servlet -C helloservlet.scm 
```

WEB-INFフォルダに以下のようなweb.xmlファイルを作成します。

```xml
<web-app>
  <display-name>Hello Servlet Application</display-name>

  <servlet>
    <servlet-name>hello</servlet-name>
    <servlet-class>helloservlet</servlet-class>
  </servlet>

  <servlet-mapping>
    <servlet-name>hello</servlet-name>
    <url-pattern>/helloservlet</url-pattern>
  </servlet-mapping>
</web-app>
```

helloディレクトリに移動して、warファイルを作成し、Tomcatのwebappsディレクトリにコピーします。

```bash
$ cd hello
$ jar -cvf ../hello.war .
$ cp ../hello.war /opt/tomcat/webapps/
```

ローカルのマシンでTomcatを動かしている場合、Webブラウザで、 http://localhost:8080/hello/helloservlet にアクセスするとWebページが表示されます。
