## Databaseとの接続

Schemeで本格的にコードを書きはじめて、ふと気付く事に、リレーショナルDBMSに接続するのはどうすればいいんだ？ という事があります。

使っているScheme処理系が、使いたいDBMSへの接続をサポートしていればいいのですが(例えば、Gaucheは、[いくつかのDBのパッケージ](http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3APackages#H-3ysn8b34y)がありますが)、サポートしていない場合は接続ドライバを書くことから始めなければなりません。

これは、データベースへの接続だけの問題ではなく、他に使いたいライブラリやWebサービスがあった場合にも、まず最初にライブラリやサービス接続用のコードを書かなければならないという事を意味します。

Kawaを使用する場合は、大抵、Javaのライブラリが存在するので、実行時のクラス・パスにjarを設定して、Javaのクラスを呼び出すだけです。
### SQLiteのDBへの接続

Javaでは、リレーショナル・データベースへの接続のAPIはJDBCで規定されていて、実用的なDBMSは、JDBCのドライバが提供されています。

[SQLiteのJDBCドライバ](https://github.com/xerial/sqlite-jdbc)がありますので、それをダウンロードします。そして、起動時に、以下のようにKawaのjarファイルと同様にクラスパスに指定します。

```bash
$ java -cp /usr/local/lib/kawa-2.1.jar:/usr/local/lib/sqlite-jdbc-3.8.11.2.jar kawa.repl
```
起動したら、以下のようにDBを作成して、アクセスできます。

```scheme
#|kawa:1|# (define dbCon (java.sql.DriverManager:getConnection "jdbc:sqlite:/var/tmp/sample.db"))
#|kawa:2|# (define statement (*:createStatement dbCon))
#|kawa:3|# (*:executeUpdate statement "CREATE TABLE person (id INTEGER, name TEXT)")
#|kawa:4|# (*:executeUpdate statement "INSERT INTO person VALUES(1, 'John Do')")
1
#|kawa:5|# (*:executeUpdate statement "INSERT INTO person VALUES(2, 'Maria Smith')")
1
#|kawa:6|# (define result-set (*:executeQuery statement "SELECT * FROM person"))
#|kawa:7|# (let loop ((row-exist (*:next result-set))
                      (person-list '()))
             (cond ((not row-exist)
                    (reverse person-list))
                   (else
                    (let ((person (list (*:getInt result-set "id") (*:getString result-set "name"))))
                      (loop (*:next result-set) (cons person person-list))))))
((1 "John Do") (2 "Maria Smith"))
```

### Webサーバ(Servlet)からのデータベースへの接続
では、WebサーバからDBへ接続するServletを作ってみましょう。[Webサーバ(Java Servlet)として動かす](./servlet)と同様の手順で作成します。

まず、適当なディレクトリを作成し、KawaとSQLite JDBCのjarファイルをlibディレクトリにコピーします。

```bash
$ mkdir DBApp
$ cd DBApp
$ mkdir WEB-INF WEB-INF/lib WEB-INF/classes
$ cp /usr/local/lib/kawa-2.1.jar WEB-INF/lib
$ cp /usr/local/lib/sqlite-jdbc-3.8.11.2.jar WEB-INF/lib
```

WEB-INF/classesディレクトリに以下のようにHelloDB.scmを作成します。

```scheme
(require 'xml)

(java.lang.Class:forName "org.sqlite.JDBC")

;;;; Databaseへ接続して、人のリストを取得
(define dbCon (java.sql.DriverManager:getConnection "jdbc:sqlite:/var/tmp/sample.db"))
(define statement (*:createStatement dbCon))
(define result-set (*:executeQuery statement "SELECT * FROM person"))
(define person-list (let loop ((row-exist (*:next result-set))
			       (person-list '()))
		      (cond ((not row-exist)
			     (reverse person-list))
			    (else
			     (let ((person (list (*:getInt result-set "id") (*:getString result-set "name"))))
			       (loop (*:next result-set) (cons person person-list)))))))

;;;; LispのリストをHTMLのULに変換
(define (person-list-&gt;html-ul ls)
  ;; mapの結果はリストで返ってくるので、applyを使っている事に注意。
  (apply html:ul
	 (map (lambda (person)
		(html:li (cadr person)))
	      ls)))

;;;; Webページを生成
(define (create-html person-list)
  (as-xml
   (values
    (unescaped-data "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?&gt;")
    (unescaped-data "&lt;!DOCTYPE html&gt;")
    (html:html 
     (html:head
      (html:title "Members"))
     (html:body
      (html:h1 "Menbers")
      (html:p "Current member is below.")
      (person-list-&gt;html-ul person-list))))))

(define response (get-response))
(response:setContentType "text/html")

(define oport (gnu.kawa.io.OutPort (response:getWriter)))
(display (create-html person-list) oport)
(flush-output-port oport)
```

そして、以下のように、コンパイルします。jarファイルのパスは、インストール環境に合わせます。

```bash
$ cd WEB-INF/classes/
$ java -cp /usr/local/lib/kawa-2.1.jar:/opt/tomcat/lib/servlet-api.jar kawa.repl --servlet -C HelloDB.scm
```

WEB-INFフォルダに以下のようなweb.xmlファイルを作成します。
```xml
<web-app>
  <display-name>DB Application</display-name>

  <servlet>
    <servlet-name>DBApp</servlet-name>
    <servlet-class>HelloDB</servlet-class>
  </servlet>

  <servlet-mapping>
    <servlet-name>DBApp</servlet-name>
    <url-pattern>/member</url-pattern>
  </servlet-mapping>
</web-app>
```

DBAppディレクトリに移動して、warファイルを作成し、Tomcatのwebappsディレクトリにコピーします。

```bash
$ cd DBApp
$ jar -cvf ../DBApp.war .
$ cp ../DBApp.war /opt/tomcat/webapps/
```

ローカルのマシンでTomcatを動かしている場合、Webブラウザで、 http://localhost:8080/DBApp/member にアクセスするとWebページが表示されます。
