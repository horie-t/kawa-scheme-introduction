## Javaコードとの連携

KawaはJavaの仮想マシン上で動いているので、簡単にJavaのオブジェクトを生成したりアクセスしたりできます。

### Javaのクラスとインスタンス
Javaのクラスのインスタンスを生成するには、クラス名で呼び出します。

```scheme
#|kawa:3|# (define arrayList (java.util.ArrayList))
```

newする時に引数を指定したい場合は、引数を列挙します。

```scheme
;; 初期容量20でnew
#|kawa:15|# (define capArrayList (java.util.ArrayList 20))
```

メンバーメソッドを呼び出すときは、(インスタンス変数:メソッド名 メソッドの引数)を評価します。

```scheme
#|kawa:5|# (arrayList:add 3)
#t
#|kawa:6|# (arrayList:add 1)
#t
#|kawa:7|# (arrayList:add 7)
#t
#|kawa:8|# (arrayList:add 9)
#t
#|kawa:9|# (arrayList:get 0)
3
```

(*:メソッド名 インスタンス変数 引数)でも呼び出せます。

```scheme
#|kawa:16|# (*:get arrayList 2)
7
```

クラスメソッドを呼び出すときは、(クラス名:メソッド名 引数)を評価します。

```scheme
#|kawa:10|# (java.util.Collections:sort arrayList)
#|kawa:11|# (*:get arrayList 0)
1
```

クラスのフィールドにアクセスするには 、クラス名:フィールド名でアクセスします。 (インスタンス・フィールドにアクセスするには、インスタンス変数:フィールド名で)
```scheme
#|kawa:18|# java.lang.Integer:MAX_VALUE
2147483647
```

クラス名のカノニカル名を入力するのが面倒な時は、importします。

```scheme
#|kawa:10|# (import (class java.util ArrayList))
#|kawa:11|# (define ls (ArrayList))
```

ネストしたクラスを使用するには、「:」または、「$」を使います。

```scheme
#|kawa:17|# (define locBuilder (java.util.Locale:Builder))
#|kawa:18|# (define locBuilder2 (java.util.Locale$Builder))
```

### Javaの配列

Javaの配列を生成するには以下のようにします。

```scheme
#|kawa:5|# (define primes (integer[] 2 3 5 7 11 13))
```

配列にアクセスするには、以下のようにします。

```scheme
#|kawa:6|# (primes 5)
13
```

配列の値を変更するには以下のようにします。

```scheme
#|kawa:7|# (set! (primes 0) -2)
#|kawa:8|# primes
[-2 3 5 7 11 13]
```

配列の領域だけ確保するには以下のようにします。

```scheme
#|kawa:9|# (int[] length: 10)
[0 0 0 0 0 0 0 0 0 0]
```

### Javaのクラスの定義・継承

Javaのオブジェクトを生成するだけでなく、継承したクラスを作成して、利用したいこともあります。

クラスを定義する簡単な方法は、define-simple-classを使う方法で、以下の書式でクラスを定義します。
```scheme
(define-simple-class クラス名 (superクラス名 インタフェース名...)
 (フィールド名 型宣言)
 ((メソッド名 (引数 型宣言)...)
  メソッドの実装)))
```

コンストラクタは*init*という特別な名前で定義します 。また、thisの参照は(this)マクロを使用します。

