## KawaのプロセスにREPLをつける

GUIのアプリケーションや、Webサーバとして動作させながら、REPLでインタラクティブにプログラムを更新できると、プログラムの更新の度に再起動しなくてすむので便利です。

```scheme
(define-alias Thread java.lang.Thread)
(define-alias ServerSocket java.net.ServerSocket)
(define-alias InPort gnu.kawa.io.InPort)
(define-alias OutPort gnu.kawa.io.OutPort)

(define (process-repl iport oport)
  (display "> " oport)
  (flush-output-port oport)
  (let ((exp (read iport)))
    (let ((val (eval exp (intGUIのアプリケーションや、Webサーバとして動作させながら、REPLでインタラクティブにプログラムを更新できると、プログラムの更新の度に再起動しなくてすむので便利です。eraction-environment))))
      (display val oport)
      (newline oport)
      (process-repl iport oport))))

(define (echo-filter val)
  (apply values val))

(define (process-echo iport oport)
  (let ((line (read-line iport)))
    (display (echo-filter line) oport)
    (newline oport)
    (flush-output-port oport)
    (process-echo iport oport)))
   

(define (run-server port proc)
  (let ((listener (ServerSocket)))
    (*:setReuseAddress listener #t)
    (*:bind listener (java.net.InetSocketAddress port))
    (*:start
     (Thread (runnable
	      (lambda ()
		(let loop ()
		  (let ((socket (*:accept listener)))
		    (*:start 
		     (Thread (runnable 
			      (lambda () 
				(try-catch
				 (let ((iport (InPort (*:getInputStream socket)))
				       (oport (OutPort (*:getOutputStream socket))))
				   (proc iport oport))
				 (ex java.lang.Throwable
				     ex))))))
		    (loop)))))))))

;;;;
(run-server 8080 process-echo)
(run-server 8081 process-repl)
(sleep (* 24 60 60))
```
