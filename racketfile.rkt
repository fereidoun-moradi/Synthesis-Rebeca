#lang rosette
(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/destruct   ; provides `destruct`
         rosette/lib/match
         rosette/lib/profile
         rosette/lib/synthax
         rosette/lib/value-browser)  
; Tell Rosette we really do want to use integers.
(current-bitwidth #f)



(define (string-index hay needle)
  (define n (string-length needle))
  (define h (string-length hay))
  (and (<= n h) ; if the needle is longer than hay, then the needle can not be found
       (for/or ([i (- h n -1)]
                #:when (string=? (substring hay i (+ i n)) needle))
         i)))

; Syntax for a DSL is defined on Rebeca language
(struct actor (name) ;Reactiveclass
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "reactiveclass " other
                        "(3) { " (actor-name self))))
(define reactiveclass (actor "}"))

(struct rebec (name) ;Known rebec
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "knownrebec " other
                        " { " (rebec-name self))))
(define knownrebec (rebec "}"))

(struct var (name) ;State varriable
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "statevars " other
                        " { " (var-name self))))
(define statevars (var "}"))

(struct msg (name) ;Message servers
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "msgsrv " other
                        "() { " (msg-name self))))
(define msgsrv (msg "}"))

(struct instances (name) ;Main
          #:property prop:procedure
                     (lambda (self other)
                       (string-append
                        "main " other
                        " { " (instances-name self))))
(define main (instances "}"))
;Structure of operations 
(struct cons (a) #:transparent)   ;construct reactiveclass a
(struct conc (a b) #:transparent) ;concatenation a and b
(struct add (a b) #:transparent)  ;place b inside a 
(struct len (a) #:transparent)    ;length string a
(struct conv (a) #:transparent)   ;convert a to string


; (Semantic), interpreter for our DSL. 
(define (interpreter p)
  (destruct p
    [(cons a) (reactiveclass (interpreter a))]
    [(conc a b) (string-append (interpreter a) (interpreter b))]
    [(add a b) (string-append (substring (interpreter a) 0 (string-index (interpreter a) "}"))
                              (interpreter b)
                              (substring (interpreter a) (string-index (interpreter a) "}")))]
    [(len a) (string-length (interpreter a))]
    [(conv a) (~r (interpreter a))]
    [_ p]))


; Create an unknown expression
(define (??exp)
  (define left (choose* (string-append (string-append (knownrebec "")
                                                      (statevars "")) (msgsrv "msg1"))
                        (string-append (string-append (knownrebec "") (statevars ""))
                                       (string-append (msgsrv "msg1") (msgsrv "msg2")))))
  (define right (choose* (string-append (string-append (knownrebec "") (statevars ""))
                                        (msgsrv "msg3"))))
  (choose*  left  right)) 

; Create a sketch representing all desirable Rebeca codes
; where the ??s are unknown expressions created by ??exp.
(define sketch_rebeca
    (conv (len (conc (add (cons "actor1") (??exp)) (add (cons "actor2") (??exp))))))

; Solve the sketch to find a program equivalent to lenght 203,
(define-symbolic k integer?)
(define MM
  (synthesize
   #:forall (list k)
   #:guarantee (assert (equal? (interpreter sketch_rebeca) (interpreter (conv 203))))))

; Substitute the bindings in MM into the sketch to get back the
; synthesized Rebeca code.
;(evaluate sketch_rebeca MM)
