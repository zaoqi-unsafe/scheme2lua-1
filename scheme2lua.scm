;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; comma expression
(define (list->comma-exp l)
  (if (null? l)
      ""
      (let ((h (car l))
            (t (cdr l)))
        (if (null? t)
            h
            (string-append h ", " (list->comma-exp t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua basic operators and keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #
(define (lua# var)
  (string-append "(" "#" var ")"))

; +
(define (lua+ var1 var2)
  (string-append "(" var1 " + " var2 ")"))

; -
(define (lua- var1 var2)
  (string-append "(" var1 " - " var2 ")"))

; *
(define (lua* var1 var2)
  (string-append "(" var1 " * " var2 ")"))

; /
(define (lua/ var1 var2)
  (string-append "(" var1 " / " var2 ")"))

; %
(define (lua% var1 var2)
  (string-append "(" var1 " % " var2 ")"))

; ^
(define (lua^ var1 var2)
  (string-append "(" var1 " ^ " var2 ")"))

; ==
(define (lua== var1 var2)
  (string-append "(" var1 " == " var2 ")"))

; ~=
(define (lua~= var1 var2)
  (string-append "(" var1 " ~= " var2 ")"))

; <=
(define (lua<= var1 var2)
  (string-append "(" var1 " <= " var2 ")"))

; >=
(define (lua>= var1 var2)
  (string-append "(" var1 " >= " var2 ")"))

; <
(define (lua< var1 var2)
  (string-append "(" var1 " < " var2 ")"))

; >
(define (lua> var1 var2)
  (string-append "(" var1 " > " var2 ")"))

; =
(define (lua= var1 var2)
  (string-append var1 " = " var2))

; ()
(define (lua-parenthesis var)
  (string-append "(" var ")"))

; []
(define (lua-bracket var)
  (string-append "[" var "]"))

; {}
(define (lua-brace var)
  (string-append "{" var "}"))

; ;
(define (lua-semicolon)
  ";")

; :
(define (lua-colon var1 var2)
  (string-append var1 ":" var2))

; ::
(define (lua-label var)
  (string-append "::" var "::"))

; .
(define (lua-dot var1 var2)
  (string-append var1 "." var2))

; ..
(define (lua-concat var1 var2)
  (string-append var1 ".." var2))

; ...
(define (lua-variadic)
  "...")

; and
(define (lua-and var1 var2)
  (string-append "(" var1 " and " var2 ")"))

; or
(define (lua-or var1 var2)
  (string-append "(" var1 " or " var2 ")"))

; not
(define (lua-not var)
  (string-append "(not " var ")"))

; local
(define (lua-local var)
  (string-append "local " var))

; break
(define (lua-break)
  "break")

; return
(define (lua-return var)
  (string-append "return " var))

; goto
(define (lua-goto var)
  (string-append "goto " var))

; if
(define (lua-if . case-list)
  (if (null? case-list)
      ""
      (let ((case1 (car case-list))
            (other-case (cdr case-list)))
        (string-append "if "
                       (car case1)
                       " then\n"
                       (car (cdr case1))
                       "\n"
                       (if (null? other-case)
                           ""
                           (let loop ((current-case (car other-case))
                                      (follow-case (cdr other-case)))
                             (let ((head-exp (car current-case))
                                   (tail-exp (cdr current-case)))
                               (if (null? tail-exp)
                                   (string-append "else\n"
                                                  head-exp
                                                  "\nend\n")
                                   (string-append "elseif "
                                                  head-exp
                                                  " then\n"
                                                  (car tail-exp)
                                                  "\n"
                                                  (if (null? follow-case)
                                                      "end"
                                                      (loop (car follow-case)
                                                            (cdr follow-case))))))))))))

; while
(define (lua-while condition body)
  (string-append "while " condition " do\n"
                 body "\nend"))

; repeat
(define (lua-repeat condition body)
  (string-append "repeat\n"
                 body
                 "\nuntil " condition))

; for
(define (lua-for var ctrl-list body)
  (string-append "for " var " = "
                 (list->comma-exp ctrl-list)
                 " do\n"
                 body "\nend"))

; generic for
(define (lua-generic-for var exp body)
  (string-append "for " var " in " exp " do\n"
                 body "\nend"))

; function
(define (lua-function var-list body)
  (string-append "function ("
                 (list->comma-exp var-list)
                 ")\n"
                 body
                 "\nend"))
