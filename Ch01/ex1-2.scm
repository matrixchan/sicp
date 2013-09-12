#!
;Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))
)
!#
 

;Exercise 1.3
(define (max x1 x2)
  (if (> x1 x2) x1 x2)
)

(define (min x1 x2)
  (if (< x1 x2) x1 x2)
)

(define (sum-max2 v1 v2 v3)
  (define max2 (max v1 v2))
  (define min2 (min v1 v2))
  (cond ((> min2 v3) (+ v1 v2))
	((< min2 v3) (+ max2 v3))
	((= min2 v3) (+ max2 v3))
	(else -1))
)

;Exercise 1.4
#!
如果b大于0,側 a+b;
否则,a-b；
其实就是求a+|b|
!#
(define (a-plus-abs-b a b)
  ((if (> b 0) + - ) a b)
)

;Exercise 1.5
!#
正则序:先展开到只剩下基本过程,再对其中"需要求值"的部分求值
应用序:先把参数求值,再把值带入函数体里面.
-------------------------------------------------
如果在正则序中,函数会展开成如下:
(if (= x 0) 0 (p)) 正常返回0.
如果在应用序中,函数在(test 0 (p))出对(p)求值会进入到死循环中,
程序会提示出错.

!#
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

;调用:(test 0 (p))


;1.1.7牛顿法计算平方根
;求绝对值
(define (abs x)
  (if (< x 0)
       (- x)
       x)
)

;求平方
(define (square x) (* x x))

;求平均值
( (average x y)
  (/ (+ x y) 2)
)


(define (improve guess x)
  (average guess (/ x guess))
)

(define (good-enough? guess x)
  (< (abs (-(square guess) x)) 0.001 )
)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (squrt-iter (improve guess x)
		 x))
)
;启动
(define (sqrt x)
  (sqrt-iter 1.0 x)
)

;练习1.6 
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause))
)

#!
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x))
)!#


;练习1.8 使用牛顿法求立方根
;求立方
(define (cube x)
  (* x x x)
)
(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)


(define (cube-good-engouh? guess x)
  (< (abs (- (cube guess) x)) 0.001 )
)


(define (cube-iter guess x)
  (if (cube-good-engouh? guess x)
      guess
      (cube-iter (cube-improve guess x)
		 x))
)

(define (cube-roots x)
  (cube-iter 1.0 x)
)









;-----------------------------------------
;1.2.2 树形递归
;实例:换零钱方式的统计
(define (count-change amount)
  (cc amount 5)
)

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0 )
	(else (+ (cc amount (- kinds-of-coins 1))
		 (cc (- amount (first-denomination kinds-of-coins) )  kinds-of-coins)))
	)
)

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50))
)

;;-------------------------------------------------------
;; 1.2.4 求幂
;; 线性递归
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1))))
  )


;;线性迭代
(define (expt2 b n)
  (expt-iter b n 1)
  )

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
;;---------------------------------
;; 方法三,连续求平方
(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0 ) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )

(define (even? n)
  (= (remainder n 2) 0))





;;-------------------------------------------------------
;练习1.10 Ackermann函数
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		  (A x (- y 1))))
	 )
  )

;练习1.11 n<3,f(n)=;n>=3,f(n)=f(n-1)+2f(n-2)+3f(n-3)
;递归方法:
(define (f n)
  (if (< n 3)
      n
      (+ 
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3)))
       )
   )

)

;迭代法:
(define (f2 n) 
  (f-iter 2 1 0 n)
)
(define (f-iter a b c count)
  (if (= count 0) c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))
       )
)

;1.12 计算帕斯卡
(define (combination x y)
  (cond((= x y) 1)
       ((= y 0) 1)
       (else (+ (combination (- x 1) (- y 1))
		(combination (- x 1) (y))
		)
	     )
       )
   )


;;练习1.16

