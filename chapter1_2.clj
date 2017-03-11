; (defn factorial [n]
;   (if (= n 1)
;     1
;     (* n (factorial (- n 1)))))
; (defn debug [value] (or (println value) value))

; (defn fib [x]
;     (cond (= x 0) 0
;         (= x 1) 1
;         :else (+ (fib (- x 1)) (fib (- x 2)))))

; (println (fib 6))

; (defn fib2 [x]
;     (defn fib-iter [a b c]
;         (if (= c 0) b (fib-iter (+ a b) a (- c 1))))
;     (fib-iter 1 0 x)
;     )

; (println (fib2 6))


; (defn first-denomination [kinds-of-coins]
;     (cond (= kinds-of-coins 1) 1
;           (= kinds-of-coins 2) 5
;           (= kinds-of-coins 3) 10
;           (= kinds-of-coins 4) 20
;           (= kinds-of-coins 5) 50))

; (defn cc [amount kinds-of-coins]
;     (cond (= amount 0) 1
;           (or (< amount 0) (= kinds-of-coins 0)) 0
;           :else (+ (cc amount 
;                        (- kinds-of-coins 1))
;                    (cc (- amount (first-denomination kinds-of-coins))
;                        kinds-of-coins))))

; (defn count-change [amount] 
;     (cc amount 5))

(defn expt [b n]
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))
(defn expt2 [b n]
    (defn expt-iter [x y sum]
        (if (= y 0)
            sum
            (expt-iter x (- y 1) (* x sum))))
    (expt-iter b n 1))
(defn expt-fast [x y]
    (defn event? [n]
        (= (rem n 2) 0))
    (cond (= y 0) 1
          (event? y) (* (expt-fast x (/ y 2)) (expt-fast x (/ y 2)))
          :else (* x (expt-fast x (- y 1)))))

(println (expt-fast 2 3))




























