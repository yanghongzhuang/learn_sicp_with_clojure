;; (def size 2)
;; (pr (* 5 size))

;; (def pi 3.14159)
;; (def radius 10)
;; (println (* pi (* radius radius)))
;; (def circumference (* 2 pi radius))
;; (println circumference)

; (println 
; (* (+ 2 (* 4 6))
;    (+ 3 5 7)))

(defn square [x] (* x x))
; (println (square 21))

; (defn abs [x] 
;     (cond (> x 0) x
;           (= x 0) 0
;           (< x 0) (- x)
;     ))

; (defn abs [x] 
;     (cond (< x 0) (- x)
;           :else x ))

(defn abs [x]
    (if (< x 0) (- x) x))

; (println (abs 12) (abs 0) (abs -5))

; (defn tttt [x] (or (> x 5) (< x 10)))
; (println (tttt 1) (tttt 5) (tttt 9) (tttt 111))

;; 1.1.7 
(defn debug [value] (or (println value) value))

(defn new-if [predicate then-clause else-clause]
      (cond predicate then-clause
            :else else-clause))

(defn average [x y] 
      (/ (+ x y) 2))

(defn improve [guess x]
      (average guess (/ x guess)))

(defn good-enough? [guess x]
      (< (abs (- (square guess) x)) 0.0001))

(defn sqrt-iter [guess x]
      (if (good-enough? (debug guess) x) 
          guess
          (sqrt-iter (improve guess x)
                     x)))

(defn sqrt [x]
      (sqrt-iter 1.0 x))

(println (sqrt 999999999))







