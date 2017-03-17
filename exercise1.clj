;; 1.3
(defn mymax [x y] (if (> x y) x y))
; (println (mymax 3 4))

;; 1.11
(defn f [n]
    (cond (< n 3) n
          :else (+ (f (- n 1)) 
                   (* 2 (f (- n 2))) 
                   (* 3 (f (- n 3))))))

(defn f2 [n]
    (defn f-iter [a b c d]
        (if (= d 0) c (f-iter (+ a (* 2 b) (* 3 c)) a b (- d 1))))
    (f-iter 2 1 0 n))

;; 1.12
(defn pascal-triangle [row col]
    (cond (or (= col 0) (= row col)) 1
          :else (+ (pascal-triangle (- row 1) (- col 1))
                   (pascal-triangle (- row 1) col))))

;; 1.16
(defn sqrt [x]
    (* x x))
(defn event? [y]
    (= (rem y 2) 0))
(defn fast-expt-iter [x y z]
    (cond (= y 0) z
          (event? y) (fast-expt-iter (sqrt x) (/ y 2) z)
          :else (fast-expt-iter x (- y 1) (* x z))))
(defn expt [x y]
    (fast-expt-iter x y 1))

;; 1.17
(defn fast-mul [a b]
    (defn doub [x] (+ x x))
    (defn hal [x] (/ x 2))
    (defn event? [x] (= (rem x 2) 0))
    (cond (= b 0) 0
          (event? b) (doub (fast-mul a (hal b)))
          :else (+ a (fast-mul a (- b 1)))))

;; 1.18
(defn fast-mul2 [a b]
    (defn doub [x] (+ x x))
    (defn hal [x] (/ x 2))
    (defn event? [x] (= (rem x 2) 0))
    (defn mul-iter [x y z]
        (cond (= y 0) z
              (event? y) (mul-iter (doub x) (hal y) z)
              :else (mul-iter x (- y 1) (+ x z))))
    (mul-iter a b 0))

;; 1.29
(defn simpson [f a b n]
    (def h (/ (- b a) n))
    (defn event? [x] (= (rem x 2) 0))
    (defn y [k] (f (+ a (* k h))))
    (defn simpson-term [k] 
        (cond (= k 0) (y k)
              (= k n) (y k)
              (event? k) (* 2 (y k))
              :else (* 4 (y k))))
    (* (/ h 3)
       (sum simpson-term 0 incr n)))

;; 1.30
(defn sum-fast [term a next b]
    (defn iter [a result]
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
    (iter a 0))

;; 1.31
(defn product [term a next b]
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b))))

(defn factorial [n]
    (product identity 1 inc n))

(defn pi [n]
    (defn pi-term [x]
        (if (even? x)
            (/ (+ 2 x) (+ 3 x))
            (/ (+ 3 x) (+ 2 x))))
    (* 4.0 (product pi-term 0 inc n)))

(defn product-fast [term a next b]
    (defn iter [a result]
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

;; 1.32
(defn accumulate [combiner null-value term a next b]
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(defn sum2 [term a next b]
    (accumulate + 0 term a next b))
(defn product2 [term a next b]
    (accumulate-fast * 1 term a next b))

(defn accumulate-fast [combiner null-value term a next b]
    (defn iter [a result]
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

;; 1.33
(defn filtered-accumulate2 [filter? combiner null-value term a next b]
    (defn filtered-term [x]
        (if (filter? x)
            (term x)
            null-value))
    (accumulate combiner null-value filtered-term a next b))
; a) b) ??



