(defn cube [x] (* x x x))

(defn sum-integers [a b]
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))

(defn sum-cubes [a b]
    (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))))

(defn pi-sum [a b]
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defn sum [term a next b]
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(defn incr [n] (+ n 1))
(defn sum-cubes2 [a b]
    (sum cube a incr b))

; (defn identity1 [x] (+ (- x 1) 1))
(defn sum-integers2 [a b]
    (sum identity a incr b))

(defn pi-sum2 [a b]
    (defn pi-term [x] (/ 1.0 (* x (+ x 2))))
    (defn pi-next [x] (+ x 4))
    (sum pi-term a pi-next b))

(defn integral [f a b dx]
    (defn add-dx [x] (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))
(println (integral cube 0 1 0.001))

(defn pi-sum [a b]
    (sum (fn [x] (/ 1.0 (* x (+ x 2))))
         a
         (fn [x] (+ x 4))
         b))
; (println (* 8 (pi-sum 1 1000)))

; (println (sum (fn [x] x) 
;               0 
;               #(+ % 1) 
;               10))

(defn integral [f a b dx]
    (* (sum f
            (+ a (/ dx 2.0))
            (fn [x] (+ x dx))
            b)
       dx))
; (println (integral #(* % % %) 0 1 0.01))
; (println (integral #(* % % %) 0 1 0.001))

(defn square [x] (* x x))
(println ((fn [x y z] (+ x y (square z))) 1 2 3))

;; let
(defn f1 [x y]
    (defn f-helper [a b]
        (+ (* x (square a)) 
           (* y b) 
           (* a b)))
    (f-helper (+ 1 (* x y)) (- 1 y)))

(defn f2 [x y]
    ((fn [a b]
        (+ (* x (square a))
           (* y b)
           (* a b)))
     (+ 1 (* x y))
     (- 1 y)))

(defn f3 [x y]
    (let [a (+ 1 (* x y))
          b (- 1 y)]
        (+ (* x (square a))
           (* y b)
           (* a b))))

;; ============================================================
;; 1.3.3 Procedures as General Method
(defn abs [x]
    (cond (< x 0) (- x)
          :else x))
(defn close-enough? [x y]
    (< (abs (- x y)) 0.001))
(defn average [x y] (/ (+ x y) 2))
(defn positive? [x] (or (> x 0) (= x 0)))
(defn negative? [x] (< x 0))

(defn search [f neg-point pos-point]
    (let [midpoint (average neg-point pos-point)]
        (if (close-enough? neg-point midpoint)
            midpoint
            (let [test-value (f midpoint)]
                (cond (positive? test-value) (search f neg-point midpoint)
                      (negative? test-value) (search f midpoint pos-point)
                      :else midpoint)))))

(defn half-interval-method [f a b]
    (let [[a-value b-value] [(f a) (f b)]]
        (cond (and (negative? a-value) (positive? b-value))
                    (search f a b)
              (and (negative? b-value) (positive? a-value))
                    (search f b a)
              :else (println "Values are not of opposite sign" a b))))

(defn search [f neg-point pos-point]
    (let [midpoint (average neg-point pos-point)]
        (if (close-enough? neg-point midpoint)
            midpoint
            (let [test-value (f midpoint)]
                (cond (positive? test-value) (search f neg-point midpoint)
                      (negative? test-value) (search f midpoint pos-point)
                      :else midpoint)))))

(defn half-interval-method [f a b]
    (let [[a-value b-value] [(f a) (f b)]]
        (cond (and (negative? a-value) (positive? b-value))
                    (search f a b)
              (and (negative? b-value) (positive? a-value))
                    (search f b a)
              :else (print "Values are not of opposite sign" a b))))

(def tolerance 0.00001)
(defn fixed-point [f first-guess]
    (defn close-enough? [v1 v2]
        (< (abs (- v1 v2)) tolerance))
    (defn tryit [guess]
        (let [nextt (f guess)]
            (if (close-enough? guess nextt)
                nextt
                (tryit nextt))))
    (tryit first-guess))

;; 1.3.4
(defn average-damp [f]
    (fn [x]
        (average x (f x))))
(defn sqrt2 [x]
    (fixed-point (average-damp (fn [y] (/ x y)))
                 1.0))

