(ns euler.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Problem #1
(defn find-mult
  [peak & factors]
  (for [i (range peak)
        :when (some (fn [factor] (zero? (rem i factor))) factors)]
    i))

(find-mult 1000 3 5)

;; Problem #2
(defn even-fibonacci
  [peak]
  (loop [t1 0
         t2 1
         acc 0]
    (if (> t2 peak)
      acc
      (let [t3 (+ t1 t2)]
        (recur t2 t3 (if (even? t3)
                       (+ acc t3)
                       acc))))))

(even-fibonacci 4000000)

;; Problem #3
(defn greatest-prime-factor
  ([value]
   (greatest-prime-factor value (Math/sqrt value) 2))
  ([value max-value factor]
   (if (or
         (> factor max-value)
         (= value factor))
     value
     (if (zero? (rem value factor))
       (recur (/ value factor) max-value factor)
       (recur value max-value (inc factor))))))

(greatest-prime-factor 600851475143)

;; Problem #4
(defn palindrome?
  [string]
  (let [string (str string)
        strlen (count string)
        half-len (/ strlen 2)
        s1     (seq (subs string 0 (Math/ceil half-len)))
        s2     (reverse (subs string (Math/floor half-len) strlen))]
    (= s1 s2)))

(apply max (for [x (range 100 1000)
                 y (range 100 1000)
                 :let [value (* x y)]
                 :when (palindrome? value)]
             value))

;; Problem #5
(defn greatest-common-divisor
  [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

(defn lowest-common-multiple
  [x y]
  (/ (* x y) (greatest-common-divisor x y)))

(defn smallest-multiple
  [limit]
  (reduce (fn [x y] (lowest-common-multiple x y)) (range 1 (inc limit))))

(smallest-multiple 20)

;; Problem #6
(defn sum-square-difference
  [limit]
  (let [numbers (range 1 (inc limit))
        sum-square (apply + (map #(* % %) numbers))
        square-sum (let [num-sum (apply + numbers)]
                     (* num-sum num-sum))]
    (- square-sum sum-square)))

(sum-square-difference 100)

;; Problem #7
(defn is-greater-prime?
  [prime]
  (loop [i 5]
    (if (> (* i i) prime)
      true
      (if (or (zero? (rem prime i)) (zero? (rem prime (+ i 2))))
        false
        (recur (inc i))))))

(defn is-prime?
  [prime]
  (cond
    (<= prime 1) false
    (<= prime 3) true
    (or (zero? (rem prime 2)) (zero? (rem prime 3))) false
    :else (is-greater-prime? prime)))

(defn primes
  [limit]
  (->>
    (filter (fn [prime] (is-prime? prime)) (range))
    (drop (dec limit))
    first))

;; Problem #8
(defn digits
  [number]
  (->> number
    (iterate #(quot % 10))
    (take-while pos?)
    (reduce #(cons (int (rem %2 10)) %1) (list))
    (into [])))

(defn max-adjacency-sum
  [value length]
  (let [value (digits value)]
    (apply max
      (for [index (range (- (count value) length))]
        (reduce * 1 (subvec value index (+ index length)))))))

(max-adjacency-sum 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450 13)

;; Problem #9
