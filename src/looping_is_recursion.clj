(ns looping-is-recursion)

(defn power-helper [base base-orig exp]
  (cond
   (= 1 exp) base
   (>= 0 exp) 1
   :else (power-helper (* base base-orig) base-orig (dec exp))))

(defn power [base exp]
  (power-helper base base exp))

(defn last-element [a-seq]
  (if (>= 1 (count a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  ;(println seq1 seq2 (= (first seq1) (first seq2)))
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or
    (and (= nil first seq1) (not= nil (first seq2)))
    (and (not= nil first seq1) (= nil (first seq2)))) false
   (not (= (first seq1) (first seq2))) false
   :else (seq= (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  ; (find-first-index zero? [1 1 1 0 3 7 0 2]) ;=> 3
  (loop [pred pred
         a-seq a-seq
         index 0]
    (cond
     (empty? a-seq) nil
     (= true (pred (first a-seq))) index
     :else (recur pred (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         sum 0
         n 0]
    (cond
     (and (empty? a-seq) (= 0 n)) 0
     (empty? a-seq) (/ sum n)
     :else (recur (rest a-seq) (+ (first a-seq) sum) (inc n)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn parity [a-seq]
  ; (parity [:a :b :c :a]) ;=> #{:b :c}
  (loop [a-seq a-seq
         a-set #{}]
    (cond
     (empty? a-seq) a-set
     :else (recur (rest a-seq) (toggle a-set (first a-seq))))))

(defn fast-fibo [n]
  (loop [n n
         n1 0
         n2 1]
    (cond
     (= n 0) n1
     :else (recur (dec n) n2 (+ n1 n2)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         b-vec '[]]
    (cond
     (empty? a-seq) b-vec
     (contains? (set b-vec) (first a-seq)) b-vec
     :else (recur (rest a-seq) (conj b-vec (first a-seq))))))

