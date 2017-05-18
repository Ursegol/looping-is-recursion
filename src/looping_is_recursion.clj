(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [v r]
                 (if (empty? r)
                   v
                   (recur (first r) (rest r))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc a b]
                 (if (or (not acc)
                         (and (empty? a)
                              (empty? b)))
                   acc
                   (recur (= (first a) (first b)) (rest a) (rest b))))]
    (helper (= (count seq1)
               (count seq2))
            seq1
            seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (if (empty? seq)
      nil
      (if (pred (first seq))
        index
        (recur (inc index) (rest seq))))))

(defn avg [a-seq]
  (let [helper (fn [sum count seq]
                 (if (empty? seq)
                   (/ sum count)
                   (recur (+ sum (first seq))
                          (inc count)
                          (rest seq))))]
    (helper 0 0 a-seq)))

(defn parity [a-seq]
  (loop [s #{}
         seq a-seq]
    (if (empty? seq)
      s
      (if (contains? s (first seq))
        (recur (disj s (first seq)) (rest seq))
        (recur (conj s (first seq)) (rest seq))))))

(defn fast-fibo [n]
  (loop [res 0
         add 1
         pos 0
         till n]
    (if (= pos till)
      res
      (recur (+ res add) res (inc pos) till))))

(defn cut-at-repetition [a-seq]
    (loop [myset []
           seq a-seq]
      (if (or (empty? seq)
              (contains? (set myset) (first seq)))
        myset
        (recur (conj myset (first seq)) (rest seq)))))
