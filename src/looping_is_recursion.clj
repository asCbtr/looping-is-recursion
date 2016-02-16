(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last seq1]
                 (if (empty? seq1)
                   last
                   (recur (first seq1) (rest seq1))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [b seqq1 seqq2]
                 (cond
                   (and (empty? seqq1) (empty? seqq2)) b
                   (and (not (empty? seqq1)) (empty? seqq2)) false
                   (and (empty? seqq1) (not (empty? seqq2))) false
                   :else (recur (= (first seqq1) (first seqq2)) (rest seqq1) (rest seqq2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         predicate pred
         seq1 a-seq]
    (cond
     (empty? seq1) nil
     (predicate (first seq1)) acc
     :else (recur (+ 1 acc) predicate (rest seq1)))))

(defn avg [a-seq]
  (loop [sum 0
         ind 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ sum ind)
      (recur (+ sum (first seq1)) (+ 1 ind) (rest seq1)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
               (if (contains? a-set elem)
                 (disj a-set elem)
                 (conj a-set elem)))]
    (loop [seq1 a-seq
           result #{}]
      (if (empty? seq1)
        result
        (recur (rest seq1) (toggle result (first seq1)))))))

(defn fast-fibo [n]
  (loop [m n
         fibo-first 0
         fibo-second 1]
    (if (= 0 m)
      fibo-first
      (recur (dec m) fibo-second (+ fibo-first fibo-second)))))

(defn cut-at-repetition [a-seq]
  (let [helper (fn [a-seq elem]
                 (cond
                   (empty? a-seq) false
                   (= (first a-seq) elem) true
                   :else (recur (rest a-seq) elem)))]
    (loop [result '()
           seq1 a-seq]
      (cond
        (empty? seq1) (reverse result)
        (helper result (first seq1)) (reverse result)
        :else (recur (cons (first seq1) result) (rest seq1))))))
