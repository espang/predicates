(ns predicates)

(defn sum-f [f g x]
  (+ (f x)
     (g x)))

(defn less-than [n]
  (fn [x] (< x n)))

(defn equal-to [n]
  (fn [x] (== x n)))

(defn set->predicate [a-set]
  (fn [member] (contains? a-set member)))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x)
               (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x)
              (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (let [awards (:awards book)]
    (contains? awards award)))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [book-has-award? (fn [award] (has-award? book award))]
    (every? book-has-award? awards)))

(defn my-some [pred a-seq]
    (let [ret (map pred a-seq)
          ret (first (filter #(if % true false) ret))]
    (when ret ret)))

(defn my-every? [pred a-seq]
  (empty? (filter (complement pred) a-seq)))

(defn prime? [n]
  (let [divide? (fn [divisor n] (= 0 (mod n divisor)))
        pred (fn [x] (divide? x n))]
    (not (some pred (range 2 n)))))
;^^
