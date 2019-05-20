(ns book-recommender.core
  (:gen-class))

(def ratings-raw
  (->> (slurp "resources/BX-Book-Ratings.csv")
       (clojure.string/split-lines)
       (rest)
       (map #(.split % ";"))
       (filter #(not (= (last %) "0")))
       (map #(hash-map :id (first %) :ratings (hash-map (keyword (second %)) (Double/parseDouble(last %)))))       ))

(def books-raw
  (->> (slurp "resources/BX-Books.csv")
       (clojure.string/split-lines)
       (rest)
       (map #(.split % ";"))
       (map #(hash-map :isbn (first %) :title (second %) :publisher (nth % 4)))))

(defn- find-by-title [title]
  (->> (first (filter #(.equalsIgnoreCase (:title %) title) books-raw))
       (:isbn)
       (keyword)))

(defn- find-by-isbn [isbn]
  (->> (first (filter #(.equalsIgnoreCase (:isbn %) (.substring (str isbn) 1)) books-raw))
       (:title)))

(def ratings
  (->> ratings-raw (sort-by :id)
       (partition-by :id)
       (map (partial
              apply
              merge-with (fn [x y] (if (= x y) x (into x y)))))))

(defn- form-pairs [coll temp]
  (map #(list % temp) coll))

(defn- compare-maps [x y]
  (let [x-ratings (:ratings x) y-ratings (:ratings y)]
    (keep #(some->> % (find x-ratings) key) (keys y-ratings))))

(defn- calculate-pearson-score [pair]
  (let [x (first pair)
        y (second pair)
        sim (compare-maps x y)
        x-rat-val (map (into (sorted-map) (:ratings x)) sim)
        y-rat-val (map (into (sorted-map) (:ratings x)) sim)
        n (count sim)]
    (if (= n 0)
      {:id (if (map? x) (:id x) (val x)) :score 0}
      (let [sum1 (reduce + x-rat-val)
            sum2 (reduce + y-rat-val)
            sum1Sq (reduce + (map #(* % %) x-rat-val))
            sum2Sq (reduce + (map #(* % %) y-rat-val))
            num (- (reduce + (map * x-rat-val y-rat-val)) (/ (* sum1 sum2) n))
            den (Math/sqrt (* (- sum1Sq (/ (* sum1 sum1) n)) (- sum2Sq (/ (* sum2 sum2) n))))]
        {:id (if (map? x) (:id x) (val x)) :score (if (= den 0) 0 (/ num den))}))))

(defn- scores [n test-rating]
  (->> (form-pairs ratings test-rating)
       (map #(calculate-pearson-score %))
       (sort-by :score)
       (reverse)
       (take n)))

(defn- get-user-ratings [key]
  (keys (apply :ratings (filter #(= (:id %) key) ratings))))


(defn- get-recommendations-for-user [user]
  (->> (map #(get-user-ratings %) (map :id (scores 3 user)))
       (flatten)
       (distinct)
       (filter #(nil? (% (:ratings user))))
       (map #(find-by-isbn %))
       (filter #(not (nil? %)))
       (take 10)))

(defn- parse-args [args]
  {:id "user"
   :ratings (apply merge (map #(hash-map (keyword (find-by-title (first %))) (second %)) (partition 2 2 args)))})


(defn get-recommendations [& args]
  (get-recommendations-for-user (parse-args args)))

(defn -main [& args]
  (doseq [[i itm] (map-indexed vector (get-recommendations args))]
    (println (str (inc i) ". " itm))))

