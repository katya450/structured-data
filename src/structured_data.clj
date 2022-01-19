(ns structured-data
  (:require [clojure.set :as set]))

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff "sum of 1st and 3rd" [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring "give names to parameters" [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width "[x1 y1] [x2 y2]" [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? "destructure two parameters like this" [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
    (and
     (contains-point? outer [x3 y3])
     (contains-point? outer [x4 y4]))))

;; MAPS

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (count (get book :authors)) 1))

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)]
    (assoc book :authors authors-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

;; SEQUENCES

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsecond (fn [x] (get x 1))]
    (map getsecond collection)))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? "make a set of those and compare length" [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn titles [books]
  (map :title books))

(defn old-book->new-book "assoc overwrites the old authors by keyword" [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn all-authors-to-set [books]
  :-)

(defn author->string [author]
  (let [name (:name author)
        deathyear (:death-year author)
        birthyear (:birth-year author)]
    (cond (contains? author :birth-year)
          (apply str name " (" birthyear " - " deathyear ")")
          :else (apply str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (apply str title ", written by " (authors->string authors))))

(defn books->string [books]
  (if (empty? books) "No books."
      (str (count books) " " (if (= (count books) 1) "book. " "books. ")
           (apply str (interpose ". " (map book->string books))) ".")))
;; or way more complex:
  ;; (let [book-count (count books)
  ;;       books-string (trim (apply str (seq (map (fn [b] (apply str b ". ")) (map book->string books)))))]
  ;;   (cond (= 0 book-count) (str "No books.")
  ;;         (= 1 book-count) (apply str "1 book. " books-string)
  ;;         :else (apply str book-count " books. " books-string)))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))
;OR:   (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (boolean (seq (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))
