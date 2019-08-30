(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
   (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2))) ; sum of 1st and 3rd

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
       (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;[x1 y1] [x2 y2]
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1))) 

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1) (- y2 y1)))) ; erotus pitää olla sama

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point] ;destructure two parameters like this
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
    (let [[[x3 y3] [x4 y4]] inner]
      (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4])))) 
      
(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
    (> (count (get book :authors)) 1 ))

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)]
    (assoc book :authors authors-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsecond (fn [x] (get x 1))]
  (map getsecond collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond 
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

; make a set of those and compare length 
(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq)))) 

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
       (assoc book :authors author-set)))

; check if author is in the author set
(defn has-author? [book author]
  (contains? book author))
  ;(print author))
  

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn all-authors-to-set [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)



; %________%
